(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Format

(* TODO: add section descriptions *)

let default_section_id = "default"
let default_section_title = "Miscellaneous"

(* Association list where keys are set of identifier's prefixes that
   maps to a section title. The ordering of sections in the rst output
   depends on their position in this list.

   e.g. : an error which id is 'utils.Timeout' will be documented
   under the `Miscellaneous` section which will be displayed at the
   bottom of the document. Unprefixed ids or unreferenced prefixes
   will default to `Miscellaneous` *)
let section_titles =
  [ [ "proto.alpha" ], "Protocol Alpha";
    [ "distributed_db" ; "node" ; "raw_store" ; "validator" ; "worker" ], "Shell" ;
    [ "micheline" ; "michelson" ], "Michelson parsing/macros" ;
    [ "rpc_client" ], "Client" ;
    [ "cli"; "utils"; default_section_id ], default_section_title ;
  ]
let pp_rst_title ~char ppf title =
  let sub = String.map (fun _ -> char) title in
  fprintf ppf "@[<v 0>%s@\n@]@[<v 0>%s@\n@\n@]" title sub

let pp_rst_h1 = pp_rst_title ~char:'#'
let pp_rst_h2 = pp_rst_title ~char:'*'
(* let pp_rst_h3 = pp_rst_title ~char:'='
 * let pp_rst_h4 = pp_rst_title ~char:'`' *)

let string_of_err_category = function
  | `Branch -> "branch"
  | `Temporary -> "temporary"
  | `Permanent -> "permanent"

let make_counter () =
  let i = ref 1 in
  fun () -> incr i; !i

let count = make_counter ()

let unique_label () =
  let label = sprintf "ref%d" (count ()) in
  label

let pp_print_html_tab_button fmt ?(default=false) ~shortlabel ~content idref =
  fprintf fmt "<button class=\"tablinks%s\" onclick=\"showTab(this, '%s', '%s')\">%s</button>@ "
    (if default then " defaultOpen" else "")
    (idref ^ shortlabel) idref content

let pp_print_html_tabs fmt { Error_monad.id ; category ; description ; schema ; _  } =
  let idref = unique_label () in
  let descr_label = "descr" in
  let schema_label = "schema" in

  fprintf fmt "@[<v 2>.. raw:: html@ @ ";
  fprintf fmt "@[<v 2><div class=\"tab\">@ ";

  fprintf fmt "%a" (pp_print_html_tab_button ~default:true ~shortlabel:descr_label ~content:"Description") idref;
  fprintf fmt "%a" (pp_print_html_tab_button ~default:false ~shortlabel:schema_label ~content:"JSON Schema") idref;
  fprintf fmt "@ </div>@ @]";

  let description_content =
    asprintf "<p>%s</p><p><i>Id</i> : %s<br/><i>Category</i> : %s</p>" description id (string_of_err_category category)
  in

  open_vbox 2;

  (* Print description *)
  begin
    fprintf fmt "<div id=\"%s\" class=\"%s tabcontent\" style=\"min-height:100px; max-height:200px; overflow:auto\" >@ "
      (idref ^ descr_label) idref;
    fprintf fmt "%s@ " description_content;
    fprintf fmt "</div>@]";
  end;

  (* Print schema *)
  begin
    (* Hack: negative offset in order to reduce the <pre>'s content left-margin *)
    (* TODO: pretty-(html)-print the schema *)
    open_vbox (-8);
    fprintf fmt "<div id=\"%s\" class=\"%s tabcontent\" style=\"min-height:100px; max-height:200px; overflow:auto\" >@ "
      (idref ^ schema_label) idref;
    fprintf fmt "<%s>@ %a</%s>@ " "pre" Json_schema.pp schema "pre";
    fprintf fmt "</div>";
    close_box ();
  end;

  close_box ()

let pp_info_to_rst
    ppf
    (Error_monad.{ title ; _ } as error_info) =
  let open Format in

  fprintf ppf "**%s**@\n@\n" (if title = "" then "<Untitled>" else title);
  fprintf ppf "@[<v>%a@ @ @]" pp_print_html_tabs error_info;

module ErrorSet = Set.Make(struct
    type t = Error_monad.error_info
    let compare { Error_monad.id ; _ } { Error_monad.id = id' ; _ } =
      String.compare id id'
  end)

module ErrorPartition = struct
  include Map.Make(struct
      include String
      let titles = List.map snd section_titles

      let compare s s' =
        let idx s =
          let rec loop acc = function
            | [] -> assert false
            | h::_ when h = s -> acc
            | _::t -> loop (acc + 1) t
          in loop 0 titles
        in
        Pervasives.compare (idx s) (idx s')
    end)

  let add_error (id : key) (error : Error_monad.error_info) (map : 'a t) =
    let title =
      try
        snd
          (List.find
             (fun (id_set, _) ->
                List.exists (fun pattern -> Stringext.find_from id ~pattern = Some 0) id_set)
             section_titles)
      with
      | Not_found -> default_section_title
    in
    let set =
      try find title map with Not_found -> ErrorSet.empty
    in
    add title (ErrorSet.add error set) map
end

let pp_error_map ppf (map : ErrorSet.t ErrorPartition.t) : unit =
  let open Format in
  ErrorPartition.iter (fun section_title set ->
      fprintf ppf "%a" pp_rst_h2 section_title ;

      ErrorSet.iter
        (fun error_repr ->
           fprintf ppf "@[%a@]@\n@\n" pp_info_to_rst error_repr
        ) set
    ) map

let script =
  "<script>\
   function showTab(elt, tab, ref) {\
   var i, tabcontent, tablinks;\
   \
   tabcontent = document.getElementsByClassName(ref);\
   for (i = 0; i < tabcontent.length; i++) {\
   tabcontent[i].style.display = 'none';\
   }\
   \
   tablinks = elt.parentNode.children;\
   for (i = 0; i < tablinks.length; i++) {\
   tablinks[i].className = tablinks[i].className.replace(' active', '');\
   }\
   \
   document.getElementById(tab).style.display = 'block';\
   elt.className += ' active';\
   }\
   \
   document.addEventListener('DOMContentLoaded', function(){\
   var a = document.getElementsByClassName('defaultOpen');\
   for (i = 0; i < a.length; i++) { a[i].click() }\
   })\
   </script>"

let style =
  "<style>\
   .tab {\
   overflow: hidden;\
   border: 1px solid #ccc;\
   background-color: #f1f1f1;\
   }\
   .tab button {\
   background-color: inherit;\
   float: left;\
   border: none;\
   outline: none;\
   cursor: pointer;\
   padding: 5px 10px;\
   }\
   .tab button:hover {\
   background-color: #ddd;\
   }\
   .tab button.active {\
   background-color: #ccc;\
   }\
   .tabcontent {\
   display: none;\
   padding: 6px 12px;\
   border: 1px solid #ccc;\
   border-top: none;\
   margin-bottom: 20px;\
   }\
   pre {\
   font-size: 12px\
   }</style>"

let print_script ppf =
  (* HACK : show/hide JSON schemas + style *)
  fprintf ppf "@[<v 2>.. raw:: html@\n@\n" ;
  fprintf ppf "@[<v 0>%s%s@]@\n@\n@]@]@." script style

(* Main *)
let () =
  let open Format in
  let ppf = std_formatter in

  (* Header *)
  let title = "RPC Errors" in
  fprintf ppf "%a" pp_rst_h1 title ;

  print_script ppf ;

  fprintf ppf
    "This document references possible errors that can come \
     from RPC calls. It is generated from the OCaml source \
     code (master branch).@\n@\n" ;

  (* Body *)
  let map =
    let all_errors =
      Error_monad.get_registered_errors () in
    List.fold_left
      (fun acc ( Error_monad.{ id ; _ } as error ) ->
         ErrorPartition.add_error id error acc
      ) ErrorPartition.empty all_errors
  in

  fprintf ppf "%a" pp_error_map map
