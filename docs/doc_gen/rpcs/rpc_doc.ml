(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type service_repr =
  { path : string list ;
    meth : Resto.meth ;
    description : string ;
    input : Json_schema.schema option ;
    output : Json_schema.schema option ;
    example : string option ;
    error : Json_schema.schema option
  }

type service_tree =
  | Root of service_tree list
  | Node of service_repr * service_tree list
  | SymbNode of string list * service_tree list

let make_descr = function
  | None | Some "" -> "No description"
  | Some s -> s

(** Inspect a JSON schema: if it doesn't contain any field (e.g. { }),
    return None *)
let normalize_json_schema = function
  | None -> None
  | Some schema ->
      let open Json_schema in
      let elt = root schema in
      match elt.kind with
      | Object specs when
          specs = object_specs ||
          specs = { object_specs with additional_properties = None } ->
          None
      | _ -> Some schema

let repr_of_service path
    RPC_description.{ description ; error ;
                      meth ; input ; output ; _ }  : service_repr =
  { path ; meth ;
    description = make_descr description ;
    input = normalize_json_schema input ;
    output = normalize_json_schema (Some output) ;
    example = None ; error = Some error  }

open Format

let pp_print_service fmt
    { path ; meth }
  =
  fprintf fmt "%s %s" (Resto.string_of_meth meth) (String.concat "/" path)

let rec pp_print_service_tree fmt = function
  | Root l ->
      fprintf fmt "@[<v 2>/";
      List.iter
        (fun tree ->
           fprintf fmt "@ ";
           fprintf fmt "%a" pp_print_service_tree tree
        ) l;
      fprintf fmt "@]"
  | Node (repr, l) ->
      fprintf fmt "@[<v 2>%a" pp_print_service repr;
      List.iter
        (fun tree ->
           fprintf fmt "@ ";
           fprintf fmt "%a" pp_print_service_tree tree
        ) l;
      fprintf fmt "@]"
  | SymbNode (sl, l) ->
      fprintf fmt "@[<v 2>%s" (String.concat "/" sl);
      List.iter
        (fun tree ->
           fprintf fmt "@ ";
           fprintf fmt "%a" pp_print_service_tree tree
        ) l;
      fprintf fmt "@]"

let make_tree cctxt path =
  (* TODO : add automatic example generation *)
  let open RPC_description in
  describe cctxt ~recurse:true path >>=? fun dir ->
  let rec loop path : _ directory -> service_tree list = function
    | Dynamic descr ->
        [ Node ({ path ; meth=`POST ;
                  description=make_descr descr ;
                  input = None ; output = None ;
                  example = None ; error = None }, []) ]

    | Empty -> []

    | Static { services ; subdirs = None } ->
        let l = RPC_service.MethMap.bindings services in
        let l = List.map snd l in
        List.map
          (fun service -> Node (repr_of_service path service, []))
          l

    | Static { services ; subdirs = Some (Suffixes subdirs) } ->
        let subdirs =  Resto.StringMap.bindings subdirs in

        let l = List.map (fun (name, subdir) ->
            loop (path @ [ name ]) subdir
          ) subdirs |> List.concat
        in

        let services = RPC_service.MethMap.bindings services in
        let services = List.map snd services in

        begin
          match services with
          | [] -> [ SymbNode (path, l) ]
          | service::[] -> [ Node (repr_of_service path service, l) ]
          | _ -> assert false (* ? *)
        end

    | Static { services ; subdirs = Some (Arg (arg, solo)) } ->
        let name = Printf.sprintf "<%s>" arg.RPC_arg.name in

        let services = RPC_service.MethMap.bindings services in
        let services = List.map snd services in

        let l = loop (path @ [ name ]) solo in

        begin
          match services with
          | [] -> [ SymbNode (path, l) ]
          | service::[] -> [ Node (repr_of_service path service, l) ]
          | _ -> assert false (* ? *)
        end
  in
  return (Root (loop path dir))

let rec pp_print_hierarchy fmt =
  let open Format in
  function
  | Root l ->
      List.iter
        (fun tree ->
           fprintf fmt "@ ";
           fprintf fmt "%a" pp_print_hierarchy tree
        ) l;
      fprintf fmt "@]"

  | SymbNode (path, l)
  | Node ( { path } , l) ->
      if List.length path = 0 then begin
        List.iter
          (fun tree ->
             fprintf fmt "@ ";
             fprintf fmt "%a" pp_print_hierarchy tree
          ) l;
        fprintf fmt "@]"
      end
      else
        begin
          let name = "/" ^ List.hd (List.rev path) in
          let offset = max 2 (String.length name / 2) in

          if List.length l = 0 then
            pp_open_vbox fmt 0
          else
            pp_open_vbox fmt offset;

          fprintf fmt "%s" name;
          List.iter
            (fun tree ->
               fprintf fmt "@ ";
               fprintf fmt "%a" pp_print_hierarchy tree)
            l;
          pp_close_box fmt ()
        end

(**************** RST PRINTING ****************)

let pp_print_rst_title ~char ppf title =
  let sub = String.map (fun _ -> char) title in
  Format.fprintf ppf "@[<v 0>%s@ %s@ @ @]" title sub

let pp_print_rst_h1 = pp_print_rst_title ~char:'#'
let pp_print_rst_h2 = pp_print_rst_title ~char:'*'
let pp_print_rst_h3 = pp_print_rst_title ~char:'='
let pp_print_rst_h4 = pp_print_rst_title ~char:'`'

let pp_print_rst_raw_html fmt str =
  (* let ic = open_in file in *)
  fprintf fmt "@[<v 2>.. raw:: html@ @ %s@ @ @]" str

let label_table = Hashtbl.create 17

let make_counter () =
  let i = ref 1 in
  fun () -> incr i; !i

let count = make_counter ()

let rst_label_of_path path =
  let label = Printf.sprintf "ref%d" (count ()) in
  Hashtbl.add label_table path label;
  "<" ^ label ^ "_>"

let ref_of_path path =
  Hashtbl.find label_table path

let rec pp_print_rst_hierarchy fmt ~title node =
  fprintf fmt "%a@ " pp_print_rst_h2 title;
  let rst_name =
    String.lowercase_ascii title
    |> String.map (function ' ' -> '-' | x -> x)
  in
  let rst_name =
    let rec loop str =
      let open Stringext in
      if find_from str ~pattern:"--" <> None then
        loop (replace_all_assoc str [("--","-")])
      else
        str
    in loop rst_name
  in

  fprintf fmt "%a@." pp_print_rst_raw_html
    (sprintf "<style>#%s * { margin-bottom:0px }\
              #%s > *:last-child { margin-bottom:15px }\
              #%s > h2 { margin-bottom:15px}</style>" rst_name rst_name rst_name);

  let rec loop fmt tree =
    match tree with
    | Root l ->
        (* fprintf fmt "@[<v 4>/"; *)
        fprintf fmt "@[<v 0>";
        List.iter
          (fun tree ->
             fprintf fmt "@ ";
             fprintf fmt "%a" loop tree
          ) l;
        fprintf fmt "@]"
    | Node ( { path }, l) ->
        let name = "/" ^ String.concat "/" path in
        fprintf fmt "@[<v 4>";
        fprintf fmt "`%s %s`_"  name (rst_label_of_path path);
        fprintf fmt "@ ";

        List.iter
          (fun tree ->
             fprintf fmt "@ ";
             fprintf fmt "%a" loop tree
          ) l;
        fprintf fmt "@]"
    | SymbNode (path, l) ->
        if List.length path > 0 then begin
          let name = "\\/" ^ String.concat "/" path in

          fprintf fmt "@[<v 2>%s" name;
          (* fprintf fmt "%s" name; *)

          fprintf fmt "@ ";

          List.iter
            (fun tree ->
               fprintf fmt "@ ";
               fprintf fmt "%a" loop tree
            ) l;
          fprintf fmt "@]"

        end else
          List.iter
            (fun tree ->
               fprintf fmt "@ ";
               fprintf fmt "%a" loop tree
            ) l
  in
  loop fmt node

let pp_print_html_tab_button fmt ?(default=false) ~shortlabel ~content path =
  let target_ref = ref_of_path path in
  fprintf fmt "<button class=\"tablinks%s\" onclick=\"showTab(this, '%s', '%s')\">%s</button>@ "
    (if default then " defaultOpen" else "")
    (target_ref ^ shortlabel)
    target_ref
    content

let pp_print_html_tab_content fmt ~tag ~shortlabel ~pp_content ~content path =
  let target_ref = ref_of_path path in
  fprintf fmt "@[<v 2><div id=\"%s\" class=\"%s tabcontent\" style=\"max-height:200px; overflow:auto\" >@ "
    (target_ref ^ shortlabel) target_ref;
  fprintf fmt "<%s>@ %a</%s>@ " tag pp_content content tag;
  fprintf fmt "</div>@]"

let pp_print_html_tabs fmt { path ; description ; input ; output ; _ (* example ; error *) } =
  fprintf fmt "@[<v 2>.. raw:: html@ @ ";
  fprintf fmt "@[<v 2><div class=\"tab\">@ ";

  fprintf fmt "%a" (pp_print_html_tab_button ~default:true ~shortlabel:"descr" ~content:"Description") path;
  (match input with
   | Some _ ->
       fprintf fmt "%a" (pp_print_html_tab_button ~default:false ~shortlabel:"input" ~content:"Input format") path;
   | None -> ());
  fprintf fmt "%a" (pp_print_html_tab_button ~default:false ~shortlabel:"output" ~content:"Output format") path;
  (* (match example with
   *  | Some _ ->
   *      fprintf fmt "%a" (pp_print_html_tab_button ~default:false ~shortlabel:"example" ~content:"Example") path;
   *  | None -> ()); *)
  fprintf fmt "</div>@]@ ";

  fprintf fmt "%a@ " (pp_print_html_tab_content ~tag:"p" ~shortlabel:"descr"
                        ~pp_content:pp_print_string ~content:description) path;
  (match input with
   | Some x -> fprintf fmt "%a@ " (pp_print_html_tab_content ~tag:"pre" ~shortlabel:"input"
                                     ~pp_content:Json_schema.pp ~content:x) path;
   | None -> ());
  (match output with
   | Some x -> fprintf fmt "%a@ " (pp_print_html_tab_content ~tag:"pre" ~shortlabel:"output"
                                     ~pp_content:Json_schema.pp ~content:x) path;
   | None -> ());
  (* (match example with
   *  | Some x -> fprintf fmt "%a@ " (pp_print_html_tab_content ~tag:"pre" ~shortlabel:"example"
   *                                    ~pp_content:pp_print_string ~content:x) path;
   *  | None -> ()); *)

  fprintf fmt "@]"

let pp_print_rst_full_service fmt ({ path ; meth } as repr) =
  fprintf fmt ".. _%s :@\n@\n**%s %s**@\n@\n"
    (ref_of_path path)
    (Resto.string_of_meth meth)
    ("/" ^ String.concat "/" path);
  fprintf fmt "%a" pp_print_html_tabs repr

let rec pp_print_rst_service_tree fmt node =
  let open Format in
  match node with
  | Root l -> (pp_print_list ~pp_sep:pp_print_cut pp_print_rst_service_tree) fmt l
  | SymbNode (_, l) -> (pp_print_list ~pp_sep:pp_print_cut pp_print_rst_service_tree) fmt l
  | Node ( repr , l) ->
      (* Generates services details and ref for links  *)
      fprintf fmt "%a@\n@\n" pp_print_rst_full_service repr;
      fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_newline pp_print_rst_service_tree) l

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

let ppf = Format.std_formatter
let err_ppf = Format.err_formatter

let run ?(rpc_port=18731) () =
  (* Client context *)
  let rpc_config = { RPC_client.default_config with port=rpc_port } in
  let open Client_config in
  let {block; _}  = default_cli_args in
  let (cctxt : #Tezos_client_base.Client_context.full) =
    new Client_context_unix.unix_full ~block ~base_dir: "/" ~rpc_config
  in

  let print_header () =
    (* Style : hack *)
    fprintf ppf "%a@." pp_print_rst_raw_html style;
    (* Script : hack *)
    fprintf ppf "%a@." pp_print_rst_raw_html script;
    (* Page title *)
    fprintf ppf "%a" pp_print_rst_h1 "RPC API";
    (* include/copy usage.rst from input  *)
    let rec loop () =
      let s = read_line () in
      fprintf ppf "%s@\n" s;
      loop ()
    in begin try loop () with End_of_file -> () end
  in
  (make_tree cctxt [] >>= function
    | Ok service_tree ->
        (* Print header!! *)
        fprintf ppf "@\n";
        print_header ();
        fprintf ppf "@\n";

        (* Shell RPCs tree *)
        fprintf ppf "%a@." (pp_print_rst_hierarchy ~title:"Client RPCs - Index") service_tree;
        fprintf ppf "%a" pp_print_rst_h2 "Client RPCs - Full description";
        fprintf ppf "%a@." pp_print_rst_service_tree service_tree;
        Lwt.return 0

    | Error _ ->
        Format.eprintf "[RPC Doc Generation] Client : Couldn't reach node\n";
        Lwt.return 1) >>= function
  | 0 ->
      (* Alpha Protocol RPCs *)
      let path_proto_alpha = String.split '/' "/blocks/head/proto" in
      begin
        make_tree cctxt path_proto_alpha >>= function
        | Ok service_tree ->
            (* Proto alpha RPCs tree *)
            fprintf ppf "%a@." (pp_print_rst_hierarchy ~title:"Protocol Alpha RPCs - Index") service_tree;
            fprintf ppf "%a" pp_print_rst_h2 "Protocol Alpha RPCs - Full description";
            fprintf ppf "%a@." pp_print_rst_service_tree service_tree;

            Lwt.return 0

        | Error _ ->
            Format.fprintf err_ppf "[RPC Doc Generation] Proto alpha : Couldn't reach node\n";
            Lwt.return 1
      end
  | _ -> Lwt.return 1

let () =
  Pervasives.exit
    (Lwt_main.run
       begin try
           if Array.length Sys.argv > 1 then
             let rpc_port = int_of_string Sys.argv.(1) in
             run ~rpc_port ()
           else
             run ()
         with _ ->
           run ()
       end)
