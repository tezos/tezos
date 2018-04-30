(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let protocols = [
  "Alpha", "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" ;
]

module Rst = struct

  let pp_title ~char ppf title =
    let sub = String.map (fun _ -> char) title in
    Format.fprintf ppf "@[<v 0>%s@ %s@ @ @]" title sub

  let pp_h1 = pp_title ~char:'#'
  let pp_h2 = pp_title ~char:'*'
  let pp_h3 = pp_title ~char:'='
  let pp_h4 = pp_title ~char:'`'

  let pp_raw_html ppf str =
    Format.fprintf ppf "@[<v>.. raw:: html@   @   %s@ @ @]"
      (Re.Str.global_replace (Re.Str.regexp "\n") "\n  " str)

  let pp_html ppf f =
    Format.fprintf ppf
      "@[<v 2>.. raw:: html@ @ %a@]@\n@\n"
      (fun ppf () -> f ppf) ()

  let pp_ref ppf name = Format.fprintf ppf ".. _%s :@\n@\n" name

end

let pp_name ppf = function
  | [] | [""] -> Format.pp_print_string ppf "/"
  | prefix -> Format.pp_print_string ppf (String.concat "/" prefix)

let ref_of_service (prefix, meth) =
  Format.asprintf "%s_%s"
    (Resto.string_of_meth meth)
    (Re.Str.global_replace
       (Re.Str.regexp "<\\([^>]*\\)>")
       "\\1"
       (String.concat "--" prefix))

module Index = struct

  let rec pp prefix ppf dir =
    let open Resto.Description in
    match dir with
    | Empty -> Format.fprintf ppf "Empty"
    | Static { services ; subdirs = None } ->
        pp_services prefix ppf services
    | Static { services ; subdirs = Some (Suffixes map) } ->
        Format.fprintf ppf "@[<v 2>%a@ @ %a@]"
          (pp_services prefix) services
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ @ ")
             (pp_suffixes prefix))
          (Resto.StringMap.bindings map)
    | Static { services ; subdirs = Some (Arg (arg, dir)) } ->
        let name = Format.asprintf "<%s>" arg.name in
        Format.fprintf ppf "@[<v 2>%a@ @ %a@]"
          (pp_services prefix) services
          (pp_suffixes prefix) (name, dir)
    | Dynamic _ ->
        Format.fprintf ppf "* %a (<dyn>)" pp_name prefix

  and pp_suffixes prefix ppf (name, dir) =
    pp (prefix @ [name]) ppf dir

  and pp_services prefix ppf services =
    match (Resto.MethMap.bindings services) with
    | [] ->
        Format.fprintf ppf "* %a" pp_name prefix
    | _ :: _ as services ->
        Format.fprintf ppf "* %a (@[<h>%a@])"
          pp_name prefix
          (Format.pp_print_list
             ~pp_sep:Format.pp_print_space
             (pp_service_method prefix)) services

  and pp_service_method prefix ppf (meth, _service) =
    Format.fprintf ppf "`%s <%s_>`_"
      (Resto.string_of_meth meth)
      (ref_of_service (prefix, meth))

end

module Description = struct

  module Query = struct

    let pp_arg fmt =
      let open RPC_arg in
      function { name ; _ } ->
        Format.fprintf fmt "<%s>" name

    let pp_title_item ppf =
      let open RPC_description in
      function {name ; kind ; _ } ->
      match kind with
      | Single arg | Optional arg ->
          Format.fprintf ppf "[%s=%a]" name pp_arg arg
      | Flag ->
          Format.fprintf ppf "[%s]" name
      | Multi arg ->
          Format.fprintf ppf "(%s=%a)\\*" name pp_arg arg

    let pp_title ppf query =
      Format.fprintf ppf "%s%a"
        (if query = [] then "" else "?")
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "&")
           pp_title_item) query

    let pp_html_arg fmt =
      let open RPC_arg in
      function { name ; _ } ->
        Format.fprintf fmt "&lt;%s&gt;" name

    let pp_item ppf =
      let open RPC_description in
      function { name ; description ; kind } ->
        begin match kind with
          | Single arg
          | Optional arg
          | Multi arg ->
              Format.fprintf ppf
                "<span class=\"query\">%s = %a</span>"
                name pp_html_arg arg
          | Flag ->
              Format.fprintf ppf
                "<span class=\"query\">%s</span>"
                name
        end ;
        begin match description with
          | None -> ()
          | Some descr -> Format.fprintf ppf " : %s" descr
        end

    let pp ppf query =
      match query with
      | [] -> ()
      | _ :: _ as query ->
          Format.fprintf ppf
            "</p> <p>Optional query arguments :<ul><li>%a</li></ul>"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.fprintf ppf "</li><li>")
               pp_item)
            query

  end

  module Tabs = struct

    let pp_tab_div ppf f =
      Format.fprintf ppf
        "@[<v 2><div class=\"tab\">%a</div>@]"
        (fun ppf () -> f ppf) ()

    let pp_tabcontent_div ~id ~class_ ppf f =
      Format.fprintf ppf
        "@[<v 2><div id=\"%s\" class=\"%s tabcontent\">@ \
         %a@ \
         @]</div>@ "
        id class_ (fun ppf () -> f ppf) ()

    let pp_button ppf ?(default=false) ~shortlabel ~content target_ref =
      Format.fprintf ppf
        "<button class=\"tablinks%s\" onclick=\"showTab(this, '%s', '%s')\">%s</button>@ "
        (if default then " defaultOpen" else "")
        (target_ref ^ shortlabel)
        target_ref
        content

    let pp_content ppf ~tag ~shortlabel target_ref pp_content content =
      pp_tabcontent_div
        ~id:(target_ref ^ shortlabel) ~class_:target_ref ppf
        begin fun ppf ->
          Format.fprintf ppf "<%s>@ %a</%s>" tag pp_content content tag
        end

    let pp_description ppf (service : _ RPC_description.service) =
      let open RPC_description in
      (* TODO collect and display arg description (in path and in query) *)
      Format.fprintf ppf "%s%a"
        (Option.unopt ~default:"" service.description)
        Query.pp service.query

    let pp ppf prefix service =
      let open RPC_description in
      let target_ref = ref_of_service (prefix, service.meth) in
      Rst.pp_html ppf begin fun ppf ->
        pp_tab_div ppf begin fun ppf ->
          pp_button ppf
            ~default:true ~shortlabel:"descr" ~content:"Description"
            target_ref ;
          Option.iter service.input ~f: begin fun __ ->
            pp_button ppf
              ~default:false ~shortlabel:"input" ~content:"Input format"
              target_ref
          end ;
          pp_button ppf
            ~default:false ~shortlabel:"output" ~content:"Output format"
            target_ref ;
        end ;
        pp_content ppf
          ~tag:"p" ~shortlabel:"descr" target_ref
          pp_description service ;
        Option.iter service.input ~f: begin fun schema ->
          pp_content ppf
            ~tag:"pre" ~shortlabel:"input" target_ref
            Json_schema.pp schema ;
        end ;
        pp_content ppf
          ~tag:"pre" ~shortlabel:"output" target_ref
          Json_schema.pp service.output ;
      end

  end

  let rec pp prefix ppf dir =
    let open Resto.Description in
    match dir with
    | Empty -> ()
    | Static { services ; subdirs = None } ->
        pp_services prefix ppf services
    | Static { services ; subdirs = Some (Suffixes map) } ->
        pp_services prefix ppf services ;
        Format.pp_print_list (pp_suffixes prefix)
          ppf (Resto.StringMap.bindings map)
    | Static { services ; subdirs = Some (Arg (arg, dir)) } ->
        let name = Format.asprintf "<%s>" arg.name in
        pp_services prefix ppf services ;
        pp_suffixes prefix ppf (name, dir)
    | Dynamic _ -> ()

  and pp_suffixes prefix ppf (name, dir) =
    pp (prefix @ [name]) ppf dir

  and pp_services prefix ppf services =
    List.iter
      (pp_service prefix ppf)
      (Resto.MethMap.bindings services)

  and pp_service prefix ppf (meth, service) =
    Rst.pp_ref ppf (ref_of_service (prefix, meth)) ;
    Format.fprintf ppf "**%s %a%a**@\n@\n"
      (Resto.string_of_meth meth)
      pp_name prefix
      Query.pp_title service.query ;
    Tabs.pp ppf prefix service

end

let style = {css|
<style>
  .tab {
    overflow: hidden;
    border: 1px solid #ccc;
    background-color: #f1f1f1;
  }
  .tab button {
    background-color: inherit;
    float: left;
    border: none;
    outline: none;
    cursor: pointer;
    padding: 5px 10px;
  }
  .tab button:hover {
    background-color: #ddd;
  }
  .tab button.active {
    background-color: #ccc;
  }
  .tabcontent {
    display: none;
    padding: 6px 12px;
    border: 1px solid #ccc;
    border-top: none;
    max-height: 40ex;
    margin-bottom: 7ex;
    overflow: auto;
  }
  .tabcontent p {
    margin-bottom: 12px;
  }
  pre {
    font-size: 12px
  }
  .rst-content .section ul p {
    margin-bottom: 0;
  }
  span.query {
    font-family: monospace;
    white-space: pre;
  }
</style>
|css}

let script = {script|
<script>
  function showTab(elt, tab, ref) {
    var i, tabcontent, tablinks;
    tabcontent = document.getElementsByClassName(ref);
    for (i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = 'none';
    }

    tablinks = elt.parentNode.children;
    for (i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(' active', '');
    }

    document.getElementById(tab).style.display = 'block';
    elt.className += ' active';
  }

  document.addEventListener('DOMContentLoaded', function() {
    var a = document.getElementsByClassName('defaultOpen');
    for (i = 0; i < a.length; i++) { a[i].click() }
  })
</script>
|script}


let pp_document ppf descriptions =
  (* Style : hack *)
  Format.fprintf ppf "%a@." Rst.pp_raw_html style ;
  (* Script : hack *)
  Format.fprintf ppf "%a@." Rst.pp_raw_html script ;
  (* Page title *)
  Format.fprintf ppf "%a" Rst.pp_h1 "RPC API" ;
  (* include/copy usage.rst from input  *)
  let rec loop () =
    let s = read_line () in
    Format.fprintf ppf "%s@\n" s ;
    loop () in
  begin try loop () with End_of_file -> () end ;
  Format.fprintf ppf "@\n" ;
  (* Index *)
  Format.pp_set_margin ppf 10000 ;
  Format.pp_set_max_indent ppf 9000 ;
  Rst.pp_h2 ppf "RPCs - Index" ;
  List.iter
    (fun (name, prefix, rpc_dir) ->
       Rst.pp_h3 ppf name ;
       Format.fprintf ppf "%a@\n@\n" (Index.pp prefix) rpc_dir)
    descriptions ;
  (* Full description *)
  Rst.pp_h2 ppf "RPCs - Full description" ;
  Format.pp_set_margin ppf 80 ;
  Format.pp_set_max_indent ppf 76 ;
  List.iter
    (fun (name, prefix, rpc_dir) ->
       Rst.pp_h3 ppf name ;
       Format.fprintf ppf "%a@\n@\n" (Description.pp prefix) rpc_dir)
    descriptions

let genesis : State.Chain.genesis = {
  time =
    Time.of_notation_exn "2018-04-17T11:46:23Z" ;
  block =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisa52f8bUWPcg" ;
  protocol =
    Protocol_hash.of_b58check_exn
      "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im" ;
}

let main dir =
  let (/) = Filename.concat in
  let node_config : Node.config = {
    genesis ;
    patch_context = None ;
    store_root = dir / "store" ;
    context_root = dir / "context" ;
    p2p = None ;
    test_chain_max_tll = None ;
  } in
  Node.create
    node_config
    Node.default_peer_validator_limits
    Node.default_block_validator_limits
    Node.default_prevalidator_limits
    Node.default_chain_validator_limits >>=? fun node ->
  let shell_dir = Node.build_rpc_directory node in
  let protocol_dirs =
    List.map
      (fun (name, hash) ->
         let hash = Protocol_hash.of_b58check_exn hash in
         let (module Proto) = Registered_protocol.get_exn hash in
         "Protocol " ^ name,
         [".." ; "<block_id>"] ,
         RPC_directory.map (fun () -> assert false) @@
         Block_directory.build_raw_rpc_directory (module Proto) (module Proto))
      protocols in
  let dirs = ("Shell", [""], shell_dir) :: protocol_dirs in
  Lwt_list.map_p
    (fun (name, path, dir) ->
       RPC_directory.describe_directory ~recurse:true ~arg:() dir >>= fun dir ->
       Lwt.return (name, path, dir))
    dirs >>= fun descriptions ->
  let ppf = Format.std_formatter in
  pp_document ppf descriptions ;
  return ()

let () =
  Lwt_main.run begin
    Lwt_utils_unix.with_tempdir "tezos_rpcdoc_" main >>= function
    | Ok _ ->
        Lwt.return_unit
    | Error err ->
        Format.eprintf "%a@." pp_print_error err ;
        Pervasives.exit 1
  end

