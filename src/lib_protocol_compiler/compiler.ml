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

let warnings = "+a-4-6-7-9-29-40..42-44-45-48"
let warn_error = "-a+8"

let () =
  Clflags.unsafe_string := false

(** Override the default 'Env.Persistent_signature.load'
    with a lookup in locally defined hashtable.
*)

let preloaded_cmis : (string, Env.Persistent_signature.t) Hashtbl.t =
  Hashtbl.create ~random:true 42

(* Set hook *)
let () =
  Env.Persistent_signature.load :=
    (fun ~unit_name ->
       try Some (Hashtbl.find preloaded_cmis (String.capitalize_ascii unit_name))
       with Not_found -> None)

let load_cmi_from_file file =
  Hashtbl.add preloaded_cmis
    (String.capitalize_ascii Filename.(basename (chop_extension file)))
    { filename = file ;
      cmi = Cmi_format.read_cmi file ;
    }

let load_embeded_cmi (unit_name, content) =
  let content = Bytes.of_string content in
  (* Read cmi magic *)
  let magic_len = String.length Config.cmi_magic_number in
  let magic = Bytes.sub content 0 magic_len in
  assert (magic = Bytes.of_string Config.cmi_magic_number) ;
  (* Read cmi_name and cmi_sign *)
  let pos = magic_len in
  let (cmi_name, cmi_sign) = Marshal.from_bytes content pos in
  let pos = pos + Marshal.total_size content pos in
  (* Read cmi_crcs *)
  let cmi_crcs = Marshal.from_bytes content pos in
  let pos = pos + Marshal.total_size content pos  in
  (* Read cmi_flags *)
  let cmi_flags = Marshal.from_bytes content pos in
  (* TODO check crcrs... *)
  Hashtbl.add
    preloaded_cmis
    (String.capitalize_ascii unit_name)
    { filename  =  unit_name ^ ".cmi" ;
      cmi = { cmi_name; cmi_sign; cmi_crcs; cmi_flags } ;
    }

let load_embeded_cmis cmis = List.iter load_embeded_cmi cmis

(** Compilation environment.

    [tezos_protocol_env] defines the list of [cmi] available while compiling
    the protocol version. The [cmi] are packed into the [tezos-node]
    binary by using [ocp-ocamlres], see the Makefile.

    [register_env] defines a complementary list of [cmi] available
    while compiling the generated [register.ml] file (that register
    the protocol first-class module into the [Updater.versions]
    hashtable).

*)


let tezos_protocol_env =
  let open Embedded_cmis in
  [
    "CamlinternalFormatBasics", camlinternalFormatBasics_cmi ;
    "Tezos_protocol_environment_sigs", tezos_protocol_environment_sigs_cmi ;
    "Tezos_protocol_environment_sigs__V1", tezos_protocol_environment_sigs__V1_cmi ;
  ]

let register_env =
  let open Embedded_cmis in
  [
    "tezos_protocol_registerer__Registerer", tezos_protocol_registerer__Registerer_cmi ;
  ]


(** Helpers *)

let (//) = Filename.concat

let create_file ?(perm = 0o644) name content =
  let open Unix in
  let fd = openfile name [O_TRUNC; O_CREAT; O_WRONLY] perm in
  ignore(write_substring fd content 0 (String.length content));
  close fd

let safe_unlink file =
  try Unix.unlink file
  with Unix.Unix_error(Unix.ENOENT, _, _) -> ()

let unlink_cmi dir (file, _) =
  safe_unlink (dir // file ^ ".cmi")

let unlink_object obj =
  safe_unlink obj;
  safe_unlink (Filename.chop_suffix obj ".cmx" ^ ".cmi");
  safe_unlink (Filename.chop_suffix obj ".cmx" ^ ".o")

let debug_flag = ref false

let debug fmt =
  if !debug_flag then Format.eprintf fmt
  else Format.ifprintf Format.err_formatter fmt

let mktemp_dir () =
  Filename.get_temp_dir_name () //
  Printf.sprintf "tezos-protocol-build-%06X" (Random.int 0xFFFFFF)

(** Main *)

type driver = {
  compile_ml: ?for_pack:string -> string -> string ;
  pack_objects: string -> string list -> string ;
  link_shared: string -> string list -> unit ;
}

let main { compile_ml ; pack_objects ; link_shared } =
  Random.self_init () ;
  let anonymous = ref []
  and static = ref false
  and register = ref false
  and build_dir = ref None
  and output_file = ref None
  and output_dep = ref false
  and hash_only = ref false
  and check_protocol_hash = ref true in
  let args_spec = [
    "-o", Arg.String (fun s -> output_file := Some s), "" ;
    "-hash-only", Arg.Set hash_only, " Only display the hash of the protocol and don't compile" ;
    "-no-hash-check", Arg.Clear check_protocol_hash, " Don't check that TEZOS_PROTOCOL declares the expected protocol hash (if existent)" ;
    "-static", Arg.Set static, " Only build the static library (no .cmxs)" ;
    "-register", Arg.Set register, " Generate the `Registerer` module" ;
    "-bin-annot", Arg.Set Clflags.binary_annotations, " (see ocamlopt)" ;
    "-g", Arg.Set Clflags.debug, " (see ocamlopt)" ;
    "-output-dep", Arg.Set output_dep, " ..." ;
    "-build-dir", Arg.String (fun s -> build_dir := Some s),
    "use custom build directory and preserve build artifacts"
  ] in
  let usage_msg =
    Printf.sprintf
      "Usage: %s [options] <srcdir>\nOptions are:"
      Sys.argv.(0) in
  Arg.parse args_spec (fun s -> anonymous := s :: !anonymous) usage_msg ;
  let source_dir =
    match List.rev !anonymous with
    | [ protocol_dir ] -> protocol_dir
    | _ -> Arg.usage args_spec usage_msg ; Pervasives.exit 1 in
  let announced_hash, protocol =
    match Lwt_main.run (Lwt_utils_unix.Protocol.read_dir source_dir) with
    | Ok (hash, proto) -> (hash, proto)
    | Error err ->
        Format.eprintf
          "Failed to read TEZOS_PROTOCOL: %a" pp_print_error err ;
        exit 2 in
  let real_hash = Protocol.hash protocol in
  if !hash_only then begin
    Format.printf "%a@." Protocol_hash.pp real_hash ;
    exit 0 ;
  end ;
  let hash =
    match announced_hash with
    | None -> real_hash
    | Some hash
      when !check_protocol_hash && not (Protocol_hash.equal real_hash hash) ->
        Format.eprintf
          "Inconsistent hash for protocol in TEZOS_PROTOCOL.@\n\
           Found: %a@\n\
           Expected: %a@."
          Protocol_hash.pp hash
          Protocol_hash.pp real_hash ;
        exit 2
    | Some hash -> hash in
  let build_dir =
    match !build_dir with
    | None ->
        let dir = mktemp_dir () in
        at_exit (fun () -> Lwt_main.run (Lwt_utils_unix.remove_dir dir)) ;
        dir
    | Some dir -> dir in
  let output =
    match !output_file with
    | Some output -> output
    | None -> Format.asprintf "proto_%a" Protocol_hash.pp hash in
  Lwt_main.run (Lwt_utils_unix.create_dir ~perm:0o755 build_dir) ;
  Lwt_main.run (Lwt_utils_unix.create_dir ~perm:0o755 (Filename.dirname output)) ;
  (* Generate the 'functor' *)
  let functor_file = build_dir // "functor.ml" in
  let oc = open_out functor_file in
  Packer.dump oc hash
    (Array.map
       begin fun { Protocol.name ; _ }  ->
         let name_lowercase = String.uncapitalize_ascii name in
         source_dir // name_lowercase ^ ".ml"
       end
       (Array.of_list protocol.components)) ;
  close_out oc ;
  (* Compile the protocol *)
  let proto_cmi = Filename.chop_extension functor_file ^ ".cmi" in
  let functor_unit =
    String.capitalize_ascii
      Filename.(basename (chop_extension functor_file)) in
  let for_pack = String.capitalize_ascii (Filename.basename output) in
  (* Initialize the compilers *)
  Compenv.(readenv Format.err_formatter Before_args);
  Clflags.nopervasives := true;
  Clflags.no_std_include := true ;
  Clflags.include_dirs := [Filename.dirname functor_file] ;
  Warnings.parse_options false warnings ;
  Warnings.parse_options true warn_error ;

  load_embeded_cmis tezos_protocol_env ;
  let packed_protocol_object = compile_ml ~for_pack functor_file in

  let register_objects =
    if not !register then
      []
    else begin
      load_embeded_cmis register_env ;
      load_cmi_from_file proto_cmi ;
      (* Compiler the 'registering module' *)
      let register_file = Filename.dirname functor_file // "register.ml" in
      create_file register_file
        (Printf.sprintf
           "module Name = struct let name = %S end\n\
           \ let () = Tezos_protocol_registerer__Registerer.register Name.name (module %s.Make)"
           (Protocol_hash.to_b58check hash)
           functor_unit) ;
      let register_object = compile_ml ~for_pack register_file in
      [ register_object ]
    end
  in

  let resulting_object =
    pack_objects output (packed_protocol_object :: register_objects) in

  (* Create the final [cmxs] *)
  if not !static then begin
    Clflags.link_everything := true ;
    link_shared (output ^ ".cmxs") [resulting_object] ;
  end ;

  if !output_dep then begin
    let dsrc = Digest.file functor_file in
    let dimpl = Digest.file resulting_object in
    let dintf = Digest.file (Filename.chop_extension resulting_object ^ ".cmi") in
    Format.printf "module Toto = struct include %s end ;; \n" for_pack ;
    Format.printf "let src_digest = %S ;;\n" (Digest.to_hex dsrc) ;
    Format.printf "let impl_digest = %S ;;\n" (Digest.to_hex dimpl) ;
    Format.printf "let intf_digest = %S ;;\n" (Digest.to_hex dintf)
  end ;

  Format.printf "Success: %a.@." Protocol_hash.pp hash

