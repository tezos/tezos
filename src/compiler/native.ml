(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** The OCaml compiler not being implemented with Lwt, the compilation
    take place in a separated process (by using [Lwt_process.exec]).

    The [main] function is the entry point for the forked process.
    While [Updater.compile] is the 'forking' function to be called by
    the [tezos-node] process.

*)

(* TODO: fail in the presence of "external" *)

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

let warnings = "+a-4-6-7-9-29-40..42-44-45-48"
let warn_error = "-a+8"

let () =
  Clflags.unsafe_string := false ;
  Clflags.native_code := true

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
    "tezos_protocol_compiler__Registerer", tezos_protocol_compiler__Registerer_cmi ;
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

let hash_file file =
  let open Sodium.Generichash in
  let buflen = 8092 in
  let buf = BytesLabels.create buflen in
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o600 in
  let state = init ~size:32 () in
  let loop () =
    match Unix.read fd buf 0 buflen with
    | 0 -> ()
    | nb_read ->
        Bytes.update state @@
        if nb_read = buflen then buf else BytesLabels.sub buf ~pos:0 ~len:nb_read
  in
  loop () ;
  Unix.close fd ;
  BytesLabels.unsafe_to_string (Bytes.of_hash (final state))

(** Semi-generic compilation functions *)

let pack_objects output objects =
  let output = output ^ ".cmx" in
  Compmisc.init_path true;
  Asmpackager.package_files
    ~backend Format.err_formatter Env.initial_safe_string objects output ;
  Warnings.check_fatal () ;
  output

let link_shared output objects =
  Compenv.(readenv Format.err_formatter Before_link) ;
  Compmisc.init_path true;
  Asmlink.link_shared Format.err_formatter objects output ;
  Warnings.check_fatal ()

let compile_ml ?for_pack ml =
  let target = Filename.chop_extension ml in
  Clflags.for_package := for_pack ;
  Compenv.(readenv Format.err_formatter (Before_compile ml));
  Optcompile.implementation ~backend Format.err_formatter ml target ;
  Clflags.for_package := None ;
  target ^ ".cmx"

module Meta = struct

  let name = "TEZOS_PROTOCOL"

  let config_file_encoding =
    let open Data_encoding in
    obj3
      (opt "hash"
         ~description:"Used to force the hash of the protocol"
         Protocol_hash.encoding)
      (opt "expected_env_version"
         Protocol.env_version_encoding)
      (req "modules"
         ~description:"Modules comprising the protocol"
         (list string))

  let to_file dirname ?hash ?env_version modules =
    let config_file =
      Data_encoding.Json.construct
        config_file_encoding
        (hash, env_version, modules) in
    Utils.write_file ~bin:false (dirname // name) @@
    Data_encoding_ezjsonm.to_string config_file

  let of_file dirname =
    Utils.read_file ~bin:false (dirname // name) |>
    Data_encoding_ezjsonm.from_string |> function
    | Error err -> Pervasives.failwith err
    | Ok json -> Data_encoding.Json.destruct config_file_encoding json

end

let find_component dirname module_name =
  let open Protocol in
  let name_lowercase = String.uncapitalize_ascii module_name in
  let implementation = dirname // name_lowercase ^ ".ml" in
  let interface = implementation ^ "i" in
  match Sys.file_exists implementation, Sys.file_exists interface with
  | false, _ -> Pervasives.failwith @@ "Not such file: " ^ implementation
  | true, false ->
      let implementation = Utils.read_file ~bin:false implementation in
      { name = module_name; interface = None; implementation }
  | _ ->
      let interface = Utils.read_file ~bin:false interface in
      let implementation = Utils.read_file ~bin:false implementation in
      { name = module_name; interface = Some interface; implementation }

let read_dir dirname =
  let hash, expected_env, modules = Meta.of_file dirname in
  let components = List.map (find_component dirname) modules in
  let expected_env = match expected_env with None -> Protocol.V1 | Some v -> v in
  let protocol = Protocol.{ expected_env ; components } in
  let hash =
    match hash with
    | None -> Protocol.hash protocol
    | Some hash -> hash in
  hash, protocol

(** Main *)

let mktemp_dir () =
  Filename.get_temp_dir_name () //
  Printf.sprintf "tezos-protocol-build-%06X" (Random.int 0xFFFFFF)

let main () =
  Random.self_init () ;
  let anonymous = ref []
  and static = ref false
  and build_dir = ref None in
  let args_spec = [
    "-static", Arg.Set static, " Only build the static library (no .cmxs)";
    "-bin-annot", Arg.Set Clflags.binary_annotations, " (see ocamlopt)" ;
    "-g", Arg.Set Clflags.debug, " (see ocamlopt)" ;
    "-build-dir", Arg.String (fun s -> build_dir := Some s),
    "use custom build directory and preserve build artifacts"
  ] in
  let usage_msg =
    Printf.sprintf
      "Usage: %s [options] <out> <srcdir>\nOptions are:"
      Sys.argv.(0) in
  Arg.parse args_spec (fun s -> anonymous := s :: !anonymous) usage_msg ;
  let (output, source_dir) =
    match List.rev !anonymous with
    | [ output ; protocol_dir ] -> output, protocol_dir
    | _ -> Arg.usage args_spec usage_msg ; Pervasives.exit 1 in
  let build_dir =
    match !build_dir with
    | None ->
        let dir = mktemp_dir () in
        at_exit (fun () -> Lwt_main.run (Lwt_utils.remove_dir dir)) ;
        dir
    | Some dir -> dir in
  Lwt_main.run (Lwt_utils.create_dir ~perm:0o755 build_dir) ;
  Lwt_main.run (Lwt_utils.create_dir ~perm:0o755 (Filename.dirname output)) ;
  let hash, protocol = read_dir source_dir in
  (* Generate the 'functor' *)
  let functor_file = build_dir // "functor.ml" in
  let oc = open_out functor_file in
  Packer.dump oc
    (Array.map
       begin fun { Protocol.name }  ->
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

  load_embeded_cmis register_env ;
  load_cmi_from_file proto_cmi ;

  (* Compiler the 'registering module' *)
  let register_file = Filename.dirname functor_file // "register.ml" in
  create_file register_file
    (Printf.sprintf
       "module Name = struct let name = %S end\n\
       \ let () = Tezos_protocol_compiler__Registerer.register Name.name (module %s.Make)"
       (Protocol_hash.to_b58check hash)
       functor_unit) ;
  let register_object = compile_ml ~for_pack register_file in

  let resulting_object =
    pack_objects output [ packed_protocol_object ; register_object ] in

  (* Create the final [cmxs] *)
  if not !static then begin
    Clflags.link_everything := true ;
    link_shared (output ^ ".cmxs") [resulting_object] ;
  end
