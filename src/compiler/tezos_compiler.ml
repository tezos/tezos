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

(* GRGR TODO: fail in the presence of "external" *)

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

let usage () =
  Printf.eprintf
    "Usage: %s output.cmxs source_dir [--in-place]\n%!"
    Sys.argv.(0)

let warnings = "+a-4-6-7-9-29-40..42-44-45-48"
let warn_error = "-a"

let () =
  Clflags.unsafe_string := false ;
  Clflags.native_code := true

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
  [ "camlinternalFormatBasics", Embedded_cmis.camlinternalFormatBasics_cmi ;
    "proto_environment", Embedded_cmis.proto_environment_cmi ;
  ]

let register_env =
  [ "register", Embedded_cmis.register_cmi ]

(** Helpers *)

let (//) = Filename.concat

let create_file ?(perm = 0o644) name content =
  let open Unix in
  let fd = openfile name [O_TRUNC; O_CREAT; O_WRONLY] perm in
  ignore(write_substring fd content 0 (String.length content));
  close fd

let read_md5 file =
  let ic = open_in file in
  let md5 = input_line ic in
  close_in ic ;
  md5

let rec create_dir ?(perm = 0o755) dir =
  if not (Sys.file_exists dir) then begin
    create_dir (Filename.dirname dir);
    Unix.mkdir dir perm
  end

let dump_cmi dir (file, content) =
  create_file (dir // file ^ ".cmi") content

let safe_unlink file =
  try Unix.unlink file
  with Unix.Unix_error(Unix.ENOENT, _, _) -> ()

let unlink_cmi dir (file, _) =
  safe_unlink (dir // file ^ ".cmi")

let unlink_object obj =
  safe_unlink obj;
  safe_unlink (Filename.chop_suffix obj ".cmx" ^ ".cmi");
  safe_unlink (Filename.chop_suffix obj ".cmx" ^ ".o")


(** TEZOS_PROTOCOL files *)

module Meta = struct

  let hash_wrapper =
    let open Config_file in
    { to_raw = (fun h -> Raw.String (Protocol_hash.to_b48check h));
      of_raw = (function
          | Raw.String h -> begin try
                Protocol_hash.of_b48check h
              with _ ->
                let error oc = Printf.fprintf oc "Invalid Base48Check-encoded SHA256 key %S" h in
                raise (Wrong_type error)
            end
          | _ ->
              let error oc =
                Printf.fprintf oc "Unexcepted value: should be a Base48Check-encoded SHA256 key." in
              raise (Wrong_type error));
    }

  class protocol_hash_cp =
    [Protocol_hash.t] Config_file.cp_custom_type hash_wrapper

  let to_file file hash modules =
    let group = new Config_file.group in
    let _ = new protocol_hash_cp ~group ["hash"] hash "" in
    let _ =
      new Config_file.list_cp Config_file.string_wrappers ~group
        ["modules"] modules "" in
    group#write file

  let of_file file =
    let group = new Config_file.group in
    let hash =
      new protocol_hash_cp ~group ["hash"]
        (Protocol_hash.of_b48check
           "TnrnfGHMCPAcxtMAHXdpfebbnn2XvPAxq7DHbpeJbKTkJQPgcgRGr")
        "" in
    let modules =
      new Config_file.list_cp Config_file.string_wrappers ~group
        ["modules"] [] "" in
    group#read file;
    (hash#get, modules#get)

end

(** Semi-generic compilation functions *)

let compile_mli ?(ctxt = "") ?(keep_object = false) target mli =
  Printf.printf "OCAMLOPT%s %s\n%!" ctxt (Filename.basename target ^ ".cmi");
  Compenv.(readenv Format.err_formatter (Before_compile mli)) ;
  Optcompile.interface Format.err_formatter mli target ;
  if not keep_object then
    at_exit (fun () -> safe_unlink (target ^ ".cmi"))


let compile_ml ?(ctxt = "") ?(keep_object = false) ?for_pack target ml =
  Printf.printf "OCAMLOPT%s %s\n%!" ctxt (Filename.basename target ^ ".cmx") ;
  Compenv.(readenv Format.err_formatter (Before_compile ml));
  Clflags.for_package := for_pack;
  Optcompile.implementation
    ~backend Format.err_formatter ml target;
  Clflags.for_package := None;
  if not keep_object then
    at_exit (fun () -> unlink_object (target ^ ".cmx")) ;
  target ^ ".cmx"

let modification_date file = Unix.((stat file).st_mtime)

let compile_units
    ?ctxt
    ?(update_needed = true)
    ?keep_object ?for_pack ~source_dir ~build_dir units =
  let compile_unit update_needed unit =
    let basename = String.uncapitalize_ascii unit in
    let mli = source_dir // basename ^ ".mli" in
    let cmi = build_dir // basename ^ ".cmi" in
    let ml  = source_dir // basename ^ ".ml"  in
    let cmx = build_dir // basename ^ ".cmx"  in
    let target = build_dir  // basename in
    let update_needed =
      update_needed
      || not (Sys.file_exists cmi)
      || ( Sys.file_exists mli
           && modification_date cmi < modification_date mli )
      || not (Sys.file_exists cmx)
      || modification_date cmx < modification_date ml in
    if update_needed then begin
      unlink_object cmx ;
      if Sys.file_exists mli then compile_mli ?ctxt ?keep_object target mli ;
      ignore (compile_ml ?ctxt ?keep_object ?for_pack target ml)
    end ;
    update_needed, cmx in
  List.fold_left
    (fun (update_needed, acc) unit->
       let update_needed, output = compile_unit update_needed unit in
       update_needed, output :: acc)
    (update_needed, []) units
  |> snd |> List.rev

let pack_objects ?(ctxt = "") ?(keep_object = false) output objects =
  Printf.printf "PACK%s %s\n%!" ctxt (Filename.basename output);
  Compmisc.init_path true;
  Asmpackager.package_files
    ~backend Format.err_formatter Env.initial_safe_string objects output;
  if not keep_object then at_exit (fun () -> unlink_object output) ;
  Warnings.check_fatal ()

let link_shared output objects =
  Printf.printf "LINK %s\n%!" (Filename.basename output);
  Compenv.(readenv Format.err_formatter Before_link);
  Compmisc.init_path true;
  if Filename.check_suffix output ".cmxa" then
    Asmlibrarian.create_archive objects output
  else
    Asmlink.link_shared Format.err_formatter objects output;
  Warnings.check_fatal ()

(** Main for the 'forked' compiler.

    It expect the following arguments:

      output.cmxs source_dir

    where, [source_dir] should contains a TEZOS_PROTOCOL file such as:

      hash = "69872d2940b7d11c9eabbc685115bd7867a94424"
      modules = [Data; Main]

    The [source_dir] should also contains the corresponding source
    file. For instance: [data.ml], [main.ml] and optionnaly [data.mli]
    and [main.mli].

 *)

let create_register_file client file hash packname modules =
  let unit = List.hd (List.rev modules) in
  let error_monad = packname ^ ".Local_error_monad.Error_monad" in
  create_file file
    (Printf.sprintf
       "module Packed_protocol = struct\n\
       \  let hash = (Hash.Protocol_hash.of_b48check %S)\n\
       \  type error = %s.error = ..\n\
       \  type 'a tzresult = 'a %s.tzresult\n\
       \  include %s.%s\n\
       \  let error_encoding  = %s.error_encoding  ()\n\
       \  let classify_errors = %s.classify_errors\n\
       \  let pp = %s.pp\n\
       \ end\n\
       \ %s\n\
       "
       (Protocol_hash.to_b48check hash)
       error_monad
       error_monad
       packname (String.capitalize_ascii unit)
       error_monad
       error_monad
       error_monad
       (if client then
          "include Register.Make(Packed_protocol)"
        else
          "let () = Register.register (module Packed_protocol : PACKED_PROTOCOL)"))

let mktemp_dir () =
  Filename.get_temp_dir_name () //
  Printf.sprintf "tezos-protocol-build-%06X" (Random.int 0xFFFFFF)

let main () =

  Random.self_init () ;
  Sodium.Random.stir () ;

  let anonymous = ref []
  and client = ref false
  and build_dir = ref None
  and include_dirs = ref [] in
  let args_spec = [
    "--client", Arg.Set client, "TODO" ;
    "-I", Arg.String (fun s -> include_dirs := s :: !include_dirs), "TODO" ;
    "--build-dir", Arg.String (fun s -> build_dir := Some s), "TODO"] in
  let usage_msg = "TODO" in
  Arg.parse args_spec (fun s -> anonymous := s :: !anonymous) "TODO" ;

  let client = !client and include_dirs = !include_dirs in
  let output, source_dir =
    match List.rev !anonymous with
    | [ output ; source_dir ] -> output, source_dir
    | _ -> Arg.usage args_spec usage_msg ; Pervasives.exit 1 in
  if include_dirs <> [] && not client then begin
    Arg.usage args_spec usage_msg ; Pervasives.exit 1
  end ;

  let keep_object, build_dir, sigs_dir =
    match !build_dir with
    | None ->
        let build_dir = mktemp_dir () in
        false, build_dir, build_dir // "sigs"
    | Some build_dir ->
        true, build_dir, mktemp_dir () in
  create_dir build_dir ;
  create_dir sigs_dir ;
  at_exit (fun () ->
      Unix.rmdir sigs_dir ;
      if not keep_object then Unix.rmdir build_dir ) ;

  let hash, units = Meta.of_file (source_dir // "TEZOS_PROTOCOL") in
  let packname =
    if keep_object then
      String.capitalize_ascii (Filename.(basename @@ chop_extension output))
    else
      Format.asprintf "Protocol_%a" Protocol_hash.pp hash in
  let packed_objects =
    if keep_object then
      Filename.dirname output // String.uncapitalize_ascii packname ^ ".cmx"
    else
      build_dir // packname ^ ".cmx" in
  let ctxt = Printf.sprintf " (%s)" (Filename.basename output) in
  let logname =
    if keep_object then
      try
        Scanf.sscanf
          Filename.(basename @@ chop_extension output)
          "embedded_proto_%s"
          (fun s -> "proto." ^ s)
      with _ ->
        Filename.(basename @@ chop_extension output)
    else
      Format.asprintf "proto.%a" Protocol_hash.pp hash in

  (* TODO proper error *)
  assert (List.length units >= 1);

  (* Initialize the compilers *)
  Compenv.(readenv Format.err_formatter Before_args);
  if not client then Clflags.no_std_include := true;
  Clflags.include_dirs := build_dir :: sigs_dir :: include_dirs;
  Clflags.nopervasives := true;
  Warnings.parse_options false warnings;
  Warnings.parse_options true warn_error;
  if keep_object then Clflags.binary_annotations := true;

  let md5 =
    if not client then
      Digest.(to_hex (file Sys.executable_name))
    else
      try
        let environment_cmi =
          Misc.find_in_path_uncap !Clflags.include_dirs "environment.cmi" in
        let environment_cmx =
          Misc.find_in_path_uncap !Clflags.include_dirs "environment.cmx" in
        Digest.(to_hex (file Sys.executable_name) ^
                (to_hex (file environment_cmi)) ^
                (to_hex (file environment_cmx)))
      with Not_found ->
        Printf.eprintf "%s: Cannot find 'environment.cmi'.\n%!" Sys.argv.(0);
        Pervasives.exit 1
  in
  let update_needed =
    not (Sys.file_exists (build_dir // ".tezos_compiler"))
    || read_md5 (build_dir // ".tezos_compiler") <> md5 in

  if keep_object then
    create_file (build_dir // ".tezos_compiler") (md5 ^ "\n");

  Compenv.implicit_modules :=
    if client then [ "Environment" ] else [ "Proto_environment" ] ;

  (* Compile the /ad-hoc/ Error_monad. *)
  List.iter (dump_cmi sigs_dir) tezos_protocol_env ;
  at_exit (fun () -> List.iter (unlink_cmi sigs_dir) tezos_protocol_env ) ;
  let error_monad_unit = "local_error_monad" in
  let error_monad_ml = build_dir // error_monad_unit ^ ".ml" in
  create_file error_monad_ml @@ Printf.sprintf {|
      module Error_monad = struct
        type error_category = [ `Branch | `Temporary | `Permanent ]
        include Error_monad.Make()
      end
      module Logging = Logging.Make(struct let name = %S end)
    |}
    logname ;
  let error_monad_mli = build_dir // error_monad_unit ^ ".mli" in
  create_file error_monad_mli @@ Printf.sprintf {|
      module Error_monad : sig %s end
      module Logging : sig %s end
    |}
    Embedded_cmis.error_monad_mli
    Embedded_cmis.logging_mli  ;
  if not keep_object then
    at_exit (fun () ->
        safe_unlink error_monad_mli ;
        safe_unlink error_monad_ml) ;
  let error_monad_object =
    compile_units
      ~ctxt
      ~for_pack:packname
      ~keep_object
      ~build_dir ~source_dir:build_dir [error_monad_unit]
  in

  Compenv.implicit_modules :=
    !Compenv.implicit_modules @
    [ "Local_error_monad"; "Error_monad" ; "Hash" ; "Logging" ];

  (* Compile the protocol *)
  let objects =
    compile_units
      ~ctxt
      ~update_needed
      ~keep_object ~for_pack:packname ~build_dir ~source_dir units in
  pack_objects ~ctxt ~keep_object
    packed_objects (error_monad_object @ objects) ;

  (* Compiler the 'registering module' *)
  List.iter (dump_cmi sigs_dir) register_env;
  at_exit (fun () -> List.iter (unlink_cmi sigs_dir) register_env ) ;
  let register_unit =
    if client then
      Filename.dirname output //
      "register_" ^
      Filename.(basename @@ chop_extension output)
    else
      build_dir // Format.asprintf "register_%s" packname in
  let register_file = register_unit ^ ".ml" in
  create_register_file client register_file hash packname units ;
  if not keep_object then at_exit (fun () -> safe_unlink register_file) ;
  if keep_object then
    Clflags.include_dirs := !Clflags.include_dirs @ [Filename.dirname output] ;
  let register_object =
    compile_ml ~keep_object:client (register_unit) register_file in

  (* Create the final [cmxs] *)
  Clflags.link_everything := true ;
  link_shared output [packed_objects; register_object]
