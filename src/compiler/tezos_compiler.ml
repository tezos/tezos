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
  let name = "TEZOS_PROTOCOL"
  let config_file_encoding =
    let open Data_encoding in
    obj2
      (opt "hash" ~description:"Used to force the hash of the protocol" Protocol_hash.encoding)
      (req "modules" ~description:"Modules comprising the protocol" (list string))

  let to_file dirname ?hash modules =
    let config_file =
      Data_encoding.Json.construct config_file_encoding (hash, modules) in
    Utils.write_file ~bin:false (dirname // name) @@
    Data_encoding_ezjsonm.to_string config_file

  let of_file dirname =
    Utils.read_file ~bin:false (dirname // name) |>
    Data_encoding_ezjsonm.from_string |> function
    | Error err -> Pervasives.failwith err
    | Ok json -> Data_encoding.Json.destruct config_file_encoding json
end

module Protocol = struct

  type component = {
    name: string;
    interface: string option;
    implementation: string;
  }

  let component_encoding =
    let open Data_encoding in
    conv
      (fun { name ; interface; implementation } -> (name, interface, implementation))
      (fun (name, interface, implementation) -> { name ; interface ; implementation })
      (obj3
         (req "name" string)
         (opt "interface" string)
         (req "implementation" string))

  type t = component list
  type protocol = t
  let encoding = Data_encoding.list component_encoding

  let compare = Pervasives.compare
  let equal = (=)

  let to_bytes v = Data_encoding.Binary.to_bytes encoding v
  let of_bytes b = Data_encoding.Binary.of_bytes encoding b
  let hash proto = Protocol_hash.hash_bytes [to_bytes proto]

  let find_component dirname module_name =
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

  let of_dir dirname =
    let _hash, modules = Meta.of_file dirname in
    List.map (find_component dirname) modules
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

let link_shared ?(static=false) output objects =
  Printf.printf "LINK %s\n%!" (Filename.basename output);
  Compenv.(readenv Format.err_formatter Before_link);
  Compmisc.init_path true;
  if static then
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
  let environment_module = packname ^ ".Local_environment.Environment" in
  let error_monad_module = environment_module ^ ".Error_monad" in
  let context_module = environment_module ^ ".Context" in
  let hash_module = environment_module ^ ".Hash" in
  create_file file
    (Printf.sprintf
       "module Packed_protocol = struct\n\
       \  let hash = (%s.Protocol_hash.of_b58check %S)\n\
       \  type error = %s.error = ..\n\
       \  type 'a tzresult = 'a %s.tzresult\n\
       \  include %s.%s\n\
       \  let error_encoding  = %s.error_encoding  ()\n\
       \  let classify_errors = %s.classify_errors\n\
       \  let pp = %s.pp\n\
       \  let complete_b58prefix = %s.complete
       \ end\n\
       \ %s\n\
       "
       hash_module
       (Protocol_hash.to_b58check hash)
       error_monad_module
       error_monad_module
       packname (String.capitalize_ascii unit)
       error_monad_module
       error_monad_module
       error_monad_module
       context_module
       (if client then
          "include Register.Make(Packed_protocol)"
        else
          Printf.sprintf
            "let () = Register.register (%s.__cast (module Packed_protocol : %s.PACKED_PROTOCOL))" environment_module environment_module))

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
  let static = ref false in
  let args_spec = [
    "-static", Arg.Set static, " Build a library (.cmxa)";
    "-client", Arg.Set client, " Preserve type equality with concrete node environment (used to embed protocol into the client)" ;
    "-I", Arg.String (fun s -> include_dirs := s :: !include_dirs), "path Path for concrete node signatures (used to embed protocol into the client)" ;
    "-bin-annot", Arg.Set Clflags.binary_annotations, " (see ocamlopt)" ;
    "-g", Arg.Set Clflags.debug, " (see ocamlopt)" ;
    "-build-dir", Arg.String (fun s -> build_dir := Some s), "path Reuse build dir (incremental compilation)"] in
  let usage_msg = Printf.sprintf "Usage: %s <out> <src>\nOptions are:" Sys.argv.(0) in
  Arg.parse args_spec (fun s -> anonymous := s :: !anonymous) usage_msg ;

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

  let hash, units = Meta.of_file source_dir in
  let hash = match hash with
    | Some hash -> hash
    | None -> Protocol.hash @@ List.map (Protocol.find_component source_dir) units
  in
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

  (* Compile the /ad-hoc/ Error_monad. *)
  List.iter (dump_cmi sigs_dir) tezos_protocol_env ;
  at_exit (fun () -> List.iter (unlink_cmi sigs_dir) tezos_protocol_env ) ;
  let local_environment_unit = "local_environment" in
  let local_environment_ml = build_dir // local_environment_unit ^ ".ml" in
  create_file local_environment_ml @@ Printf.sprintf {|
      module Environment = %s.Make(struct let name = %S end)()
    |}
    (if client then "Environment" else "Proto_environment")
    logname ;
  if not keep_object then
    at_exit (fun () ->
        safe_unlink local_environment_ml) ;
  let local_environment_object =
    compile_units
      ~ctxt
      ~for_pack:packname
      ~keep_object
      ~build_dir ~source_dir:build_dir [local_environment_unit]
  in

  Compenv.implicit_modules :=
    [ "Local_environment"; "Environment" ;
      "Error_monad" ; "Hash" ; "Logging" ];

  (* Compile the protocol *)
  let objects =
    compile_units
      ~ctxt
      ~update_needed
      ~keep_object ~for_pack:packname ~build_dir ~source_dir units in
  pack_objects ~ctxt ~keep_object
    packed_objects (local_environment_object @ objects) ;

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
  link_shared ~static:!static output [packed_objects; register_object]
