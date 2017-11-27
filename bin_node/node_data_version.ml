(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = string

let data_version = "0.0.1"

let default_identity_file_name = "identity.json"

let version_encoding = Data_encoding.(obj1 (req "version" string))

let version_file_name = "version.json"

let pp ppf version = Format.pp_print_string ppf version

type error += Invalid_data_dir_version of t * t
type error += No_data_dir_version_file of string
type error += Could_not_read_data_dir_version of string

let () =
  register_error_kind
    `Permanent
    ~id: "invalidDataDirVersion"
    ~title: "Invalid data directory version"
    ~description: "The data directory version was not the one that was expected"
    Data_encoding.(obj2
                     (req "expectedVersion" string)
                     (req "actualVersion" string))
    (function
      | Invalid_data_dir_version (expected, actual) ->
          Some (expected, actual)
      | _ -> None)
    (fun (expected, actual) -> Invalid_data_dir_version (expected, actual)) ;
  register_error_kind
    `Permanent
    ~id: "couldNotReadDataDirVersion"
    ~title: "Could not read data directory version file"
    ~description: "Data directory version file was invalid."
    Data_encoding.(obj1 (req "versionPath" string))
    ~pp:(fun ppf path ->
        Format.fprintf ppf
          "Tried to read version file at '%s', \
          \ but the file could not be parsed."
          path)
    (function Could_not_read_data_dir_version path -> Some path | _ -> None)
    (fun path -> Could_not_read_data_dir_version path);
  register_error_kind
    `Permanent
    ~id: "noDataDirVersionFile"
    ~title: "Data directory version file does not exist"
    ~description: "Data directory version file does not exist"
    Data_encoding.(obj1 (req "versionPath" string))
    ~pp:(fun ppf path ->
        Format.fprintf ppf
          "Expected to find data directory version file at '%s', \
          \ but the file does not exist."
          path)
    (function No_data_dir_version_file path -> Some path | _ -> None)
    (fun path -> No_data_dir_version_file path)

let version_file data_dir =
  (Filename.concat data_dir version_file_name)

let check_data_dir_version data_dir =
  let version_file = version_file data_dir in
  fail_unless (Sys.file_exists version_file)
    (No_data_dir_version_file version_file) >>=? fun () ->
  Data_encoding_ezjsonm.read_file version_file
  |> trace (Could_not_read_data_dir_version version_file) >>=? fun json ->
  begin
    try return (Data_encoding.Json.destruct version_encoding json)
    with _ -> fail (Could_not_read_data_dir_version version_file)
  end >>=? fun version ->
  fail_unless
    (String.equal data_version version)
    (Invalid_data_dir_version (data_version, version)) >>=? fun () ->
  return ()

let ensure_data_dir data_dir =
  let write_version () =
    Data_encoding_ezjsonm.write_file
      (version_file data_dir)
      (Data_encoding.Json.construct version_encoding data_version) in
  if Sys.file_exists data_dir then
    match Sys.readdir data_dir with
    | [||] -> write_version ()
    | [| single |] when single = default_identity_file_name -> write_version ()
    | _ -> check_data_dir_version data_dir
  else
    Lwt_utils.create_dir ~perm:0o700 data_dir >>= fun () ->
    write_version ()
