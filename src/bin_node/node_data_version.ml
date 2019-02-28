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

let (//) = Filename.concat

type t = string

let data_version = "0.0.2"

(* List of upgrade functions from each still supported previous
   version to the current [data_version] above. If this list grows too
   much, an idea would be to have triples (version, version,
   converter), and to sequence them dynamically instead of
   statically. *)
let upgradable_data_version = [
  "0.0.1", begin fun ~store_root ~context_root:_ ~protocol_root:_ ->
    State.upgrade_0_0_1 ~store_root ()
  end ;
]

let store_dir data_dir = data_dir // "store"
let context_dir data_dir = data_dir // "context"
let protocol_dir data_dir = data_dir // "protocol"
let lock_file data_dir = data_dir // "lock"

let default_identity_file_name = "identity.json"

let version_encoding = Data_encoding.(obj1 (req "version" string))

let version_file_name = "version.json"

let pp ppf version = Format.pp_print_string ppf version

type error += Invalid_data_dir_version of t * t
type error += Invalid_data_dir of string
type error += No_data_dir_version_file of string
type error += Could_not_read_data_dir_version of string
type error += Data_dir_needs_upgrade of { expected: t ; actual: t }

let () =
  register_error_kind
    `Permanent
    ~id: "invalidDataDir"
    ~title: "Invalid data directory"
    ~description: "The data directory cannot be accessed or created"
    ~pp:(fun ppf path ->
        Format.fprintf ppf
          "Invalid data directory '%s'."
          path)
    Data_encoding.(obj1 (req "datadir_path" string))
    (function
      | Invalid_data_dir path ->
          Some path
      | _ -> None)
    (fun path -> Invalid_data_dir path) ;
  register_error_kind
    `Permanent
    ~id: "invalidDataDirVersion"
    ~title: "Invalid data directory version"
    ~description: "The data directory version was not the one that was expected"
    ~pp:(fun ppf (exp, got) ->
        Format.fprintf ppf
          "Invalid data directory version '%s' (expected '%s')."
          got exp)
    Data_encoding.(obj2
                     (req "expected_version" string)
                     (req "actual_version" string))
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
    Data_encoding.(obj1 (req "version_path" string))
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
    Data_encoding.(obj1 (req "version_path" string))
    ~pp:(fun ppf path ->
        Format.fprintf ppf
          "Expected to find data directory version file at '%s', \
          \ but the file does not exist."
          path)
    (function No_data_dir_version_file path -> Some path | _ -> None)
    (fun path -> No_data_dir_version_file path) ;
  register_error_kind
    `Permanent
    ~id: "dataDirNeedsUpgrade"
    ~title: "The data directory needs to be upgraded"
    ~description: "The data directory needs to be upgraded"
    ~pp:(fun ppf (exp, got) ->
        Format.fprintf ppf
          "The data directory version is too old.@,\
           Found '%s', expected '%s'.@,\
           It needs to be upgraded with `tezos-node upgrade_storage`."
          got exp)
    Data_encoding.(obj2
                     (req "expected_version" string)
                     (req "actual_version" string))
    (function
      | Data_dir_needs_upgrade { expected ; actual } ->
          Some (expected, actual)
      | _ -> None)
    (fun (expected, actual) -> Data_dir_needs_upgrade { expected ; actual })

let version_file data_dir =
  (Filename.concat data_dir version_file_name)

let check_data_dir_version data_dir =
  let version_file = version_file data_dir in
  fail_unless (Sys.file_exists version_file)
    (No_data_dir_version_file version_file) >>=? fun () ->
  Lwt_utils_unix.Json.read_file version_file
  |> trace (Could_not_read_data_dir_version version_file) >>=? fun json ->
  begin
    try return (Data_encoding.Json.destruct version_encoding json)
    with _ -> fail (Could_not_read_data_dir_version version_file)
  end >>=? fun version ->
  if String.equal version data_version then
    return_none
  else
    match
      List.find_opt (fun (v, _) -> String.equal v version) upgradable_data_version
    with
    | Some f -> return_some f
    | None ->
        fail (Invalid_data_dir_version (data_version, version))

let write_version data_dir =
  Lwt_utils_unix.Json.write_file
    (version_file data_dir)
    (Data_encoding.Json.construct version_encoding data_version)

let ensure_data_dir bare data_dir  =
  let write_version () = write_version data_dir >>=? fun () -> return_none in
  try if Sys.file_exists data_dir then
      match Sys.readdir data_dir with
      | [||] -> write_version ()
      | [| single |] when single = default_identity_file_name -> write_version ()
      | _ when bare ->
          fail (Invalid_data_dir
                  (Format.asprintf
                     "Please provide a clean directory (only %s is allowed)."
                     default_identity_file_name))
      | _ -> check_data_dir_version data_dir
    else begin
      Lwt_utils_unix.create_dir ~perm:0o700 data_dir >>= fun () ->
      write_version ()
    end
  with Sys_error _ | Unix.Unix_error _ ->
    fail (Invalid_data_dir data_dir)

let upgrade_data_dir data_dir =
  ensure_data_dir false data_dir >>=? function
  | None ->
      Format.printf "Node data dir is up-to-date.@." ;
      return_unit
  | Some (version, upgrade) ->
      Format.printf
        "Upgrading node data dir from %s to %s...@."
        version data_version ;
      upgrade
        ~store_root:(store_dir data_dir)
        ~context_root:(context_dir data_dir)
        ~protocol_root:(protocol_dir data_dir) >>=? fun () ->
      write_version data_dir >>=? fun () ->
      Format.printf "The node data dir is now up-to-date!@." ;
      write_version data_dir

let ensure_data_dir ?(bare = false ) data_dir =
  ensure_data_dir bare data_dir >>=? function
  | None -> return_unit
  | Some (version, _) ->
      fail (Data_dir_needs_upgrade { expected = data_version ;
                                     actual = version })
