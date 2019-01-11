open Internal_pervasives

type t = {id: string; port: int; exec: [`Admin] Tezos_executable.t}

let base_dir t ~state = Paths.root state // sprintf "Admin-client-base-%s" t.id

let of_client ~exec n =
  let id = sprintf "A-%s" n.Tezos_client.id in
  let port = n.Tezos_client.port in
  {id; port; exec}

let of_node ~exec n =
  let id = sprintf "C-%s" n.Tezos_node.id in
  let port = n.Tezos_node.rpc_port in
  {id; port; exec}

let make_command t state args =
  let open Tezos_executable.Make_cli in
  Tezos_executable.call t.exec
    ~path:(base_dir t ~state // "exec-admin")
    (optf "port" "%d" t.port @ opt "base-dir" (base_dir ~state t) @ args)

module Command_error = struct
  type t = [`Admin_command_error of string * string list option]

  let failf ?args fmt =
    ksprintf (fun s -> fail (`Admin_command_error (s, args) : [> t])) fmt

  let pp fmt (`Admin_command_error (msg, args) : t) =
    Format.fprintf fmt "Admin-command-error:@ %s%s" msg
      (Option.value_map args ~default:"" ~f:(fun l ->
           sprintf " (args: %s)"
             (List.map ~f:(sprintf "%S") l |> String.concat ~sep:", ") ))
end

open Command_error

let successful_command admin state args =
  Running_processes.run_cmdf state "sh -c %s"
    ( make_command admin state args
    |> Genspio.Compile.to_one_liner |> Filename.quote )
  >>= fun res ->
  Console.display_errors_of_command state res
  >>= function
  | true -> return res
  | false ->
      failf ~args "Admin-command failure: %s" (String.concat ~sep:" " args)
