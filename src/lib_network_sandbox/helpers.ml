open Internal_pervasives
open Console

let dump_connection =
  let open EF in
  let open Tezos_node in
  function
  | `Duplex (na, nb) ->
      af "%s:%d <-> %s:%d" na.id na.p2p_port nb.id nb.p2p_port
  | `From_to (na, nb) ->
      haf "%s:%d --> %s:%d" na.id na.p2p_port nb.id nb.p2p_port
  | `Missing (na, int) ->
      ksprintf shout "%s:%d --> ???:%d" na.id na.p2p_port int

let dump_connections state nodes =
  let conns = Tezos_node.connections nodes in
  say state
    (let open EF in
    desc_list (haf "Connections:") (List.map conns ~f:dump_connection))

let clear_root state =
  let root = Paths.(root state) in
  Lwt_exception.catch
    (fun () -> ksprintf Lwt_unix.system "rm -fr %s" (Filename.quote root))
    ()
  >>= function
  | Unix.WEXITED 0 -> return ()
  | _ -> System_error.fail "cannot delete root path (%S)" root

let wait_for state ~attempts ~seconds f =
  let rec attempt nth =
    let again () = attempt (nth + 1) in
    f nth
    >>= function
    | `Done x -> return x
    | `Not_done msg when nth < attempts ->
        say state
          EF.(
            wf "%s: attempt %d/%d, sleeping %.02f seconds" msg nth attempts
              seconds)
        >>= fun () -> System.sleep seconds >>= fun () -> again ()
    | `Not_done msg -> fail (`Waiting_for (msg, `Time_out))
  in
  attempt 1

let kill_node state nod =
  Running_processes.find_process_by_id ~only_running:true state
    ~f:(( = ) nod.Tezos_node.id)
  >>= fun states ->
  ( match states with
  | [one] -> return one
  | _ -> System_error.fail "Expecting one state for node %s" nod.Tezos_node.id
  )
  >>= fun node_state_0 -> Running_processes.kill state node_state_0

let restart_node ~client_exec state nod =
  Running_processes.start state (Tezos_node.process state nod)
  >>= fun _ ->
  let client = Tezos_client.of_node nod ~exec:client_exec in
  say state
    EF.(wf "Started node %s, waiting for bootstrap …" nod.Tezos_node.id)
  >>= fun () -> Tezos_client.bootstrapped client ~state

module Counter_log = struct
  type t = (string * int) list ref

  let create () = ref []
  let add t s n = t := (s, n) :: !t
  let incr t s = add t s 1
  let sum t = List.fold !t ~init:0 ~f:(fun prev (_, s) -> prev + s)

  let to_table_string t =
    let total = "**Total:**" in
    let longest =
      List.fold !t ~init:total ~f:(fun p (n, _) ->
          if String.length p < String.length n then n else p )
    in
    List.rev_map
      ((total, sum t) :: !t)
      ~f:(fun (cmt, n) ->
        sprintf "| %s %s|% 8d|" cmt
          (String.make (String.length longest - String.length cmt + 2) '.')
          n )
    |> String.concat ~sep:"\n"
end
