open Internal_pervasives

module Make_cli = struct
  let flag name = [sprintf "--%s" name]
  let opt name s = [sprintf "--%s" name; s]
  let optf name fmt = ksprintf (opt name) fmt
end

module Unix_files_sink = struct
  type t = {matches: string list option; level_at_least: string}

  let all_notices = {matches= None; level_at_least= "notice"}
  let all_info = {matches= None; level_at_least= "info"}
end

type kind = [`Node | `Baker | `Endorser | `Accuser | `Client | `Admin]

type 'kind t =
  { kind:
      'kind
      (* if needed, it's easy to remove this overengineered type parameter. *)
  ; binary: string option
  ; unix_files_sink: Unix_files_sink.t option
  ; environment: (string * string) list }

let node ?binary ?unix_files_sink ?(environment = []) () =
  {kind= `Node; binary; unix_files_sink; environment}

let kind_string (kind : [< kind]) =
  match kind with
  | `Accuser -> "accuser-alpha"
  | `Baker -> "baker-alpha"
  | `Endorser -> "endorser-alpha"
  | `Node -> "node"
  | `Client -> "client"
  | `Admin -> "admin-client"

let default_binary t = sprintf "tezos-%s" (kind_string t.kind)

let call (t : [< kind] t) ~path args =
  let open Genspio.EDSL in
  seq
    ( Option.value_map t.unix_files_sink ~default:[] ~f:(function
        | {matches= None; level_at_least} ->
            [ setenv
                ~var:(str "TEZOS_EVENTS_CONFIG")
                (ksprintf str "unix-files://%s?level-at-least=%s"
                   (path // "events") level_at_least) ]
        | _other -> assert false )
    @ [ exec ["mkdir"; "-p"; path]
      ; write_stdout
          ~path:(path // "last-cmd" |> str)
          (printf (str "ARGS: %s\\n") [str (String.concat ~sep:" " args)])
      ; exec (Option.value t.binary ~default:(default_binary t) :: args) ] )

let cli_term kind prefix =
  let open Cmdliner in
  let open Term in
  pure (fun binary ->
      { kind
      ; binary
      ; unix_files_sink= Some Unix_files_sink.all_info
      ; environment= [] } )
  $ Arg.(
      value
      & opt (some string) None
      & info
          [sprintf "%s-%s-binary" prefix (kind_string kind)]
          ~doc:(sprintf "Binary for the `tezos-%s` to use." (kind_string kind)))
