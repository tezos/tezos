open Internal_pervasives

module Operations = struct
  (* This is likely a temporary module, which will be obsoleted by a
     more general framework. *)
  type t =
    { mutable operations:
        [ `Bake of string * string * string list
        | `Endorse of string * string * string list
        | `Transfer of string * string * string * string list ]
        list }

  let make () = {operations= []}
  let from_state state : t = state#operations_log

  let show_all state =
    let t = from_state state in
    Console.Prompt.unit_and_loop
      EF.(af "Show all manual operations")
      ["ao"; "all-ops"; "all-operations"]
      (fun _ ->
        Console.say state
          EF.(
            desc_list (haf "Operations:")
              (List.rev_map t.operations ~f:(function
                | `Transfer (cli, msg, dest, res) ->
                    desc_list (haf "Transfer: %S" cli)
                      [ af "→ %s" msg; af "dest: %s" dest
                      ; ocaml_string_list res ]
                | `Endorse (n, msg, res) ->
                    desc_list
                      (haf "Node-endorsed: %S" n)
                      [af "→ %s" msg; ocaml_string_list res]
                | `Bake (n, msg, res) ->
                    desc_list (haf "Node-baked: %S" n)
                      [af "→ %s" msg; ocaml_string_list res] ))) )

  let bake state ~client ~output msg =
    let t = from_state state in
    t.operations <- `Bake (client, msg, output) :: t.operations

  let endorse state ~client ~output msg =
    let t = from_state state in
    t.operations <- `Endorse (client, msg, output) :: t.operations
end
