(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Tag = struct

  type t = string list

  let add tags t =
    t :: tags

  let remove tags t =
    let rec aux acc = function
      | [] -> raise Not_found
      | x :: ts when x = t -> (List.rev acc) @ ts
      | x :: ts -> aux (x :: acc) ts
    in
    aux [] tags

  let encoding =
    Data_encoding.(list string)

end

module type Entity = sig
  val name : string
end

module Tags (Entity : Entity) = struct

  include Client_aliases.Alias (struct

    type t = Tag.t

    let encoding = Tag.encoding

    (* Split a string of tags separated by commas, and possibly spaces *)
    let of_source _ tags_str =
      let rec aux tags s =
        try
          let idx = String.index s ',' in
          let tag = String.(trim (sub s 0 idx)) in
          let tail = String.(sub s (idx + 1) (length s - (idx + 1))) in
          aux (tag :: tags) tail
        with
        | Not_found ->
            String.(trim s) :: tags
      in
      return (aux [] tags_str)

    let to_source _ tags =
      return (String.concat ", " tags)

    let name = Entity.name ^ " tag"

  end)

  let tag_param ?(name = "tag") ?(desc = "list of tags") next =
    let desc =
      desc ^ "\n"
      ^ "can be one or multiple tags separated by commas" in
    Cli_entries.(
      param ~name ~desc
        (parameter (fun cctxt s -> of_source cctxt s))
        next)

  let rev_find_by_tag cctxt tag =
    load cctxt >>=? fun tags ->
    try return (Some (List.find (fun (_, v) -> List.mem tag v) tags |> fst))
    with Not_found -> return None

  let filter cctxt pred =
    load cctxt >>=? fun tags ->
    return (List.filter pred tags)

  let filter_by_tag cctxt tag =
    filter cctxt (fun (_, v) -> List.mem tag v)

end
