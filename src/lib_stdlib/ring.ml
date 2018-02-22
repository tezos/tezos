(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ring = struct
  type 'a raw =
    | Empty of int
    | Inited of {
        data : 'a array ;
        mutable pos : int ;
      }

  type 'a t = 'a raw ref

  let create size = ref (Empty size)

  let add r v =
    match !r with
    | Empty size ->
        r := Inited { data = Array.make size v ; pos = 0 }
    | Inited s ->
        s.pos <-
          if s.pos = 2 * Array.length s.data - 1 then
            Array.length s.data
          else
            s.pos + 1 ;
        s.data.(s.pos mod Array.length s.data) <- v

  let add_and_return_erased r v =
    let replaced = match !r with
      | Empty _ -> None
      | Inited s ->
          if s.pos >= Array.length s.data - 1 then
            Some (s.data.(s.pos mod Array.length s.data))
          else
            None in
    add r v ; replaced

  let clear r =
    match !r with
    | Empty _ -> ()
    | Inited { data ; _ } ->
        r := Empty (Array.length data)


  let add_list r l = List.iter (add r) l

  let last r =
    match !r with
    | Empty _ -> None
    | Inited { data ; pos } -> Some data.(pos mod Array.length data)

  let fold r ~init ~f =
    match !r with
    | Empty _ -> init
    | Inited { data ; pos } ->
        let size = Array.length data in
        let acc = ref init in
        for i = 0 to min pos (size - 1) do
          acc := f !acc data.((pos - i) mod size)
        done ;
        !acc

  let elements t =
    fold t ~init:[] ~f:(fun acc elt -> elt :: acc)

  exception Empty

  let last_exn r =
    match last r with
    | None -> raise Empty
    | Some d -> d
end

include Ring

(** Ring Buffer Table *)
module type TABLE = sig
  type t
  type v
  val create : int -> t
  val clear : t -> unit
  val add : t -> v -> unit
  val mem : t -> v -> bool
  val remove : t -> v -> unit
  val elements : t -> v list
end


(* fixed size set of Peers id. If the set exceed the maximal allowed capacity, the
   element that was added first is removed when a new one is added *)
module MakeTable (V: Hashtbl.HashedType) = struct
  module Table = Hashtbl.Make(V)

  type raw = {
    size : int ;
    mutable capacity : int ;
    ring : V.t Ring.t ;
    table : unit Table.t ;
  }

  type t = raw ref
  type v = V.t

  let create size = ref {
      size;
      capacity = 0;
      ring = Ring.create size;
      table = Table.create size }

  let add ({contents = t } as tt) v =
    assert (t.capacity <= t.size);
    if t.capacity = t.size then begin
      try Table.remove t.table (Ring.last_exn t.ring) with Ring.Empty -> assert false
    end;
    Ring.add t.ring v;
    Table.replace t.table v ();
    let capacity = if t.capacity = t.size then t.capacity else t.capacity + 1 in
    tt := { ring = t.ring ; table = t.table; size = t.size ; capacity = capacity }

  let mem t v = Table.mem !t.table v

  let remove ({contents = t } as tt) v =
    Table.remove t.table v;
    let capacity = if t.capacity = 0 then t.capacity else t.capacity - 1 in
    tt := { ring = t.ring ; table = t.table; size = t.size ; capacity = capacity }

  let clear ({contents = t } as tt) =
    tt := { t with
            capacity = 0;
            ring = Ring.create t.size;
            table = Table.create t.size
          }

  let elements t = Ring.elements !t.ring

end
