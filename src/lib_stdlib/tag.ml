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

type _ selector = ..

module type DEF_ARG = sig
  val name : string
  type t
  val doc : string
  val pp : Format.formatter -> t -> unit
end

module type DEF = sig
  include DEF_ARG

  type id
  val id: id
  type _ selector += Me : t selector

  val uid : int

end

module Def (X : DEF_ARG): DEF with type t = X.t = struct
  include X

  type id = Id
  let id = Id
  type _ selector += Me : t selector

  let uid = Obj.(extension_id @@ extension_constructor @@ Me)

end

type 'a def = (module DEF with type t = 'a)

let def (type a) ?(doc = "undocumented") name pp =
  (module Def(struct let name = name type t = a let doc = doc let pp = pp end): DEF with type t = a)

type (_,_) eq = Refl : ('a,'a) eq

let maybe_eq : type a b. a def -> b def -> (a,b) eq option =
  fun s t ->
    let module S = (val s) in
    let module T = (val t) in
    match S.Me with
    | T.Me -> Some Refl
    | _ -> None

let selector_of : type a. a def -> a selector = fun d -> let module D = (val d) in D.Me
let name : type a. a def -> string = fun d -> let module D = (val d) in D.name
let doc : type a. a def -> string = fun d -> let module D = (val d) in D.doc
let printer : type a. a def -> Format.formatter -> a -> unit = fun d -> let module D = (val d) in D.pp
let pp_def ppf d = Format.fprintf ppf "tag:%s" (name d)

module Key = struct
  type t = V : 'a def -> t
  type s = S : 'a selector -> s
  let compare (V k0) (V k1) = compare (S (selector_of k0)) (S (selector_of k1))
end

module TagSet = Map.Make(Key)

type t = V : 'a def * 'a -> t
type binding = t
type set = binding TagSet.t

let pp ppf (V (tag, v)) =
  Format.fprintf ppf "@[<1>(%a@ @[%a@])@]" pp_def tag (printer tag) v

let option_map f = function
  | None -> None
  | Some v -> Some (f v)

let option_bind f = function
  | None -> None
  | Some v -> f v

let reveal2 : type a b. a def -> b def -> b -> a option = fun t u v ->
  match maybe_eq t u with
  | None -> None
  | Some Refl -> Some v

let reveal : 'a. 'a def -> binding -> 'a option = fun tag -> function
  | V (another, v) -> reveal2 tag another v

let unveil : 'a. 'a def -> binding option -> 'a option = fun tag -> option_bind @@ reveal tag

let conceal : 'a. 'a def -> 'a -> binding = fun tag v -> V (tag, v)

let veil : 'a. 'a def -> 'a option -> binding option = fun tag -> option_map @@ conceal tag

let empty = TagSet.empty
let is_empty = TagSet.is_empty
let mem tag = TagSet.mem (Key.V tag)
let add tag v = TagSet.add (Key.V tag) (V (tag, v))
let update tag f = TagSet.update (Key.V tag) (fun b -> veil tag @@ f @@ unveil tag b)
let singleton tag v = TagSet.singleton (Key.V tag) (V (tag, v))
let remove tag = TagSet.remove (Key.V tag)
let rem = remove
type merger = { merger : 'a. 'a def -> 'a option -> 'a option -> 'a option }
let merge f = TagSet.merge @@ function
  | Key.V tag -> fun a b -> veil tag @@ f.merger tag (unveil tag a) (unveil tag b)
type unioner = { unioner : 'a . 'a def -> 'a -> 'a -> 'a }
let union f = merge { merger = fun tag a b ->
    match (a,b) with
    | (Some aa, Some bb) -> Some (f.unioner tag aa bb)
    | (Some _, None) -> a
    | (None, _) -> b
  }
(* no compare and equal, compare especially makes little sense *)
let iter f = TagSet.iter (fun _ -> f)
let fold f = TagSet.fold (fun _ -> f)
let for_all p = TagSet.for_all (fun _ -> p)
let exists p = TagSet.exists (fun _ -> p)
let filter p = TagSet.filter (fun _ -> p)
let partition p = TagSet.partition (fun _ -> p)
let cardinal = TagSet.cardinal
let bindings s = List.map snd @@ TagSet.bindings s
let min_binding s = snd @@ TagSet.min_binding s
let min_binding_opt s = option_map snd @@ TagSet.min_binding_opt s
let max_binding s = snd @@ TagSet.max_binding s
let max_binding_opt s = option_map snd @@ TagSet.max_binding_opt s
let choose s = snd @@ TagSet.choose s
let choose_opt s = option_map snd @@ TagSet.choose_opt s
let split tag s = (fun (l,m,r) -> (l,unveil tag m,r)) @@ TagSet.split (Key.V tag) s
(* In order to match the usual interface for maps, `find` should be different from
   `find_opt` but `Logs` has `find_opt` called `find` so we favor that. *)
let find tag s = option_bind (reveal tag) @@ TagSet.find_opt (Key.V tag) s
let find_opt tag s = option_bind (reveal tag) @@ TagSet.find_opt (Key.V tag) s
(* This would usually be called `find` but `Logs` has it with this name.  We can't
   have it at both named because `Logs` has `find_opt` as `find`. *)
let get tag s = find_opt tag s |> function
  | None -> invalid_arg (Format.asprintf "tag named %s not found in set" (name tag))
  | Some v -> v
let find_first p s = snd @@ TagSet.find_first p s
let find_first_opt p s = option_map snd @@ TagSet.find_first_opt p s
let find_last p s = snd @@ TagSet.find_last p s
let find_last_opt p s = option_map snd @@ TagSet.find_last_opt p s
let map = TagSet.map
let mapi = TagSet.map
let pp_set ppf s = Format.(
    fprintf ppf "@[<1>{";
    pp_print_list pp ppf (bindings s);
    Format.fprintf ppf "}@]")

module DSL = struct
  type (_,_,_,_) arg = | A : ('x def * 'x) -> (('b -> 'x -> 'c) -> 'x -> 'd, 'b, 'c, 'd) arg
                       | S : ('x def * 'x) -> ('x -> 'd, 'b, 'c, 'd) arg
                       | T : ('x def * 'x) -> ('d, 'b, 'c, 'd) arg
  let a tag v = A (tag,v)
  let s tag v = S (tag,v)
  let t tag v = T (tag,v)

  let pp_of_def (type a) tag = let module Tg = (val tag : DEF with type t = a) in Tg.pp

  let (-%): type a d. (?tags:set -> a) -> (a,Format.formatter,unit,d) arg -> (?tags:set -> d) = fun f -> function
    | A (tag,v) -> (fun ?(tags=empty) -> f ~tags:(add tag v tags) (pp_of_def tag) v) [@warning "-16"]
    | S (tag,v) -> (fun ?(tags=empty) -> f ~tags:(add tag v tags) v) [@warning "-16"]
    | T (tag,v) -> (fun ?(tags=empty) -> f ~tags:(add tag v tags)) [@warning "-16"]
end
