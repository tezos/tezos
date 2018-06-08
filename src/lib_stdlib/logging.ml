(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Tag = struct

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
  (* XXX find should be different from find_opt but Logs has find_opt called find *)
  let find tag s = option_bind (reveal tag) @@ TagSet.find_opt (Key.V tag) s
  let find_opt tag s = option_bind (reveal tag) @@ TagSet.find_opt (Key.V tag) s
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

end

type ('a, 'b) msgf =
  (('a, Format.formatter, unit, 'b) format4 -> ?tags:Tag.set -> 'a) -> ?tags:Tag.set -> 'b

type ('a, 'b) log = ('a, 'b) msgf -> 'b

module type MESSAGE = sig
  val name: string
end

type log_section = ..

type log_message = {
  section : log_section ;
  text : string ;
  tags : Tag.set ;
}

let taps : (log_message -> unit) list ref = ref []

let tap f = taps := f :: !taps

let call_taps v = List.iter (fun f -> f v) !taps

module type SEMLOG = sig

  type log_section += Section

  module Tag = Tag

  val debug: ('a, unit) log
  val log_info: ('a, unit) log
  val log_notice: ('a, unit) log
  val warn: ('a, unit) log
  val log_error: ('a, unit) log
  val fatal_error: ('a, unit) log

  val lwt_debug: ('a, unit Lwt.t) log
  val lwt_log_info: ('a, unit Lwt.t) log
  val lwt_log_notice: ('a, unit Lwt.t) log
  val lwt_warn: ('a, unit Lwt.t) log
  val lwt_log_error: ('a, unit Lwt.t) log
  val lwt_fatal_error: ('a, unit Lwt.t) log

  val event : string Tag.def
  val exn : exn Tag.def

end

let sections = ref []

let event = Tag.def ~doc:"String identifier for the class of event being logged" "event" Format.pp_print_text
let exn = Tag.def ~doc:"Exception which was detected" "exception" (fun f e -> Format.pp_print_text f (Printexc.to_string e))

module Make_semantic(S : MESSAGE) : SEMLOG = struct

  include S

  type log_section += Section

  module Tag = Tag

  let () = sections := S.name :: !sections
  let section = Lwt_log_core.Section.make S.name


  let log_f ~level =
    if level >= Lwt_log_core.Section.level section then
      fun format ?(tags=Tag.empty) ->
        Format.kasprintf
          (fun text ->
             call_taps { section = Section ; text ; tags };
             Lwt_log_core.log ~section ~level text)
          format
    else
      fun format ?(tags=Tag.empty) ->
        Format.ikfprintf
          (fun _ -> call_taps { section = Section ; text = "" ; tags }; Lwt.return_unit)
          Format.std_formatter
          format

  let ign_log_f ~level =
    if level >= Lwt_log_core.Section.level section then
      fun format ?(tags=Tag.empty) ->
        Format.kasprintf
          (fun text ->
             call_taps { section = Section ; text ; tags };
             Lwt_log_core.ign_log ~section ~level text)
          format
    else
      fun format ?(tags=Tag.empty) ->
        Format.ikfprintf
          (fun _ -> call_taps { section = Section ; text = "" ; tags })
          Format.std_formatter
          format

  let debug f = f (ign_log_f ~level:Lwt_log_core.Debug) ?tags:(Some Tag.empty)
  let log_info f = f (ign_log_f ~level:Lwt_log_core.Info) ?tags:(Some Tag.empty)
  let log_notice f = f (ign_log_f ~level:Lwt_log_core.Notice) ?tags:(Some Tag.empty)
  let warn f = f (ign_log_f ~level:Lwt_log_core.Warning) ?tags:(Some Tag.empty)
  let log_error f = f (ign_log_f ~level:Lwt_log_core.Error) ?tags:(Some Tag.empty)
  let fatal_error f = f (ign_log_f ~level:Lwt_log_core.Fatal) ?tags:(Some Tag.empty)

  let lwt_debug f = f (log_f ~level:Lwt_log_core.Debug) ?tags:(Some Tag.empty)
  let lwt_log_info f = f (log_f ~level:Lwt_log_core.Info) ?tags:(Some Tag.empty)
  let lwt_log_notice f = f (log_f ~level:Lwt_log_core.Notice) ?tags:(Some Tag.empty)
  let lwt_warn f = f (log_f ~level:Lwt_log_core.Warning) ?tags:(Some Tag.empty)
  let lwt_log_error f = f (log_f ~level:Lwt_log_core.Error) ?tags:(Some Tag.empty)
  let lwt_fatal_error f = f (log_f ~level:Lwt_log_core.Fatal) ?tags:(Some Tag.empty)

  let event = event
  let exn = exn

end

module type LOG = sig

  type log_section += Section

  val debug: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_info: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_notice: ('a, Format.formatter, unit, unit) format4 -> 'a
  val warn: ('a, Format.formatter, unit, unit) format4 -> 'a
  val log_error: ('a, Format.formatter, unit, unit) format4 -> 'a
  val fatal_error: ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_info: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_notice: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_warn: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_log_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
  val lwt_fatal_error: ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

end

let sections = ref []

module Make_unregistered(S : sig val name: string end) : LOG = struct

  let section = Lwt_log_core.Section.make S.name
  type log_section += Section

  let log_f
      ?exn ?(section = Lwt_log_core.Section.main) ?location ?logger ~level format =
    if level < Lwt_log_core.Section.level section then
      Format.ikfprintf (fun _ -> Lwt.return_unit) Format.std_formatter format
    else
      Format.kasprintf
        (fun msg ->
           call_taps { section = Section ; text = msg ; tags = Tag.empty };
           Lwt_log_core.log ?exn ~section ?location ?logger ~level msg)
        format

  let ign_log_f
      ?exn ?(section = Lwt_log_core.Section.main) ?location ?logger ~level format =
    if level < Lwt_log_core.Section.level section then
      Format.ikfprintf (fun _ -> ()) Format.std_formatter format
    else
      Format.kasprintf
        (fun msg ->
           call_taps { section = Section ; text = msg ; tags = Tag.empty };
           Lwt_log_core.ign_log ?exn ~section ?location ?logger ~level msg)
        format

  let debug fmt = ign_log_f ~section ~level:Lwt_log_core.Debug fmt
  let log_info fmt = ign_log_f ~section ~level:Lwt_log_core.Info fmt
  let log_notice fmt = ign_log_f ~section ~level:Lwt_log_core.Notice fmt
  let warn fmt = ign_log_f ~section ~level:Lwt_log_core.Warning fmt
  let log_error fmt = ign_log_f ~section ~level:Lwt_log_core.Error fmt
  let fatal_error fmt = ign_log_f ~section ~level:Lwt_log_core.Fatal fmt

  let lwt_debug fmt = log_f ~section ~level:Lwt_log_core.Debug fmt
  let lwt_log_info fmt = log_f ~section ~level:Lwt_log_core.Info fmt
  let lwt_log_notice fmt = log_f ~section ~level:Lwt_log_core.Notice fmt
  let lwt_warn fmt = log_f ~section ~level:Lwt_log_core.Warning fmt
  let lwt_log_error fmt = log_f ~section ~level:Lwt_log_core.Error fmt
  let lwt_fatal_error fmt = log_f ~section ~level:Lwt_log_core.Fatal fmt

end

module Make(S : sig val name: string end) : LOG = struct

  let () = sections := S.name :: !sections
  include Make_unregistered(S)

end

module Core = struct
  include Make_semantic(struct let name = "core" end)

  let worker = Tag.def ~doc:"Name of affected worker" "worker" Format.pp_print_text
end

type level = Lwt_log_core.level =
  | Debug
  (** Debugging message. They can be automatically removed by the
      syntax extension. *)
  | Info
  (** Informational message. Suitable to be displayed when the
      program is in verbose mode. *)
  | Notice
  (** Same as {!Info}, but is displayed by default. *)
  | Warning
  (** Something strange happend *)
  | Error
  (** An error message, which should not means the end of the
      program. *)
  | Fatal

type template = Lwt_log_core.template
let default_template = "$(date) - $(section): $(message)"
