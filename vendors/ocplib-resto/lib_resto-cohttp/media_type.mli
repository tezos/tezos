(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

module Make (Encoding : Resto.ENCODING) : sig

  type t = {
    name: Cohttp.Accept.media_range ;
    q: int option ;
    pp: 'a. 'a Encoding.t -> Format.formatter -> string -> unit ;
    construct: 'a. 'a Encoding.t -> 'a -> string ;
    destruct: 'a. 'a Encoding.t -> string -> ('a, string) result ;
  }

  val name: t -> string

  val has_complete_media: t list -> bool
  val first_complete_media: t list -> ((string * string) * t) option

  val find_media: (string * string) -> t list -> t option

  val resolve_accept_header: t list -> string option -> (string * t) option

  val accept_header: t list -> string
  val acceptable_encoding: t list -> string

end
