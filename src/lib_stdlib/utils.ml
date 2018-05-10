(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Infix = struct

  let (<<) g f = fun a -> g (f a)

  let (--) i j =
    let rec loop acc j =
      if j < i then acc else loop (j :: acc) (pred j) in
    loop [] j

end

let nbsp = Re.(compile (str "\xC2\xA0"))
let display_paragraph ppf description =
  Format.fprintf ppf "@[%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline
       (fun ppf line ->
          Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf w ->
               (* replace &nbsp; by real spaces... *)
               Format.fprintf ppf "%s@ "
                 (Re.replace ~all:true nbsp ~f:(fun _ -> " ") w))
            ppf
            (TzString.split ' ' line)))
    (TzString.split ~dup:false '\n' description)

let finalize f g = try let res = f () in g (); res with exn -> g (); raise exn
