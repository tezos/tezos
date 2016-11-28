(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

class type ma = object
  method add_float : float -> unit
  method add_int : int -> unit
  method get : float
end

class virtual base ?(init = 0.) () = object (self)
  val mutable acc : float = init
  method virtual add_float : float -> unit
  method add_int x = self#add_float (float_of_int x)
  method get = acc
end

class sma ?init () = object
  inherit base ?init ()
  val mutable i = match init with None -> 0 | _ -> 1
  method add_float x =
    acc <- (acc +. (x -. acc) /. (float_of_int @@ succ i)) ;
    i <- succ i
end

class ema ?init ~alpha () = object
  inherit base ?init ()
  val alpha = alpha
  method add_float x =
      acc <- alpha *. x +. (1. -. alpha) *. acc
end

