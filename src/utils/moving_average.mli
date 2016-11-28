(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Moving averages. The formulas are from Wikipedia
    [https://en.wikipedia.org/wiki/Moving_average] *)

class type ma = object
  method add_float : float -> unit
  method add_int : int -> unit
  method get : float
end
(** Common class type for objects computing a cumulative moving
    average of some flavor. In a cumulative moving average, the data
    arrive in an ordered datum stream, and the user would like to get
    the average of all of the data up until the current datum
    point. The method [add_float] and [add_int] are used to add the
    next datum. The method [get] and [get_exn] are used to compute the
    moving average up until the current datum point. *)

class sma : ?init:float -> unit -> ma
(** [sma ?init ()] is an object that computes the Simple Moving
    Average of a datum stream. [SMA(n+1) = SMA(n) + (x_(n+1) / SMA(n))
    / (n+1)] *)

class ema : ?init:float -> alpha:float -> unit -> ma
(** [ema ?init ~alpha ()] is an object that computes the Exponential
    Moving Average of a datum stream. [EMA(n+1) = alpha * x_(n+1) +
    (1 - alpha) * x_n] *)
