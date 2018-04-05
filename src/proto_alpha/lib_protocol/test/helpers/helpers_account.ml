(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open Proto_alpha.Error_monad
open Proto_alpha.Alpha_context

type account = {
  hpub : Signature.Public_key_hash.t ;
  pub :  Signature.Public_key.t ;
  ppk :  Signature.Secret_key.t ;
  contract : Contract.contract
}
type t = account

let bootstrap_accounts =
  let pubs = [
    "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
    "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9";
    "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV";
    "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU";
    "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n";
  ] in
  let ppks = [
    "edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9\
     rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi";
    "edskRkJz4Rw2rM5NtabEWMbbg2bF4b1nfFajaqEuEk4SgU7eeDby\
     m9gVQtBTbYo32WUg2zb5sNBkD1whRN7zX43V9bftBbtaKc";
    "edskS3qsqsNgdjUqeMsVcEwBn8dkZ5iDRz6aF21KhcCtRiAkWByp\
     USbicccR4Vgqm9UdW2Vabuos6seezqgbXTrmcbLUG4rdAC";
    "edskRg9qcPqaVQa6jXWNMU5p71tseSuR7NzozgqZ9URsVDi81wTyP\
     JdFSBdeakobyHUi4Xgu61jgKRQvkhXrPmEdEUfiqfiJFL";
    "edskS7rLN2Df3nbS1EYvwJbWo4umD7yPM1SUeX7gp1WhCVpMFXjcC\
     yM58xs6xsnTsVqHQmJQ2RxoAjJGedWfvFmjQy6etA3dgZ";
  ] in
  let pubs = List.map Signature.Public_key.of_b58check_exn pubs in
  let ppks = List.map Signature.Secret_key.of_b58check_exn ppks in
  let keys = List.combine pubs ppks in
  let aux (pub, ppk) : account =
    let hpub = Signature.Public_key.hash pub in {
      pub ;
      ppk ;
      hpub ;
      contract = Contract.implicit_contract hpub
    }
  in List.map aux keys

let new_account () : account =
  let (hpub, pub, ppk) = Signature.generate_key () in
  let contract = Contract.implicit_contract hpub in
  {hpub ; pub ; ppk ; contract}

let init_amount = 10000

let init_account ~(tc : context) account =
  Contract.credit
    tc
    account.contract
  @@ Helpers_cast.tez_of_int init_amount
  >>=? fun context -> return (account, context)

let make_account ~(tc : context) =
  let account = new_account () in
  init_account ~tc account

let make_accounts ~(tc : context) n =
  let rec aux tc n acc =
    if (n = 0) then
      return (acc, tc)
    else
      make_account ~tc >>=? fun (account, tc) ->
      aux tc (n - 1) @@ account :: acc
  in
  aux tc n []

let make_2_accounts ~(tc : context) =
  make_account ~tc >>=? fun (src, tc) ->
  make_account ~tc >>=? fun (dst, tc) ->
  return ((src, dst), tc)

let make_4_accounts ~(tc : context) =
  make_account ~tc >>=? fun (a, tc) ->
  make_account ~tc >>=? fun (b, tc) ->
  make_account ~tc >>=? fun (c, tc) ->
  make_account ~tc >>=? fun (d, tc) ->
  return ((a, b, c, d), tc)

let display_account ~tc account =
  Contract.get_balance tc account.contract >>= function
  | Ok balance -> (
      Helpers_logger.lwt_debug
        "Account %a : (%a tz)"
        Signature.Public_key_hash.pp account.hpub
        Tez.pp balance
    )| Error _ -> Helpers_logger.lwt_debug "Error in balance"

let display_accounts ~tc accounts =
  Helpers_logger.lwt_debug "Got accounts" >>= fun () ->
  Lwt_list.iter_s (display_account ~tc) accounts
