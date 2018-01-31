(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Param : sig val name: string end)() = struct

  include Tezos_base.Protocol_environment.Make(Param)()

  module Updater = struct
    include Fake_updater.Make(Fake_context)
    module type PROTOCOL =
      RAW_PROTOCOL with type error := Error_monad.error
                    and type 'a tzresult := 'a Error_monad.tzresult
  end
  module Base58 = struct
    include Base58
    let simple_encode enc s = simple_encode enc s
    let simple_decode enc s = simple_decode enc s
    include Make(struct type context = Fake_context.t end)
    let decode s = decode s
  end
  module Context = struct
    include Fake_context
    let register_resolver = Base58.register_resolver
    let complete ctxt s = Base58.complete ctxt s
  end

end
