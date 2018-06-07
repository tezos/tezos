(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Raw_store.t
type global_store = t

(**************************************************************************
 * Net store under "chain/"
 **************************************************************************)

module Chain = struct

  type store = global_store * Chain_id.t
  let get s id = (s, id)

  module Indexed_store =
    Store_helpers.Make_indexed_substore
      (Store_helpers.Make_substore(Raw_store)(struct let name = ["chain"] end))
      (Chain_id)

  let destroy = Indexed_store.remove_all
  let list t =
    Indexed_store.fold_indexes t ~init:[]
      ~f:(fun h acc -> Lwt.return (h :: acc))

  module Genesis_hash =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["genesis" ; "hash"] end)
      (Store_helpers.Make_value(Block_hash))

  module Genesis_time =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["genesis" ; "time"] end)
      (Store_helpers.Make_value(Time))

  module Genesis_protocol =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["genesis" ; "protocol"] end)
      (Store_helpers.Make_value(Protocol_hash))

  module Genesis_test_protocol =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["genesis" ; "test_protocol"] end)
      (Store_helpers.Make_value(Protocol_hash))

  module Expiration =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["expiration"] end)
      (Store_helpers.Make_value(Time))

  module Allow_forked_chain =
    Indexed_store.Make_set (struct let name = ["allow_forked_chain"] end)

end

(**************************************************************************
 * Block_header store under "chain/<id>/blocks/"
 **************************************************************************)

module Block = struct

  type store = Chain.store
  let get x = x

  module Indexed_store =
    Store_helpers.Make_indexed_substore
      (Store_helpers.Make_substore
         (Chain.Indexed_store.Store)
         (struct let name = ["blocks"] end))
      (Block_hash)

  type contents = {
    header: Block_header.t ;
    message: string option ;
    max_operations_ttl: int ;
    max_operation_data_length: int;
    context: Context_hash.t ;
    metadata: MBytes.t ;
  }

  module Contents =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["contents"] end)
      (Store_helpers.Make_value(struct
         type t = contents
         let encoding =
           let open Data_encoding in
           conv
             (fun { header ; message ; max_operations_ttl ;
                    max_operation_data_length ; context ; metadata } ->
               (message, max_operations_ttl,
                max_operation_data_length, context, metadata, header ))
             (fun (message, max_operations_ttl,
                   max_operation_data_length, context, metadata, header ) ->
               { header ; message ; max_operations_ttl ;
                 max_operation_data_length ;
                 context ; metadata })
             (obj6
                (opt "message" string)
                (req "max_operations_ttl" uint16)
                (req "max_operation_data_length" uint16)
                (req "context" Context_hash.encoding)
                (req "metadata" bytes)
                (req "header" Block_header.encoding))
       end))

  module Operations_index =
    Store_helpers.Make_indexed_substore
      (Store_helpers.Make_substore
         (Indexed_store.Store)
         (struct let name = ["operations"] end))
      (Store_helpers.Integer_index)

  module Operation_hashes =
    Operations_index.Make_map
      (struct let name = ["hashes"] end)
      (Store_helpers.Make_value(struct
         type t = Operation_hash.t list
         let encoding = Data_encoding.list Operation_hash.encoding
       end))

  module Operation_path =
    Operations_index.Make_map
      (struct let name = ["path"] end)
      (Store_helpers.Make_value(struct
         type t = Operation_list_list_hash.path
         let encoding = Operation_list_list_hash.path_encoding
       end))

  module Operations =
    Operations_index.Make_map
      (struct let name = ["contents"] end)
      (Store_helpers.Make_value(struct
         type t = Operation.t list
         let encoding = Data_encoding.(list (dynamic_size Operation.encoding))
       end))

  module Operations_metadata =
    Operations_index.Make_map
      (struct let name = ["metadata"] end)
      (Store_helpers.Make_value(struct
         type t = MBytes.t list
         let encoding = Data_encoding.(list bytes)
       end))

  type invalid_block = {
    level: int32 ;
    errors: Error_monad.error list ;
  }

  module Invalid_block =
    Store_helpers.Make_map
      (Store_helpers.Make_substore
         (Chain.Indexed_store.Store)
         (struct let name = ["invalid_blocks"] end))
      (Block_hash)
      (Store_helpers.Make_value(struct
         type t = invalid_block
         let encoding =
           let open Data_encoding in
           conv
             (fun { level ; errors } -> (level, errors))
             (fun (level, errors) -> { level ; errors })
             (tup2 int32 (list Error_monad.error_encoding))
       end))

  let register s =
    Base58.register_resolver Block_hash.b58check_encoding begin fun str ->
      let pstr = Block_hash.prefix_path str in
      Chain.Indexed_store.fold_indexes s ~init:[]
        ~f:begin fun chain acc ->
          Indexed_store.resolve_index (s, chain) pstr >>= fun l ->
          Lwt.return (List.rev_append l acc)
        end
    end

  module Predecessors =
    Store_helpers.Make_map
      (Store_helpers.Make_substore
         (Indexed_store.Store)
         (struct let name = ["predecessors"] end))
      (Store_helpers.Integer_index)
      (Store_helpers.Make_value(Block_hash))

end


(**************************************************************************
 * Blockchain data
 **************************************************************************)

module Chain_data = struct

  type store = Chain.store
  let get s = s

  module Known_heads =
    Store_helpers.Make_buffered_set
      (Store_helpers.Make_substore
         (Chain.Indexed_store.Store)
         (struct let name = ["known_heads"] end))
      (Block_hash)
      (Block_hash.Set)

  module Current_head =
    Store_helpers.Make_single_store
      (Chain.Indexed_store.Store)
      (struct let name = ["current_head"] end)
      (Store_helpers.Make_value(Block_hash))

  module In_main_branch =
    Store_helpers.Make_single_store
      (Block.Indexed_store.Store)
      (struct let name = ["in_chain"] end)
      (Store_helpers.Make_value(Block_hash)) (* successor *)

end


(**************************************************************************
 * Protocol store under "protocols/"
 **************************************************************************)

module Protocol = struct

  type store = global_store
  let get x = x

  module Indexed_store =
    Store_helpers.Make_indexed_substore
      (Store_helpers.Make_substore
         (Raw_store)
         (struct let name = ["protocols"] end))
      (Protocol_hash)

  module Contents =
    Indexed_store.Make_map
      (struct let name = ["contents"] end)
      (Store_helpers.Make_value(Protocol))

  module RawContents =
    Store_helpers.Make_single_store
      (Indexed_store.Store)
      (struct let name = ["contents"] end)
      (Store_helpers.Raw_value)

  let register s =
    Base58.register_resolver Protocol_hash.b58check_encoding begin fun str ->
      let pstr = Protocol_hash.prefix_path str in
      Indexed_store.resolve_index s pstr
    end

end

let init dir =
  Raw_store.init dir >>=? fun s ->
  Block.register s ;
  Protocol.register s ;
  return s

let close = Raw_store.close
