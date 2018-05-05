(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(***************** Prevalidation errors ***********************************)

type error += Parse_error
type error += Too_many_operations
type error += Oversized_operation of { size: int ; max: int }
type error += Future_block_header of Block_hash.t

let () =
  (* Parse error *)
  register_error_kind
    `Permanent
    ~id:"node.prevalidation.parse_error"
    ~title:"Parsing error in prevalidation"
    ~description:"Raised when an operation has not been parsed correctly during prevalidation."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Operation parsing error in prevalidation.")
    Data_encoding.empty
    (function Parse_error -> Some () | _ -> None)
    (fun () -> Parse_error) ;
  (* Too many operations *)
  register_error_kind
    `Temporary
    ~id:"node.prevalidation.too_many_operations"
    ~title:"Too many pending operations in prevalidation"
    ~description:"The prevalidation context is full."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Too many operations in prevalidation context.")
    Data_encoding.empty
    (function Too_many_operations -> Some () | _ -> None)
    (fun () -> Too_many_operations) ;
  (* Oversized operation *)
  register_error_kind
    `Permanent
    ~id:"node.prevalidation.oversized_operation"
    ~title:"Oversized operation"
    ~description:"The operation size is bigger than allowed."
    ~pp:(fun ppf (size, max) ->
        Format.fprintf ppf "Oversized operation (size: %d, max: %d)"
          size max)
    Data_encoding.(obj2
                     (req "size" int31)
                     (req "max_size" int31))
    (function Oversized_operation { size ; max } -> Some (size, max) | _ -> None)
    (fun (size, max) -> Oversized_operation { size ; max })


(************************* State errors ***********************************)

type error +=  Unknown_chain of Chain_id.t

type error += Bad_data_dir

type error += Block_not_invalid of Block_hash.t

let () =
  (* Unknown network *)
  register_error_kind
    `Permanent
    ~id:"node.state.unknown_chain"
    ~title:"Unknown chain"
    ~description:"The chain identifier could not be found in \
                  the chain identifiers table."
    ~pp:(fun ppf id ->
        Format.fprintf ppf "Unknown chain %a" Chain_id.pp id)
    Data_encoding.(obj1 (req "chain" Chain_id.encoding))
    (function Unknown_chain x -> Some x | _ -> None)
    (fun x -> Unknown_chain x) ;
  register_error_kind
    `Permanent
    ~id:"node.state.bad_data_dir"
    ~title:"Bad data directory"
    ~description:"The data directory could not be read. \
                  This could be because it was generated with an \
                  old version of the tezos-node program. \
                  Deleting and regenerating this directory \
                  may fix the problem."
    ~pp:(fun ppf () -> Format.fprintf ppf "Bad data directory.")
    Data_encoding.empty
    (function Bad_data_dir -> Some () | _ -> None)
    (fun () -> Bad_data_dir) ;
  (* Block not invalid *)
  register_error_kind
    `Permanent
    ~id:"node.state.block_not_invalid"
    ~title:"Block not invalid"
    ~description:"The invalid block to be unmarked was not actually invalid."
    ~pp:(fun ppf block ->
        Format.fprintf ppf "Block %a was expected to be invalid, but was not actually invalid."
          Block_hash.pp block)
    Data_encoding.(obj1 (req "block" Block_hash.encoding))
    (function Block_not_invalid block -> Some block | _ -> None)
    (fun block -> Block_not_invalid block)

(* Block database error *)

type error += Inconsistent_hash of Context_hash.t * Context_hash.t

let () =
  (* Inconsistent hash *)
  register_error_kind
    `Permanent
    ~id:"node.state.block.inconsistent_context_hash"
    ~title:"Inconsistent commit hash"
    ~description:
      "When commiting the context of a block, the announced context \
       hash was not the one computed at commit time."
    ~pp: (fun ppf (got, exp) ->
        Format.fprintf ppf
          "@[<v 2>Inconsistant hash:@ got: %a@ expected: %a"
          Context_hash.pp got
          Context_hash.pp exp)
    Data_encoding.(obj2
                     (req "wrong_context_hash" Context_hash.encoding)
                     (req "expected_context_hash" Context_hash.encoding))
    (function Inconsistent_hash (got, exp) -> Some (got, exp) | _ -> None)
    (fun (got, exp) -> Inconsistent_hash (got, exp))

(******************* Bootstrap pipeline errors ****************************)

type error += Invalid_locator of P2p_peer.Id.t * Block_locator.t

let () =
  (* Invalid locator *)
  register_error_kind
    `Permanent
    ~id:"node.bootstrap_pipeline.invalid_locator"
    ~title:"Invalid block locator"
    ~description:"Block locator is invalid."
    ~pp: (fun ppf (id, locator) ->
        Format.fprintf ppf
          "Invalid block locator on peer %a:\n%a"
          P2p_peer.Id.pp id
          Block_locator.pp locator)
    Data_encoding.(obj2
                     (req "id" P2p_peer.Id.encoding)
                     (req "locator" Block_locator.encoding))
    (function | Invalid_locator (id, loc) -> Some (id, loc) | _ -> None)
    (fun (id, loc) -> Invalid_locator (id, loc))

(******************* Protocol validator errors ****************************)

type protocol_error =
  | Compilation_failed
  | Dynlinking_failed

type error += Invalid_protocol of { hash: Protocol_hash.t ; error: protocol_error }

let protocol_error_encoding =
  let open Data_encoding in
  union
    [
      case (Tag 0)
        (obj1
           (req "error" (constant "compilation_failed")))
        (function Compilation_failed -> Some ()
                | _ -> None)
        (fun () -> Compilation_failed) ;
      case (Tag 1)
        (obj1
           (req "error" (constant "dynlinking_failed")))
        (function Dynlinking_failed -> Some ()
                | _ -> None)
        (fun () -> Dynlinking_failed) ;
    ]

let pp_protocol_error ppf = function
  | Compilation_failed ->
      Format.fprintf ppf "compilation error"
  | Dynlinking_failed ->
      Format.fprintf ppf "dynlinking error"

let () =
  (* Invalid protocol *)
  register_error_kind
    `Permanent
    ~id:"node.protocol_validator.invalid_protocol"
    ~title:"Invalid protocol"
    ~description:"Invalid protocol."
    ~pp:begin fun ppf (protocol, error) ->
      Format.fprintf ppf
        "@[<v 2>Invalid protocol %a@ %a@]"
        Protocol_hash.pp_short protocol pp_protocol_error error
    end
    Data_encoding.(merge_objs
                     (obj1 (req "invalid_protocol" Protocol_hash.encoding))
                     protocol_error_encoding)
    (function Invalid_protocol { hash ; error } ->
       Some (hash, error) | _ -> None)
    (fun (hash, error) ->
       Invalid_protocol { hash ; error })

(********************* Peer validator errors ******************************)

type error +=
  | Unknown_ancestor
  | Known_invalid

let () =
  (* Unknown ancestor *)
  register_error_kind
    `Permanent
    ~id: "node.peer_validator.unknown_ancestor"
    ~title: "Unknown ancestor"
    ~description: "Unknown ancestor block found in the peer's chain"
    ~pp: (fun ppf () -> Format.fprintf ppf "Unknown ancestor")
    Data_encoding.empty
    (function Unknown_ancestor -> Some () | _ -> None)
    (fun () -> Unknown_ancestor) ;
  (* Known invalid *)
  register_error_kind
    `Permanent
    ~id: "node.peer_validator.known_invalid"
    ~title: "Known invalid"
    ~description: "Known invalid block found in the peer's chain"
    ~pp: (fun ppf () -> Format.fprintf ppf "Known invalid")
    Data_encoding.empty
    (function Known_invalid -> Some () | _ -> None)
    (fun () -> Known_invalid)

(************************ Validator errors ********************************)

type error +=  Inactive_chain of Chain_id.t

let () =
  (* Inactive network *)
  register_error_kind
    `Branch
    ~id: "node.validator.inactive_chain"
    ~title: "Inactive chain"
    ~description: "Attempted validation of a block from an inactive chain."
    ~pp: (fun ppf chain ->
        Format.fprintf ppf
          "Tried to validate a block from chain %a, \
           that is not currently considered active."
          Chain_id.pp chain)
    Data_encoding.(obj1 (req "inactive_chain" Chain_id.encoding))
    (function Inactive_chain chain -> Some chain | _ -> None)
    (fun chain -> Inactive_chain chain)
