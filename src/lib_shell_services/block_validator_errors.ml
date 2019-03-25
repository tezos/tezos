(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

type block_error =
  | Cannot_parse_operation of Operation_hash.t
  | Invalid_fitness of { expected: Fitness.t ; found: Fitness.t }
  | Non_increasing_timestamp
  | Non_increasing_fitness
  | Invalid_level of { expected: Int32.t ; found: Int32.t }
  | Invalid_proto_level of { expected: int ; found: int }
  | Replayed_operation of Operation_hash.t
  | Outdated_operation of
      { operation: Operation_hash.t;
        originating_block: Block_hash.t }
  | Expired_chain of
      { chain_id: Chain_id.t ;
        expiration: Time.Protocol.t ;
        timestamp: Time.Protocol.t ;
      }
  | Unexpected_number_of_validation_passes of int (* uint8 *)
  | Too_many_operations of { pass: int; found: int; max: int }
  | Oversized_operation of { operation: Operation_hash.t;
                             size: int; max: int }
  | Unallowed_pass of { operation: Operation_hash.t ;
                        pass: int ;
                        allowed_pass: int list }
  | Cannot_parse_block_header

let errno : Unix.error Data_encoding.t =
  let open Data_encoding in
  union [
    case
      ~title:"unknown_unix_error"
      (Tag 0) int8
      (function Unix.EUNKNOWNERR i -> Some i | _ -> None)
      (fun i -> EUNKNOWNERR i) ;
    case
      ~title:"unix_error"
      (Tag 1)
      (string_enum
         Unix.[
           "2big", E2BIG ;
           "acces", EACCES ;
           "again", EAGAIN ;
           "badf", EBADF ;
           "busy", EBUSY ;
           "child", ECHILD ;
           "deadlk", EDEADLK ;
           "dom", EDOM ;
           "exist", EEXIST ;
           "fault", EFAULT ;
           "fbig", EFBIG ;
           "intr", EINTR ;
           "inval", EINVAL ;
           "io", EIO ;
           "isdir", EISDIR ;
           "mfile", EMFILE ;
           "mlink", EMLINK ;
           "nametoolong", ENAMETOOLONG ;
           "nfile", ENFILE ;
           "nodev", ENODEV ;
           "noent", ENOENT ;
           "noexec", ENOEXEC ;
           "nolck", ENOLCK ;
           "nomem", ENOMEM ;
           "nospc", ENOSPC ;
           "nosys", ENOSYS ;
           "notdir", ENOTDIR ;
           "notempty", ENOTEMPTY ;
           "notty", ENOTTY ;
           "nxio", ENXIO ;
           "perm", EPERM ;
           "pipe", EPIPE ;
           "range", ERANGE ;
           "rofs", EROFS ;
           "spipe", ESPIPE ;
           "srch", ESRCH ;
           "xdev", EXDEV ;
           "wouldblock", EWOULDBLOCK ;
           "inprogress", EINPROGRESS ;
           "already", EALREADY ;
           "notsock", ENOTSOCK ;
           "destaddrreq", EDESTADDRREQ ;
           "msgsize", EMSGSIZE ;
           "prototype", EPROTOTYPE ;
           "noprotoopt", ENOPROTOOPT ;
           "protonosupport", EPROTONOSUPPORT ;
           "socktnosupport", ESOCKTNOSUPPORT ;
           "opnotsupp", EOPNOTSUPP ;
           "pfnosupport", EPFNOSUPPORT ;
           "afnosupport", EAFNOSUPPORT ;
           "addrinuse", EADDRINUSE ;
           "addrnotavail", EADDRNOTAVAIL ;
           "netdown", ENETDOWN ;
           "netunreach", ENETUNREACH ;
           "netreset", ENETRESET ;
           "connaborted", ECONNABORTED ;
           "connreset", ECONNRESET ;
           "nobufs", ENOBUFS ;
           "isconn", EISCONN ;
           "notconn", ENOTCONN ;
           "shutdown", ESHUTDOWN ;
           "toomanyrefs", ETOOMANYREFS ;
           "timedout", ETIMEDOUT ;
           "connrefused", ECONNREFUSED ;
           "hostdown", EHOSTDOWN ;
           "hostunreach", EHOSTUNREACH ;
           "loop", ELOOP ;
           "overflow", EOVERFLOW ])
      (fun x -> Some x)
      (fun x -> x)
  ]

let block_error_encoding =
  let open Data_encoding in
  union
    [
      case (Tag 0)
        ~title:"Cannot_parse_operation"
        (obj2
           (req "error" (constant "cannot_parse_operation"))
           (req "operation" Operation_hash.encoding))
        (function Cannot_parse_operation operation -> Some ((), operation)
                | _ -> None)
        (fun ((), operation) -> Cannot_parse_operation operation) ;
      case (Tag 1)
        ~title:"Invalid_fitness"
        (obj3
           (req "error" (constant "invalid_fitness"))
           (req "expected" Fitness.encoding)
           (req "found" Fitness.encoding))
        (function
          | Invalid_fitness { expected ; found } ->
              Some ((), expected, found)
          | _ -> None)
        (fun ((), expected, found) -> Invalid_fitness { expected ; found }) ;
      case (Tag 2)
        ~title:"Non_increasing_timestamp"
        (obj1
           (req "error" (constant "non_increasing_timestamp")))
        (function Non_increasing_timestamp -> Some ()
                | _ -> None)
        (fun () -> Non_increasing_timestamp) ;
      case (Tag 3)
        ~title:"Non_increasing_fitness"
        (obj1
           (req "error" (constant "non_increasing_fitness")))
        (function Non_increasing_fitness -> Some ()
                | _ -> None)
        (fun () -> Non_increasing_fitness) ;
      case (Tag 4)
        ~title:"Invalid_level"
        (obj3
           (req "error" (constant "invalid_level"))
           (req "expected" int32)
           (req "found" int32))
        (function
          | Invalid_level { expected ; found } ->
              Some ((), expected, found)
          | _ -> None)
        (fun ((), expected, found) -> Invalid_level { expected ; found }) ;
      case (Tag 5)
        ~title:"Invalid_proto_level"
        (obj3
           (req "error" (constant "invalid_proto_level"))
           (req "expected" uint8)
           (req "found" uint8))
        (function
          | Invalid_proto_level { expected ; found } ->
              Some ((), expected, found)
          | _ -> None)
        (fun ((), expected, found) ->
           Invalid_proto_level { expected ; found }) ;
      case (Tag 6)
        ~title:"Replayed_operation"
        (obj2
           (req "error" (constant "replayed_operation"))
           (req "operation" Operation_hash.encoding))
        (function Replayed_operation operation -> Some ((), operation)
                | _ -> None)
        (fun ((), operation) -> Replayed_operation operation) ;
      case (Tag 7)
        ~title:"Outdated_operation"
        (obj3
           (req "error" (constant "outdated_operation"))
           (req "operation" Operation_hash.encoding)
           (req "originating_block" Block_hash.encoding))
        (function
          | Outdated_operation { operation ; originating_block } ->
              Some ((), operation, originating_block)
          | _ -> None)
        (fun ((), operation, originating_block) ->
           Outdated_operation { operation ; originating_block }) ;
      case (Tag 8)
        ~title:"Expired_chain"
        (obj4
           (req "error" (constant "expired_chain"))
           (req "chain_id" Chain_id.encoding)
           (req "expiration" Time.Protocol.encoding)
           (req "timestamp" Time.Protocol.encoding))
        (function
          | Expired_chain { chain_id ; expiration ; timestamp } ->
              Some ((), chain_id, expiration, timestamp)
          | _ -> None)
        (fun ((), chain_id, expiration, timestamp) ->
           Expired_chain { chain_id ; expiration ; timestamp }) ;
      case (Tag 9)
        ~title:"Unexpected_number_of_validation_passes"
        (obj2
           (req "error" (constant "unexpected_number_of_passes"))
           (req "found" uint8))
        (function
          | Unexpected_number_of_validation_passes n -> Some ((), n)
          | _ -> None)
        (fun ((), n) -> Unexpected_number_of_validation_passes n) ;
      case (Tag 10)
        ~title:"Too_many_operations"
        (obj4
           (req "error" (constant "too_many_operations"))
           (req "validation_pass" uint8)
           (req "found" uint16)
           (req "max" uint16))
        (function
          | Too_many_operations { pass ; found ; max } ->
              Some ((), pass, found, max)
          | _ -> None)
        (fun ((), pass, found, max) ->
           Too_many_operations { pass ; found ; max }) ;
      case (Tag 11)
        ~title:"Oversized_operation"
        (obj4
           (req "error" (constant "oversized_operation"))
           (req "operation" Operation_hash.encoding)
           (req "found" int31)
           (req "max" int31))
        (function
          | Oversized_operation { operation ; size ; max } ->
              Some ((), operation, size, max)
          | _ -> None)
        (fun ((), operation, size, max) ->
           Oversized_operation { operation ; size ; max }) ;
      case (Tag 12)
        ~title:"Unallowed_pass"
        (obj4
           (req "error" (constant "invalid_pass"))
           (req "operation" Operation_hash.encoding)
           (req "pass" uint8)
           (req "allowed_pass" (list uint8)))
        (function
          | Unallowed_pass { operation ; pass ; allowed_pass } ->
              Some ((), operation, pass, allowed_pass)
          | _ -> None)
        (fun ((), operation, pass, allowed_pass) ->
           Unallowed_pass { operation ; pass ; allowed_pass }) ;
      case (Tag 13)
        ~title:"Cannot_parse_block_header"
        (obj1
           (req "error" (constant "cannot_parse_bock_header")))
        (function
          | Cannot_parse_block_header ->
              Some ()
          | _ -> None)
        (fun () ->
           Cannot_parse_block_header) ;
    ]

let pp_block_error ppf = function
  | Cannot_parse_operation oph ->
      Format.fprintf ppf
        "Failed to parse the operation %a."
        Operation_hash.pp_short oph
  | Invalid_fitness { expected ; found } ->
      Format.fprintf ppf
        "@[<v 2>Invalid fitness:@ \
        \ expected %a@ \
        \ found %a@]"
        Fitness.pp expected
        Fitness.pp found
  | Non_increasing_timestamp ->
      Format.fprintf ppf "Non increasing timestamp"
  | Non_increasing_fitness ->
      Format.fprintf ppf "Non increasing fitness"
  | Invalid_level { expected ; found } ->
      Format.fprintf ppf
        "Invalid level:@ \
        \ expected %ld@ \
        \ found %ld"
        expected
        found
  | Invalid_proto_level { expected ; found } ->
      Format.fprintf ppf
        "Invalid protocol level:@ \
        \ expected %d@ \
        \ found %d"
        expected
        found
  | Replayed_operation oph ->
      Format.fprintf ppf
        "The operation %a was previously included in the chain."
        Operation_hash.pp_short oph
  | Outdated_operation { operation ; originating_block } ->
      Format.fprintf ppf
        "The operation %a is outdated (originated in block: %a)"
        Operation_hash.pp_short operation
        Block_hash.pp_short originating_block
  | Expired_chain { chain_id ; expiration ; timestamp } ->
      Format.fprintf ppf
        "The block timestamp (%a) is later than \
         its chain expiration date: %a (chain: %a)."
        Time.System.pp_hum (Time.System.of_protocol_exn timestamp)
        Time.System.pp_hum (Time.System.of_protocol_exn expiration)
        Chain_id.pp_short chain_id
  | Unexpected_number_of_validation_passes n ->
      Format.fprintf ppf
        "Invalid number of validation passes (found: %d)"
        n
  | Too_many_operations { pass ; found ; max } ->
      Format.fprintf ppf
        "Too many operations in validation pass %d (found: %d, max: %d)"
        pass found max
  | Oversized_operation { operation ; size ; max } ->
      Format.fprintf ppf
        "Oversized operation %a (size: %d, max: %d)"
        Operation_hash.pp_short operation size max
  | Unallowed_pass { operation ; pass ; allowed_pass } ->
      Format.fprintf ppf
        "Operation %a included in validation pass %d, \
        \ while only the following passes are allowed: @[<h>%a@]"
        Operation_hash.pp_short operation pass
        Format.(pp_print_list pp_print_int) allowed_pass
  | Cannot_parse_block_header ->
      Format.fprintf ppf "Failed to parse the block header."

type error +=
  | Invalid_block of
      { block: Block_hash.t ; error: block_error }
  | Unavailable_protocol of
      { block: Block_hash.t ; protocol: Protocol_hash.t }
  | Inconsistent_operations_hash of
      { block: Block_hash.t ;
        expected: Operation_list_list_hash.t ;
        found: Operation_list_list_hash.t }
  | Failed_to_checkout_context of Context_hash.t
  | System_error of { errno: Unix.error ;
                      fn: string ;
                      msg: string }
  | Missing_test_protocol of Protocol_hash.t

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"validator.invalid_block"
    ~title:"Invalid block"
    ~description:"Invalid block."
    ~pp:begin fun ppf (block, error) ->
      Format.fprintf ppf
        "@[<v 2>Invalid block %a@ %a@]"
        Block_hash.pp_short block pp_block_error error
    end
    Data_encoding.(merge_objs
                     (obj1 (req "invalid_block" Block_hash.encoding))
                     block_error_encoding)
    (function Invalid_block { block ; error } ->
       Some (block, error) | _ -> None)
    (fun (block, error) ->
       Invalid_block { block ; error }) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"validator.unavailable_protocol"
    ~title:"Missing protocol"
    ~description:"The protocol required for validating a block is missing."
    ~pp:begin fun ppf (block, protocol) ->
      Format.fprintf ppf
        "Missing protocol (%a) when validating the block %a."
        Protocol_hash.pp_short protocol
        Block_hash.pp_short block
    end
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (req "missing_protocol" Protocol_hash.encoding))
    (function
      | Unavailable_protocol { block ; protocol } ->
          Some (block, protocol)
      | _ -> None)
    (fun (block, protocol) -> Unavailable_protocol { block ; protocol }) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"validator.inconsistent_operations_hash"
    ~title:"Invalid merkle tree"
    ~description:"The provided list of operations is inconsistent with \
                  the block header."
    ~pp:begin fun ppf (block, expected, found) ->
      Format.fprintf ppf
        "@[<v 2>The provided list of operations for block %a \
        \ is inconsistent with the block header@ \
        \ expected: %a@ \
        \ found: %a@]"
        Block_hash.pp_short block
        Operation_list_list_hash.pp_short expected
        Operation_list_list_hash.pp_short found
    end
    Data_encoding.(
      obj3
        (req "block" Block_hash.encoding)
        (req "expected" Operation_list_list_hash.encoding)
        (req "found" Operation_list_list_hash.encoding))
    (function
      | Inconsistent_operations_hash { block ; expected ; found } ->
          Some (block, expected, found)
      | _ -> None)
    (fun (block, expected, found) ->
       Inconsistent_operations_hash { block ; expected ; found });
  Error_monad.register_error_kind
    `Permanent
    ~id:"Validator_process.failed_to_checkout_context"
    ~title: "Fail during checkout context"
    ~description: "The context checkout failed using a given hash"
    ~pp:(fun ppf (hash:Context_hash.t) ->
        Format.fprintf ppf
          "@[Failed to checkout the context with hash %a@]"
          Context_hash.pp_short hash)
    Data_encoding.(obj1 (req "hash" Context_hash.encoding))
    (function
      | Failed_to_checkout_context h -> Some h
      | _ -> None)
    (fun h -> Failed_to_checkout_context h) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"Validator_process.system_error_while_validating"
    ~title: "Failed to validate block because of a system error"
    ~description: "The validator failed because of a system error"
    ~pp:(fun ppf (errno, fn, msg) ->
        Format.fprintf ppf
          "System error while validating a block (in function %s(%s)):@ %s"
          fn msg (Unix.error_message errno))
    Data_encoding.(obj3
                     (req "errno" errno)
                     (req "function" string)
                     (req "msg" string))
    (function
      | System_error { errno ; fn ; msg } -> Some (errno, fn, msg)
      | _ -> None)
    (fun (errno, fn, msg) -> System_error { errno ; fn ; msg }) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"validator.missing_test_protocol"
    ~title:"Missing test protocol"
    ~description:
      "Missing test protocol when forking the test chain"
    ~pp: (fun ppf protocol ->
        Format.fprintf ppf
          "Missing test protocol %a when forking the test chain."
          Protocol_hash.pp protocol)
    Data_encoding.(obj1 (req "test_protocol" Protocol_hash.encoding))
    (function Missing_test_protocol protocol -> Some protocol | _ -> None)
    (fun protocol -> Missing_test_protocol protocol)

let invalid_block block error = Invalid_block { block ; error }
