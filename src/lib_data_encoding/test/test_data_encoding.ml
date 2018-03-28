(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Utils.Infix
open Data_encoding

let is_invalid_arg = function
  | Invalid_argument _ -> true
  | _ -> false

let test_simple_json ?msg ?(equal=Assert.equal) encoding value =
  let result = try
      let json = Json.construct encoding value in
      Json.destruct encoding json
    with exn ->
      let trace = Printexc.get_backtrace () in
      Assert.fail_msg "%s %s\n%s"
        (match msg with Some msg -> msg | None -> "no message")
        (Printexc.to_string exn)
        trace in
  equal ?msg value result

let test_simple_bin ?msg ?(equal=Assert.equal) encoding value =
  let opt = try
      let bin = Binary.to_bytes encoding value in
      Binary.of_bytes encoding bin
    with exn ->
      let trace = Printexc.get_backtrace () in
      Assert.fail_msg "%s %s\n%s"
        (match msg with Some msg -> msg | None -> "no message")
        (Printexc.to_string exn)
        trace in
  Assert.is_some ?msg opt;
  let result = match opt with None -> assert false | Some v -> v in
  equal ?msg value result

let test_simple_of_bin ?msg ?(equal=Assert.equal) encoding value bin =
  let opt = Binary.of_bytes encoding bin in
  equal ?msg value opt

let test_json_exn ?msg encoding value fail =
  let get_result () =
    let bin = Json.construct encoding value in
    Json.destruct encoding bin in
  Assert.test_fail ?msg get_result fail

let test_bin_exn ?msg encoding value fail =
  let get_result () =
    let bin = Binary.to_bytes encoding value in
    Binary.of_bytes encoding bin in
  Assert.test_fail ?msg get_result fail

let test_simple ~msg ?(equal=Assert.equal) enc value =
  test_simple_json ~msg:(msg ^ ": json") ~equal enc value ;
  test_simple_bin ~msg:(msg ^ ": binary") ~equal enc value

let test_simple_exn ~msg enc value =
  test_json_exn ~msg:(msg ^ ": json") enc value (fun _ -> true) ;
  test_bin_exn ~msg:(msg ^ ": json") enc value (fun _ -> true)

let test_simple_int ~msg encoding i =
  let range_min = - (1 lsl (i-1)) in
  let range_max = (1 lsl (i-1)) - 1 in
  let out_max = (1 lsl (i-1)) in
  let out_min = - (1 lsl (i-1)) - 1 in
  test_simple ~msg encoding range_min ;
  test_simple ~msg encoding range_max ;
  test_simple_exn ~msg encoding out_max ;
  test_simple_exn ~msg encoding out_min

let test_simple_uint ~msg encoding i =
  let range_min = 0 in
  let range_max = (1 lsl i) - 1 in
  let out_max = 1 lsl i in
  let out_min = - 1 in
  test_simple ~msg encoding range_min ;
  test_simple ~msg encoding range_max ;
  test_simple_exn ~msg encoding out_max ;
  test_simple_exn ~msg encoding out_min

let test_simple_values _ =
  test_simple ~msg:__LOC__ null ();
  test_simple ~msg:__LOC__ empty ();
  test_simple ~msg:__LOC__ (constant "toto") ();
  test_simple_int ~msg:__LOC__ int8 8;
  test_simple_uint ~msg:__LOC__ uint8 8;
  test_simple_int ~msg:__LOC__ int16 16;
  test_simple_uint ~msg:__LOC__ uint16 16;
  test_simple_int ~msg:__LOC__ int31 31;
  test_simple ~msg:__LOC__ int32 Int32.min_int;
  test_simple ~msg:__LOC__ int32 Int32.max_int;
  test_simple ~msg:__LOC__ int64 Int64.min_int;
  test_simple ~msg:__LOC__ int64 Int64.max_int;
  test_simple ~msg:__LOC__ (ranged_int 100 400) 399;
  test_simple ~msg:__LOC__ (ranged_int 19000 19254) 19000;
  test_simple ~msg:__LOC__ (ranged_int 19000 19254) 19254;
  test_simple ~msg:__LOC__ (ranged_int ~-100 300) 200;
  test_simple ~msg:__LOC__ (ranged_int ~-300_000_000 300_000_000) 200;
  test_simple ~msg:__LOC__ (ranged_int ~-300_000_000 300_000_000) 200_000_000;
  test_simple ~msg:__LOC__ (ranged_float 100. 200.) 150.;
  test_simple ~msg:__LOC__ (ranged_float ~-.100. 200.) ~-.75.;
  test_simple ~msg:__LOC__ bool true;
  test_simple ~msg:__LOC__ bool false;
  test_simple ~msg:__LOC__ string "tutu";
  test_simple ~msg:__LOC__ bytes (MBytes.of_string "titi");
  test_simple ~msg:__LOC__ float 42.;
  test_simple ~msg:__LOC__ float max_float;
  test_simple ~msg:__LOC__ float min_float;
  test_simple ~msg:__LOC__ float (-. 0.);
  test_simple ~msg:__LOC__ float (+. 0.);
  test_simple ~msg:__LOC__ float infinity;
  test_simple ~msg:__LOC__ float neg_infinity;
  test_simple ~msg:__LOC__ float epsilon_float;
  test_simple ~msg:__LOC__ ~equal:Assert.equal_float float nan;
  test_simple ~msg:__LOC__ (option string) (Some "thing");
  test_simple ~msg:__LOC__ (option string) None;
  let enum_enc =
    ["one", 1; "two", 2; "three", 3; "four", 4; "five", 6; "six", 6] in
  test_simple_bin ~msg:__LOC__ (string_enum enum_enc) 4;
  test_json_exn ~msg:__LOC__ (string_enum enum_enc) 7 is_invalid_arg ;
  test_bin_exn ~msg:__LOC__ (string_enum enum_enc) 7
    (function
      | No_case_matched -> true
      | _ -> false)
(* Should fail *)
(* test_bin_exn ~msg:__LOC__ (string_enum ["a", 1; "a", 2]) 2 (...duplicatate...); *)
(* test_json_exn ~msg:__LOC__ (string_enum ["a", 1; "a", 2]) 1 (... duplicate...); *)

let test_zarith _ =
  let test i = test_simple ~msg:("failed on Z number " ^ Z.to_string i) z i in
  let test_of_bin bin exp name = test_simple_of_bin ~msg:("failed on " ^ name) z exp (MBytes.of_string bin) in
  for i = -1_00_000 to 1_00_000 do test (Z.of_int i) done ;
  for i = 100_000_000 to 100_100_000 do test (Z.of_int i) done ;
  for i = -100_000_000 downto -100_100_000 do test (Z.of_int i) done ;
  let rec fact n l =
    if n > 1 then
      let l = Z.mul l (Z.of_int n) in
      test l ;
      fact (n - 1) l in
  fact 35 Z.one ;
  test (Z.of_string "123574503164821730218493275982143254986574985328") ;
  test (Z.of_string "8493275982143254986574985328") ;
  test (Z.of_string "123574503164821730218474985328") ;
  test (Z.of_string "10000000000100000000001000003050000000060600000000000777000008") ;
  test (Z.of_string "-123574503164821730218493275982143254986574985328") ;
  test (Z.of_string "-8493275982143254986574985328") ;
  test (Z.of_string "-123574503164821730218474985328") ;
  test (Z.of_string "-10000000000100000000001000003050000000060600000000000777000008") ;
  test_of_bin "\x03" (Some (Z.of_int 3)) "3 (size OK)" ;
  test_of_bin "\x83" None "3 (size + 1, truncated)" ;
  test_of_bin "\x83\x00"  None "3 (size + 1)" ;
  test_of_bin "\x83\x80\x00" None "3 (size + 2)" ;

type t = A of int | B of string | C of int | D of string | E

let prn_t = function
  | A i -> Printf.sprintf "A %d" i
  | B s -> Printf.sprintf "B %s" s
  | C i -> Printf.sprintf "C %d" i
  | D s -> Printf.sprintf "D %s" s
  | E -> "E"

let test_tag_errors _ =
  let duplicate_tag () =
    union [
      case (Tag 1)
        int8
        (fun i -> i)
        (fun i -> Some i) ;
      case (Tag 1)
        int8
        (fun i -> i)
        (fun i -> Some i)] in
  Assert.test_fail ~msg:__LOC__ duplicate_tag
    (function Duplicated_tag _ -> true
            | _ -> false) ;
  let invalid_tag () =
    union [
      case (Tag (2 lsl 7))
        int8
        (fun i -> i)
        (fun i -> Some i)] in
  Assert.test_fail ~msg:__LOC__  invalid_tag
    (function (Invalid_tag (_, `Uint8)) -> true
            | _ -> false)

let test_union _ =
  let enc =
    (union [
        case (Tag 1)
          int8
          (function A i -> Some i | _ -> None)
          (fun i -> A i) ;
        case (Tag 2)
          string
          (function B s -> Some s | _ -> None)
          (fun s -> B s) ;
        case (Tag 3)
          int8
          (function C i -> Some i | _ -> None)
          (fun i -> C i) ;
        case (Tag 4)
          (obj2
             (req "kind" (constant "D"))
             (req "data" (string)))
          (function D s -> Some ((), s) | _ -> None)
          (fun ((), s) -> D s) ;
      ]) in
  let jsonA = Json.construct enc (A 1) in
  let jsonB = Json.construct enc (B "2") in
  let jsonC = Json.construct enc (C 3) in
  let jsonD = Json.construct enc (D "4") in
  Assert.test_fail
    ~msg:__LOC__ (fun () -> Json.construct enc E) is_invalid_arg ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (A 1) (Json.destruct enc jsonA) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (B "2") (Json.destruct enc jsonB) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (A 3) (Json.destruct enc jsonC) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (D "4") (Json.destruct enc jsonD) ;
  let binA = Binary.to_bytes enc (A 1) in
  let binB = Binary.to_bytes enc (B "2") in
  let binC = Binary.to_bytes enc (C 3) in
  let binD = Binary.to_bytes enc (D "4") in
  Assert.test_fail ~msg:__LOC__ (fun () -> Binary.to_bytes enc E)
    (function
      | No_case_matched -> true
      | _ -> false) ;
  let get_result ~msg bin =
    match Binary.of_bytes enc bin with
    | None -> Assert.fail_msg "%s" msg
    | Some bin -> bin in
  Assert.equal ~prn:prn_t ~msg:__LOC__ (A 1) (get_result ~msg:__LOC__ binA) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (B "2") (get_result ~msg:__LOC__ binB) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (C 3) (get_result ~msg:__LOC__ binC) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (D "4") (get_result ~msg:__LOC__ binD)


type s = { field : int }

let test_splitted _ =
  let s_enc =
    def "s" @@
    describe
      ~title:"testsuite encoding test"
      ~description: "A human readable description" @@
    conv
      (fun s -> string_of_int s.field)
      (fun s -> { field = int_of_string s })
      string in
  let enc =
    (splitted
       ~binary:string
       ~json:
         (union [
             case (Tag 1)
               string
               (fun _ -> None)
               (fun s -> s) ;
             case (Tag 2)
               s_enc
               (fun s -> Some { field = int_of_string s })
               (fun s -> string_of_int s.field) ;
           ])) in
  let get_result ~msg bin =
    match Binary.of_bytes enc bin with
    | None -> Assert.fail_msg "%s: Cannot parse." msg
    | Some bin -> bin in
  let jsonA = Json.construct enc "41" in
  let jsonB = Json.construct s_enc {field = 42} in
  let binA = Binary.to_bytes enc "43" in
  let binB = Binary.to_bytes s_enc {field = 44} in
  Assert.equal ~msg:__LOC__ "41" (Json.destruct enc jsonA);
  Assert.equal ~msg:__LOC__ "42" (Json.destruct enc jsonB);
  Assert.equal ~msg:__LOC__ "43" (get_result ~msg:__LOC__ binA);
  Assert.equal ~msg:__LOC__ "44" (get_result ~msg:__LOC__ binB)

let test_wrapped_binary _ =
  let open Data_encoding in
  let enc = union [
      case (Tag 0)
        (obj1 (req "ok" string))
        (function Ok x -> Some x | _ -> None)
        (fun x -> Ok x) ;
      case (Tag 1)
        (obj1 (req "error" string))
        (function Error x -> Some x | _ -> None)
        (fun x -> Error x) ;
    ] in
  let data = (Ok "") in
  let encoded = Data_encoding.Binary.to_bytes enc data in
  let decoded = Data_encoding.Binary.of_bytes_exn enc encoded in
  Assert.equal data decoded

let test_out_of_range () =
  let assert_exception ~msg enc x =
    begin try
        ignore (Data_encoding.Json.construct enc x : Data_encoding.json) ;
        Assert.fail_msg "%s: json" msg
      with Invalid_argument _ -> ()
    end ;
    begin
      try
        ignore (Data_encoding.Binary.to_bytes enc x) ;
        Assert.fail_msg "%s: binary" msg
      with Invalid_argument _ -> ()
    end in
  let enc_int = Data_encoding.ranged_int ~-30 100 in
  let enc_float = Data_encoding.ranged_float ~-.30. 100. in
  assert_exception ~msg: __LOC__ enc_int 101 ;
  assert_exception ~msg: __LOC__ enc_int ~-32 ;
  assert_exception ~msg: __LOC__ enc_float ~-.31. ;
  assert_exception ~msg: __LOC__ enc_float 101.

let test_string_enum_boundary _ =
  let open Data_encoding in
  let entries = List.rev_map (fun x -> string_of_int x, x) (0 -- 254) in
  let run_test cases =
    List.iter (fun (_, num)  ->
        let enc = string_enum cases in
        let encoded = Data_encoding.Binary.to_bytes enc num in
        let decoded = Data_encoding.Binary.of_bytes_exn enc encoded in
        Assert.equal num decoded) cases in
  run_test entries ;
  let entries2 = (("255", 255) :: entries) in
  run_test entries2 ;
  run_test (("256", 256) :: entries2)

(** Generate encodings of the encoding and the randomized generator *)
let test_generator ?(iterations=50) encoding generator =
  for _ = 0 to iterations - 1 do
    let encode = generator () in
    let bytes = Data_encoding.Binary.to_bytes encoding encode in
    let decode = Data_encoding.Binary.of_bytes_exn encoding bytes in
    Assert.equal encode decode
  done

let rec make_int_list acc len () =
  if len = 0
  then acc
  else make_int_list (Random.int64 Int64.max_int :: acc) (len - 1) ()

let test_randomized_int_list _ =
  test_generator Data_encoding.(list int64) (make_int_list [] 100)

let test_randomized_string_list _ =
  test_generator (list string) (fun () -> List.map Int64.to_string (make_int_list [] 100 ()))

let test_randomized_variant_list _ =
  test_generator (list (result (option string) string))
    (fun () ->
       List.map
         (fun x ->
            let str = Int64.to_string x in
            if Random.bool ()
            then if Random.bool () then Ok (Some str) else Ok None
            else Error str)
         (make_int_list [] 100 ()))

let tests = [
  "zarith", `Quick, test_zarith ;
  "simple", `Quick, test_simple_values ;
  "union", `Quick, test_union ;
  "splitted", `Quick, test_splitted ;
  "tags", `Quick, test_tag_errors ;
  "wrapped_binary", `Quick, test_wrapped_binary ;
  "out_of_range", `Quick, test_out_of_range ;
  "string_enum_boundary", `Quick, test_string_enum_boundary ;
  "randomized_int_list", `Quick, test_randomized_int_list ;
  "randomized_string_list", `Quick, test_randomized_string_list ;
  "randomized_variant_list", `Quick, test_randomized_variant_list ;
]

