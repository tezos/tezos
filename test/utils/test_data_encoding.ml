open Data_encoding

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

let write_file dir ~name content =
  let file = (dir // name) in
  let oc = open_out file in
  output_string oc content ;
  close_out oc ;
  file

let is_invalid_arg = function
  | Invalid_argument _ -> true
  | _ -> false

let test_simple_json ?msg ?(equal=Assert.equal) encoding value =
  let json = Json.construct encoding value in
  let result = Json.destruct encoding json in
  equal ?msg value result

let test_simple_bin ?msg ?(equal=Assert.equal) encoding value =
  let bin = Binary.to_bytes encoding value in
  let opt = Binary.of_bytes encoding bin in
  Assert.is_some ?msg opt;
  let result = match opt with None -> assert false | Some v -> v in
  equal ?msg value result

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
      | _ -> false) ;
  (* Should fail *)
  (* test_bin_exn ~msg:__LOC__ (string_enum ["a", 1; "a", 2]) 2 (...duplicatate...); *)
  (* test_json_exn ~msg:__LOC__ (string_enum ["a", 1; "a", 2]) 1 (... duplicate...); *)

  Lwt.return_unit

let test_json testdir =
  let open Data_encoding_ezjsonm in
  let file = testdir // "testing_data_encoding.tezos" in
  let v = `Float 42. in
  let f_str = to_string ~minify:false v in
  Assert.equal_string  ~msg:__LOC__ f_str "[\n  42\n]";
  read_file (testdir // "NONEXISTINGFILE") >>= fun rf ->
  Assert.is_error ~msg:__LOC__ rf ;
  write_file file v >>= fun success ->
  Assert.is_ok ~msg:__LOC__ success ;
  read_file file >>= fun opt ->
  Assert.is_ok ~msg:__LOC__ opt ;
  Lwt.return ()

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
            | _ -> false) ;
  Lwt.return_unit

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
  Assert.equal ~prn:prn_t ~msg:__LOC__ (D "4") (get_result ~msg:__LOC__ binD) ;
  Lwt.return_unit


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
  Assert.equal ~msg:__LOC__ "44" (get_result ~msg:__LOC__ binB);
  Lwt.return_unit

let test_json_input testdir =
  let enc =
    obj1
      (req "menu" (
          obj3
            (req "id" string)
            (req "value" string)
            (opt "popup" (
                obj2
                  (req "width" int64)
                  (req "height" int64))))) in
  begin
    let file =
      write_file testdir ~name:"good.json" {|
 {
    "menu": {
        "id": "file",
        "value": "File",
        "popup": {
            "width" : 42,
            "height" : 52
        }
    }
}
|}
    in
    Data_encoding_ezjsonm.read_file file >>= function
    | Error _ -> Assert.fail_msg "Cannot parse \"good.json\"."
    | Ok json ->
        let (id, value, popup) = Json.destruct enc json in
        Assert.equal_string ~msg:__LOC__ "file" id;
        Assert.equal_string ~msg:__LOC__ "File" value;
        Assert.is_some ~msg:__LOC__ popup;
        let w,h = match popup with None -> assert false | Some (w,h) -> w,h in
        Assert.equal_int64 ~msg:__LOC__ 42L w;
        Assert.equal_int64 ~msg:__LOC__ 52L h;
        Lwt.return_unit
  end >>= fun () ->
  let enc =
    obj2
      (req "kind" (string))
      (req "data" (int64)) in
  begin
    let file =
      write_file testdir ~name:"unknown.json" {|
{
  "kind" : "int64",
  "data" : "42",
  "unknown" : 2
}
|}
    in
    Data_encoding_ezjsonm.read_file file >>= function
    | Error _ -> Assert.fail_msg "Cannot parse \"unknown.json\"."
    | Ok json ->
        Assert.test_fail ~msg:__LOC__
          (fun () -> ignore (Json.destruct enc json))
          (function
            | Json.Unexpected_field "unknown" -> true
            | _ -> false) ;
        Lwt.return_unit
  end

let wrap_test f base_dir =
  f base_dir >>= fun result ->
  return result

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
  Lwt.return @@ Assert.equal data decoded

let tests = [
  "simple", test_simple_values ;
  "json", test_json ;
  "union", test_union ;
  "splitted", test_splitted ;
  "json.input", test_json_input ;
  "tags", test_tag_errors ;
  "wrapped_binary", test_wrapped_binary ;
]

let () =
  Test.run "data_encoding." (List.map (fun (s, f) -> s, wrap_test f) tests)
