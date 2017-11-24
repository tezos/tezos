open Data_encoding

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
let (//) = Filename.concat

let is_invalid_arg = function
  | Invalid_argument _ -> true
  | _ -> false


let is_await = function Binary.Await _ -> true | _ -> false
let is_success = function Binary.Success _ -> true | _ -> false
let is_error = function Binary.Error -> true | _ -> false


let rec fold_left_pending f accu l =
  match l with
  | [] -> accu
  | a::l -> fold_left_pending f (f accu a l) l

let test_read_simple_bin_ko_invalid_data
    ?(not_equal=Assert.not_equal) encoding value =
  let len_data = MBytes.length (Binary.to_bytes encoding value) in
  if classify encoding != `Variable && len_data > 0 then
    for sz = 1 to len_data do
      let l = (Binary.to_bytes_list sz encoding value) in
      List.iter (fun b ->
          for i = 0 to MBytes.length b - 1 do
            (* alter data *)
            MBytes.set_int8 b i ((MBytes.get_int8 b i)+1)
          done
        )l;
      ignore(
        fold_left_pending
          (fun _done e _todo ->
             let _done = e :: _done in
             begin
               let status =
                 Binary.read_stream_of_bytes ~init:(List.rev _done) encoding in
               let status =
                 List.fold_left
                   (fun status mbyte ->
                      match status with
                      | Binary.Await f -> f mbyte
                      | _ -> status
                   )status _todo
               in
               match status with
               | Binary.Await _ -> ()
               | Binary.Error -> ()
               | Binary.Success {res; remaining} ->
                   (* should not have "Success" *)
                   Assert.equal ~msg:__LOC__ remaining [];
                   not_equal value res
             end;
             _done
          )[] l
      )
    done

let unexpected loc =
  loc ^ ": This case should not happen"

let test_read_simple_bin_ko_await encoding value =
  let len_data = MBytes.length (Binary.to_bytes encoding value) in
  if classify encoding != `Variable && len_data > 0 then
    for sz = 1 to len_data do
      let l = Binary.to_bytes_list sz encoding value in
      match List.rev l with
      | [] -> Assert.fail_msg "%s" (unexpected __LOC__)
      | _ :: r ->
          let l = List.rev r in (* last mbyte removed !! *)
          ignore(
            fold_left_pending
              (fun _done e _todo ->
                 let _done = e :: _done in
                 begin
                   let status=
                     Binary.read_stream_of_bytes
                       ~init:(List.rev _done) encoding in
                   let status =
                     List.fold_left
                       (fun status mbyte ->
                          if not (is_await status) then
                            Assert.fail_msg "%s" (unexpected __LOC__);
                          match status with
                          | Binary.Await f -> f mbyte
                          | _ -> status
                       )status _todo
                   in
                   match status with
                   | Binary.Await _ -> ()
                   | Binary.Error ->
                       if not (classify encoding == `Variable) then
                         Assert.fail_msg "%s" (unexpected __LOC__)
                   | Binary.Success _ ->
                       Assert.fail_msg "%s" (unexpected __LOC__)
                 end;
                 _done
              )[] l
          )
    done

let test_read_simple_bin_ok ?msg ?(equal=Assert.equal) encoding value =
  let len_data = max 1 (MBytes.length (Binary.to_bytes encoding value)) in
  for sz = 1 to len_data do
    ignore(
      fold_left_pending
        (fun _done e _todo ->
           let _done = e :: _done in
           begin
             let status =
               Binary.read_stream_of_bytes ~init:(List.rev _done) encoding in
             let status =
               List.fold_left
                 (fun status mbyte ->
                    if MBytes.length mbyte <> 0 && is_success status then
                      Assert.fail_msg "%s" (unexpected __LOC__);
                    match status with
                    | Binary.Await f -> f mbyte
                    | _ -> status
                 )status _todo
             in
             match status with
             | Binary.Success {res; remaining} ->
                 Assert.equal ~msg:__LOC__ remaining [];
                 equal ?msg value res
             | Binary.Await _ -> Assert.fail_msg "%s" (unexpected __LOC__)
             | Binary.Error ->
                 if not (classify encoding == `Variable) then
                   Assert.fail_msg "%s" (unexpected __LOC__)
           end;
           _done
        )[] (Binary.to_bytes_list sz encoding value)
    )
  done

let test_check_simple_bin_ko_invalid_data
    encoding value =
  let len_data = MBytes.length (Binary.to_bytes encoding value) in
  if classify encoding != `Variable && len_data > 0 then
    for sz = 1 to len_data do
      let l = (Binary.to_bytes_list sz encoding value) in
      List.iter (fun b ->
          for i = 0 to MBytes.length b - 1 do
            (* alter data *)
            MBytes.set_int8 b i ((MBytes.get_int8 b i)+1)
          done
        )l;
      ignore(
        fold_left_pending
          (fun _done e _todo ->
             let _done = e :: _done in
             begin
               let status =
                 Binary.check_stream_of_bytes ~init:(List.rev _done) encoding in
               let status =
                 List.fold_left
                   (fun status mbyte ->
                      match status with
                      | Binary.Await f -> f mbyte
                      | _ -> status
                   )status _todo
               in
               match status with
               | Binary.Await _ -> ()
               | Binary.Error -> ()
               | Binary.Success { remaining } ->
                   Assert.equal ~msg:__LOC__ remaining [];
                   (* res is unit for check *)
             end;
             _done
          )[] l
      )
    done

let test_check_simple_bin_ko_await encoding value =
  let len_data = MBytes.length (Binary.to_bytes encoding value) in
  if classify encoding != `Variable && len_data > 0 then
    for sz = 1 to len_data do
      let l = Binary.to_bytes_list sz encoding value in
      match List.rev l with
      | [] -> Assert.fail_msg "%s" (unexpected __LOC__)
      | _ :: r ->
          let l = List.rev r in (* last mbyte removed !! *)
          ignore(
            fold_left_pending
              (fun _done e _todo ->
                 let _done = e :: _done in
                 begin
                   let status=
                     Binary.check_stream_of_bytes
                       ~init:(List.rev _done) encoding in
                   let status =
                     List.fold_left
                       (fun status mbyte ->
                          if not (is_await status) then
                            Assert.fail_msg "%s" (unexpected __LOC__);
                          match status with
                          | Binary.Await f -> f mbyte
                          | _ -> status
                       )status _todo
                   in
                   match status with
                   | Binary.Await _ -> ()
                   | Binary.Error ->
                       if not (classify encoding == `Variable) then
                         Assert.fail_msg "%s" (unexpected __LOC__)
                   | Binary.Success _ ->
                       Assert.fail_msg "%s" (unexpected __LOC__)
                 end;
                 _done
              )[] l
          )
    done

let test_check_simple_bin_ok encoding value =
  let len_data = max 1 (MBytes.length (Binary.to_bytes encoding value)) in
  for sz = 1 to len_data do
    ignore(
      fold_left_pending
        (fun _done e _todo ->
           let _done = e :: _done in
           begin
             let status =
               Binary.check_stream_of_bytes ~init:(List.rev _done) encoding in
             let status =
               List.fold_left
                 (fun status mbyte ->
                    if MBytes.length mbyte <> 0 && is_success status then
                      Assert.fail_msg "%s" (unexpected __LOC__);
                    match status with
                    | Binary.Await f -> f mbyte
                    | _ -> status
                 )status _todo
             in
             match status with
             | Binary.Success { remaining } ->
                 Assert.equal ~msg:__LOC__ remaining [];
                 (* res is unit for check *)
             | Binary.Await _ -> Assert.fail_msg "%s" (unexpected __LOC__)
             | Binary.Error ->
                 if not (classify encoding == `Variable) then
                   Assert.fail_msg "%s" (unexpected __LOC__)
           end;
           _done
        )[] (Binary.to_bytes_list sz encoding value)
    )
  done

let test_simple
    ~msg ?(equal=Assert.equal) ?(not_equal=Assert.not_equal) enc value
  =
  test_check_simple_bin_ok enc value;
  test_check_simple_bin_ko_await enc value;
  test_check_simple_bin_ko_invalid_data enc value;

  test_read_simple_bin_ok ~msg:(msg ^ ": binary-ok") ~equal enc value;
  test_read_simple_bin_ko_await enc value;
  test_read_simple_bin_ko_invalid_data
    ~not_equal enc value




let test_simple_int ~msg encoding i =
  let range_min = - (1 lsl (i-1)) in
  let range_max = (1 lsl (i-1)) - 1 in
  test_simple ~msg encoding range_min ;
  test_simple ~msg encoding range_max

let test_simple_uint ~msg encoding i =
  let range_min = 0 in
  let range_max = (1 lsl i) - 1 in
  test_simple ~msg encoding range_min ;
  test_simple ~msg encoding range_max

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
  test_simple ~msg:__LOC__ (string_enum enum_enc) 4;

  Lwt.return_unit


type t = A of int | B of string | C of int | D of string | E

let prn_t = function
  | A i -> Printf.sprintf "A %d" i
  | B s -> Printf.sprintf "B %s" s
  | C i -> Printf.sprintf "C %d" i
  | D s -> Printf.sprintf "D %s" s
  | E -> "E"

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
  let jsonD = Json.construct enc (D"4") in
  Assert.test_fail
    ~msg:__LOC__ (fun () -> Json.construct enc E) is_invalid_arg ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (A 1) (Json.destruct enc jsonA) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (B "2") (Json.destruct enc jsonB) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (A 3) (Json.destruct enc jsonC) ;
  Assert.equal ~prn:prn_t ~msg:__LOC__ (D "4") (Json.destruct enc jsonD) ;
  let binA = Binary.to_bytes_list 1 enc (A 1) in
  let binB = Binary.to_bytes_list 1 enc (B "2") in
  let binC = Binary.to_bytes_list 1 enc (C 3) in
  let binD = Binary.to_bytes_list 1 enc (D "4") in
  Assert.test_fail ~msg:__LOC__ (fun () -> Binary.to_bytes enc E)
    (function
      | No_case_matched -> true
      | _ -> false) ;
  let get_result ~msg bin_l =
    let status = Binary.read_stream_of_bytes enc in
    let status =
      List.fold_left
        (fun status mbyte ->
           match status with
           | Binary.Await f -> f mbyte
           | _ -> status
        )status bin_l
    in
    match status with
    | Binary.Error -> Assert.fail_msg "%s" msg
    | Binary.Await _ -> Assert.fail_msg "%s" msg
    | Binary.Success {res} -> res
  in
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
  let get_result ~msg bin_l =
    let status = Binary.read_stream_of_bytes enc in
    let status =
      List.fold_left
        (fun status mbyte ->
           match status with
           | Binary.Await f -> f mbyte
           | _ -> status
        )status bin_l
    in
    match status with
    | Binary.Error -> Assert.fail_msg "%s" msg
    | Binary.Await _ -> Assert.fail_msg "%s" msg
    | Binary.Success {res} -> res
  in
  let jsonA = Json.construct enc "41" in
  let jsonB = Json.construct s_enc {field = 42} in
  let binA = Binary.to_bytes_list 1 enc "43" in
  let binB = Binary.to_bytes_list 1 s_enc {field = 44} in
  Assert.equal ~msg:__LOC__ "41" (Json.destruct enc jsonA);
  Assert.equal ~msg:__LOC__ "42" (Json.destruct enc jsonB);
  Assert.equal ~msg:__LOC__ "43" (get_result ~msg:__LOC__ binA);
  Assert.equal ~msg:__LOC__ "44" (get_result ~msg:__LOC__ binB);
  Lwt.return_unit


let wrap_test f base_dir =
  f base_dir >>= fun result ->
  return result

let tests = [
  "simple", test_simple_values ;
  "union", test_union ;
  "splitted", test_splitted ;
]

let () =
  Test.run "stream_data_encoding."
    (List.map (fun (s, f) -> s, wrap_test f) tests)
