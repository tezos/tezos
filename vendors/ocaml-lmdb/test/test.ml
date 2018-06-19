open Rresult
open Lmdb

let assert_false v =
  assert (not v)

let assert_error err = function
  | Ok _ -> invalid_arg "assert_error"
  | Error e -> if e <> err then invalid_arg "assert_error"

let assert_equal_ba expected ba =
  assert (expected = Cstruct.(to_string (of_bigarray ba)))

let version () =
  let { major ; minor ; patch } = version () in
  assert (major = 0) ;
  assert (minor = 9) ;
  assert (patch = 70)

let test_string_of_error () =
  let errmsg = string_of_error KeyExist in
  assert (String.length errmsg > 0)

let cleanup () =
  let files = [ "/tmp/data.mdb" ; "/tmp/lock.mdb" ] in
  ListLabels.iter files ~f:begin fun fn ->
    Sys.(if file_exists fn then remove fn)
  end

let env () =
  cleanup () ;
  opendir ~maxreaders:34 ~maxdbs:1 "/tmp" 0o644 >>= fun env ->
  let _stat = stat env in
  let _envinfo = envinfo env in
  let _flags = get_flags env in
  let _path = get_path env in
  let _fd = get_fd env in
  let _maxreaders = get_maxreaders env in
  let _maxkeysize = get_maxkeysize env in
  sync env >>= fun () ->
  Ok ()

let txn () =
  cleanup () ;
  opendir ~maxdbs:1 "/tmp" 0o644 >>= fun env ->
  create_ro_txn env >>= fun rotxn ->
  reset_ro_txn rotxn ;
  create_rw_txn env >>= fun rwtxn ->
  assert (rwtxn = rwtxn) ;
  let env2 = get_txn_env rwtxn in
  assert (env = env2) ;
  opendb rwtxn >>= fun defaultdbi ->
  opendb ~flags:[Create] rwtxn ~name:"bleh" >>= fun dbi ->
  put_string rwtxn dbi "test" "test" >>= fun () ->
  get rwtxn dbi "test" >>= fun buffer ->
  assert_equal_ba "test" buffer ;
  assert_error KeyNotFound (del rwtxn dbi "bleh") ;
  del rwtxn dbi "test" >>= fun () ->
  db_stat rwtxn dbi >>= fun _stat ->
  db_flags rwtxn dbi >>= fun _flags ->
  db_drop rwtxn dbi >>= fun () ->
  closedir env ;
  Ok ()

let cursors () =
  cleanup () ;
  opendir "/tmp" 0o644 >>= fun env ->
  create_rw_txn env >>= fun txn ->
  opendb txn >>= fun db ->
  opencursor txn db >>= fun cursor ->
  assert_error KeyNotFound (cursor_first cursor) ;
  assert_error KeyNotFound (cursor_last cursor) ;
  cursor_put_string cursor "test" "test" >>= fun () ->
  cursor_put_string cursor "test2" "test2" >>= fun () ->
  sync env >>= fun () ->
  cursor_first cursor >>= fun () ->
  cursor_at cursor "" >>= fun () ->
  assert_error KeyNotFound (cursor_prev cursor) ;
  cursor_last cursor >>= fun () ->
  assert_error KeyNotFound (cursor_next cursor) ;
  cursor_prev cursor >>= fun () ->
  get txn db "test" >>= fun buf ->
  assert_equal_ba "test" buf ;
  cursor_get cursor >>= fun (k, v) ->
  assert_equal_ba "test" k ;
  assert_equal_ba "test" v ;
  closedir env ;
  Ok ()

let cursors_del () =
  cleanup () ;
  opendir "/tmp" 0o644 >>= fun env ->
  with_rw_db env ~f:begin fun txn db ->
    with_cursor txn db ~f:begin fun cursor ->
      cursor_put_string cursor "k1" "v1" >>= fun () ->
      cursor_first cursor >>= fun () ->
      cursor_fold_left cursor ~init:() ~f:begin fun acc (_k, _v) ->
        cursor_del cursor
      end >>= fun () ->
      assert_error KeyNotFound (cursor_first cursor) ;
      Ok ()
    end
  end

let cursors_del4 () =
  cleanup () ;
  opendir "/tmp" 0o644 >>= fun env ->
  with_rw_db env ~f:begin fun txn db ->
    with_cursor txn db ~f:begin fun cursor ->
      cursor_put_string cursor "k1" "v1" >>= fun () ->
      cursor_put_string cursor "k2" "v2" >>= fun () ->
      cursor_put_string cursor "k3" "v3" >>= fun () ->
      cursor_put_string cursor "k4" "v4" >>= fun () ->
      cursor_first cursor >>= fun () ->
      cursor_fold_left cursor ~init:() ~f:begin fun acc (_k, _v) ->
        cursor_del cursor
      end >>= fun () ->
      assert_error KeyNotFound (cursor_first cursor) ;
      Ok ()
    end
  end

let fold () =
  cleanup () ;
  opendir "/tmp" 0o644 >>= fun env ->
  with_rw_db env ~f:begin fun txn db ->
    opencursor txn db >>= fun cursor ->
    cursor_put_string cursor "k1" "v1" >>= fun () ->
    cursor_put_string cursor "k2" "v2" >>= fun () ->
    cursor_put_string cursor "k3" "v3" >>= fun () ->
    cursor_put_string cursor "k4" "v4" >>= fun () ->
    cursor_first cursor >>= fun () ->
    cursor_fold_left ~f:begin fun i (k, v) ->
      assert_equal_ba ("k" ^ (string_of_int i)) k ;
      assert_equal_ba ("v" ^ (string_of_int i)) v ;
      Ok (succ i)
    end ~init:1 cursor >>= fun _ ->
    Ok ()
  end >>= fun () ->
  closedir env ;
  Ok ()

let consistency () =
  cleanup () ;
  opendir "/tmp" 0o644 >>= fun env ->
  let v = Cstruct.(to_bigarray (of_string "bleh")) in
  with_rw_db env ~f:begin fun txn db ->
    put txn db "bleh" v
  end >>= fun () ->
  with_ro_db env ~f:begin fun txn db ->
    get txn db "bleh" >>= fun v' ->
    (* assert (v = v') ; *)
    assert_equal_ba "bleh" v' ;
    Ok ()
  end >>= fun () ->
  Ok ()

let fail_on_error f () =
  match f () with
  | Ok _ -> ()
  | Error err -> failwith (string_of_error err)

let basic = [
  "version", `Quick, version ;
  "string_of_error", `Quick, test_string_of_error ;
  "env", `Quick, fail_on_error env ;
  "txn", `Quick, fail_on_error txn ;
  "cursors", `Quick, fail_on_error cursors ;
  "cursors_del", `Quick, fail_on_error cursors_del ;
  "cursors_del4", `Quick, fail_on_error cursors_del4 ;
  "fold", `Quick, fail_on_error fold ;
  "consistency", `Quick, fail_on_error consistency ;
]

let () =
  Alcotest.run "lmdb" [
    "basic", basic ;
  ]
