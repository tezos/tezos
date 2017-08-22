let random = Random.State.make [|0|];;
let randint = Random.State.int random

let old_list_rev_sub l n =
  ListLabels.fold_left l ~init:(n, []) ~f:begin fun (n, l) elt ->
    if n <= 0 then (n, l) else (n - 1, elt :: l)
  end |> snd

let test_random_list () =
  let len = randint 10 in
  let l = Array.init len (fun _ -> randint 100) |> Array.to_list in
  let to_take = randint (2 * len + 1) in
  let matchp = (Utils.list_rev_sub l to_take = old_list_rev_sub l to_take) in
  if not matchp then begin
    Printf.printf "Mismatch on [";
    List.iter (Printf.printf "%d; ") l;
    Printf.printf "] %d\n" to_take;
    assert matchp
  end;;

for _ = 1 to 10000 do
  test_random_list ()
done
