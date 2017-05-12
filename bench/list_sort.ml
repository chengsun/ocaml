let random_list n =
  let rng n =
    (1664525*n + 1013904223) land (0xFFFFFFFF)
  in
  let rec loop i ((seed::_) as accum) =
    match i with
    | 0 -> accum
    | _ -> loop (i - 1) ((rng seed) :: accum)
  in
  loop n [12345]
;;

let rec assert_sorted = function
  | (x1 : int)::((x2::_) as rest) ->
      assert (x1 <= x2);
      assert_sorted rest
  | _ -> ()
;;

let _ =
  let orig = random_list 100_000 in
  let sorted = List.sort (-) orig in
  assert_sorted sorted
