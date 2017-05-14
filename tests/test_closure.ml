let make_counter_v1 () =
  let ctr = ref 0 in
  fun x -> (let y = !ctr + x in ctr := y; y)

let make_counter_v2 () =
  let ctr = ref 0 in
  let count x =
    let y = !ctr + x in
    ctr := y;
    y
  in
  count

(* the same as above, but with more than one env variable used *)

let make_counter_v3 () =
  let ctr = ref 0 in
  let dummy = ref false in
  fun x -> (dummy := true; let y = !ctr + x in ctr := y; y)

let make_counter_v4 () =
  let ctr = ref 0 in
  let dummy = ref false in
  let count x =
    dummy := true;
    let y = !ctr + x in
    ctr := y;
    y
  in
  count

(* the same as above, but with more than five arguments *)

let make_counter_v1_eight () =
  let ctr = ref 0 in
  fun u1 u2 u3 u4 u5 u6 u7 u8 x -> (
    assert (u1 = 1);
    assert (u2 = 2);
    assert (u3 = 3);
    assert (u4 = 4);
    assert (u5 = 5);
    assert (u6 = 6);
    assert (u7 = 7);
    assert (u8 = 8);
    let y = !ctr + x in
    ctr := y; y
  )

let make_counter_v2_eight () =
  let ctr = ref 0 in
  let count u1 u2 u3 u4 u5 u6 u7 u8 x =
    assert (u1 = 1);
    assert (u2 = 2);
    assert (u3 = 3);
    assert (u4 = 4);
    assert (u5 = 5);
    assert (u6 = 6);
    assert (u7 = 7);
    assert (u8 = 8);
    let y = !ctr + x in
    ctr := y;
    y
  in
  count

(* --- *)

let tester make_counter = (
  let a = make_counter () in
  let b = make_counter () in
  print_int (a 1); print_newline ();
  print_int (b (-1)); print_newline ();
  print_int (a 1); print_newline ();
  print_int (b (-1)); print_newline ()
)

let tester_eight make_counter = (
  let a = make_counter () in
  let b = make_counter () in
  print_int (a 1 2 3 4 5 6 7 8 1); print_newline ();
  print_int (b 1 2 3 4 5 6 7 8 (-1)); print_newline ();
  print_int (a 1 2 3 4 5 6 7 8 1); print_newline ();
  print_int (b 1 2 3 4 5 6 7 8 (-1)); print_newline ()
)

let () = (
  tester make_counter_v1;
  print_string "---\n";
  tester make_counter_v2;
  print_string "---\n";
  tester make_counter_v3;
  print_string "---\n";
  tester make_counter_v4;
  print_string "---\n";
  tester_eight make_counter_v1_eight;
  print_string "---\n";
  tester_eight make_counter_v2_eight;
)
