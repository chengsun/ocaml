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

(* --- *)

let tester make_counter = (
  let a = make_counter () in
  let b = make_counter () in
  print_int (a 1); print_newline ();
  print_int (b (-1)); print_newline ();
  print_int (a 1); print_newline ();
  print_int (b (-1)); print_newline ()
)

let () = (
  tester make_counter_v1;
  print_string "---\n";
  tester make_counter_v2;
  print_string "---\n";
  tester make_counter_v3;
  print_string "---\n";
  tester make_counter_v4;
)
