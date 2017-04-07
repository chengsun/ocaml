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

let tester make_counter = (
  let a = make_counter () in
  let b = make_counter () in
  Printf.printf "%d\n" (a 1);
  Printf.printf "%d\n" (b (-1));
  Printf.printf "%d\n" (a 1);
  Printf.printf "%d\n" (b (-1))
)

let () = (
  tester make_counter_v1;
  Printf.printf "---\n";
  tester make_counter_v2;
)
