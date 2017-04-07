(*
let make_counter () =
  let ctr = ref 0 in
  fun x -> (let y = !ctr + x in ctr := y; y)
  *)

let make_counter () =
  let ctr = ref 0 in
  let count x =
    let y = !ctr + x in
    ctr := y;
    y
  in
  count

let () = (
  let a = make_counter () in
  let b = make_counter () in
  Printf.printf "%d\n" (a 1);
  Printf.printf "%d\n" (b (-1));
  Printf.printf "%d\n" (a 1);
  Printf.printf "%d\n" (b (-1))
)
