let rec parity = function
  | 0 -> Printf.printf "even!"
  | n -> parity_odd (pred n)
and parity_odd = function
  | 0 -> Printf.printf "odd!"
  | n -> parity (pred n)


(*
let closure_parity n =
  let rec parity = function
    | 0 -> Printf.printf "%d is even!" n
    | n -> parity_odd (pred n)
  and parity_odd = function
    | 0 -> Printf.printf "%d is odd!" n
    | n -> parity (pred n)
  in
  parity n
*)

let () = (
  parity 4;
  parity 5;
  (*
  closure_parity 4;
  closure_parity 5;
  *)
  ()
)
