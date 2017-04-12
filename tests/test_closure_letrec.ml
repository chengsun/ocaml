let closure_parity n =
  let rec parity = function
    | 0 -> Printf.printf "%d is even!\n" n
    | n -> parity_odd (pred n)
  and parity_odd = function
    | 0 -> Printf.printf "%d is odd!\n" n
    | n -> parity (pred n)
  in
  parity n

let () = (
  closure_parity 4;
  closure_parity 5;
  ()
)
