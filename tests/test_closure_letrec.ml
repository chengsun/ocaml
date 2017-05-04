let closure_parity n =
  let rec parity = function
    | 0 -> print_int n; print_string " is even!\n"
    | n -> parity_odd (pred n)
  and parity_odd = function
    | 0 -> print_int n; print_string " is odd!\n"
    | n -> parity (pred n)
  in
  parity n

let () = (
  closure_parity 4;
  closure_parity 5;
  ()
)
