let rec parity = function
  | 0 -> print_string "even!\n"
  | n -> parity_odd (pred n)
and parity_odd = function
  | 0 -> print_string "odd!\n"
  | n -> parity (pred n)

let () = (
  parity 4;
  parity 5;
  ()
)
