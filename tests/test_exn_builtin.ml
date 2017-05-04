let f x =
  raise Not_found

let g x =
  raise (Invalid_argument x)

let tryit fn x =
  try fn x; () with
  | Not_found -> print_string "Not_found\n"
  | Invalid_argument y -> print_string "Invalid_argument "; print_string y; print_newline ()

let _ =
  tryit f "test";
  tryit g "test"
