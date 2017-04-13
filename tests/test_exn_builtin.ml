let f x =
  raise Not_found

let g x =
  raise (Invalid_argument x)

let tryit fn x =
  try fn x; () with
  | Not_found -> Printf.printf "Not_found\n"
  | Invalid_argument y -> Printf.printf "Invalid_argument %s\n" y

let _ =
  tryit f "test";
  tryit g "test"
