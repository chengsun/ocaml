let f x =
  let g () = fun y -> x + y in
  g ()

let _ =
  print_int (f 1 2);
  print_newline ()
