let f x =
  let g () = fun y -> x + y in
  g ()

let _ =
  Printf.printf "%d\n" (f 1 2)
