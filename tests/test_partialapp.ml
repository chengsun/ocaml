let fma x y z = x * y + z

let three_ma = fma 3
let add10 = fma 5 2
let add6 = three_ma 2

let () = (
  print_int (three_ma 100 33); print_newline ();
  print_int (add10 1); print_newline ();
  print_int (add6 60); print_newline ()
)
