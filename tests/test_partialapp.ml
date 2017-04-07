let fma x y z = x * y + z

let three_ma = fma 3
let add10 = fma 5 2
let add6 = three_ma 2

let () = (
  Printf.printf "%d\n" (three_ma 100 33);
  Printf.printf "%d\n" (add10 1);
  Printf.printf "%d\n" (add6 60);
)
