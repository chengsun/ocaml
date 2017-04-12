let a = 3

let b () = a

let a = 4

let _ = Printf.printf "%d\n" (b ())
