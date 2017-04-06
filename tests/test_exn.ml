exception Aaa
exception Bbb of int

let f x = if x > 0 then raise Aaa else 35
let g x = if x > 0 then raise (Bbb x) else 0

let () =
  try Printf.printf "before\n"; Printf.printf "f is %d\n" (f 30); with
  | Aaa -> Printf.printf "Aaa\n"
  | Bbb x -> Printf.printf "Bbb %d\n" x

let () =
  try Printf.printf "before\n"; Printf.printf "f is %d\n" (f (-30)); with
  | Aaa -> Printf.printf "Aaa\n"
  | Bbb x -> Printf.printf "Bbb %d\n" x

let () =
  try Printf.printf "before\n"; Printf.printf "g is %d\n" (g 30); with
  | Aaa -> Printf.printf "Aaa\n"
  | Bbb x -> Printf.printf "Bbb %d\n" x

let () =
  try Printf.printf "before\n"; Printf.printf "g is %d\n" (g (-30)); with
  | Aaa -> Printf.printf "Aaa\n"
  | Bbb x -> Printf.printf "Bbb %d\n" x
