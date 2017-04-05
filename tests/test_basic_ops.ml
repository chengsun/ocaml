let a = (3,ref 4)
let b = 1::[2]
let c = b

let e = true
let f = 1
let g = 2

(*let () = (snd a) := (fst a) + !(snd a)*)

let d = if (if e then g else f) = g then f * !(snd a) else g - 1

(*
let _ = Printf.printf "%d\n" (fst a)
let _ = Printf.printf "%d\n" !(snd a)
let _ = Printf.printf "%d\n" (List.hd b)
let _ = Printf.printf "%d\n" (List.hd c)
let _ = Printf.printf "%d\n" (if e then 1 else 0)
let _ = Printf.printf "%d\n" f
let _ = Printf.printf "%d\n" g
*)
let _ = Printf.printf "%d\n" d
