let f x = match x with | "a" -> 1 | "b" -> 2 | _ -> 3

let () = Printf.printf "%d\n" (f "a")
let () = Printf.printf "%d\n" (f "b")
let () = Printf.printf "%d\n" (f "c")
