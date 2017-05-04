let f x = match x with | "a" -> 1 | "b" -> 2 | _ -> 3

let () = print_int (f "a"); print_newline ()
let () = print_int (f "b"); print_newline ()
let () = print_int (f "c"); print_newline ()
