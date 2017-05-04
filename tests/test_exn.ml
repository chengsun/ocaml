exception Aaa
exception Bbb of int

let f x = if x > 0 then raise Aaa else 35
let g x = if x > 0 then raise (Bbb x) else 0

let () =
  try
    print_string "before\n";
    let v = f 30 in
    print_string "f is ";
    print_int v;
    print_newline ()
  with
  | Aaa -> print_string "Aaa\n"
  | Bbb x -> print_string "Bbb "; print_int x; print_newline ()

let () =
  try
    print_string "before\n";
    let v = f (-30) in
    print_string "f is ";
    print_int v;
    print_newline ()
  with
  | Aaa -> print_string "Aaa\n"
  | Bbb x -> print_string "Bbb "; print_int x; print_newline ()

let () =
  try
    print_string "before\n";
    let v = g 30 in
    print_string "g is ";
    print_int v;
    print_newline ()
  with
  | Aaa -> print_string "Aaa\n"
  | Bbb x -> print_string "Bbb "; print_int x; print_newline ()

let () =
  try
    print_string "before\n";
    let v = g (-30) in
    print_string "g is ";
    print_int v;
    print_newline ()
  with
  | Aaa -> print_string "Aaa\n"
  | Bbb x -> print_string "Bbb "; print_int x; print_newline ()
