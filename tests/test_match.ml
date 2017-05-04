let a = [1;2;3;4;5]

let () =
  match a with
  | v1::v2::v3::_ ->
    print_int v1; print_char ' ';
    print_int v2; print_char ' ';
    print_int v3; print_newline ()
  | _ ->
    print_string "Fail!\n"
