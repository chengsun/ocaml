let rec a = 1::b and b = 2::a

let () =
  match a with
  | v1::v2::v3::v4::v5::_ ->
    print_int v1; print_char ' ';
    print_int v2; print_char ' ';
    print_int v3; print_char ' ';
    print_int v4; print_char ' ';
    print_int v5; print_newline ()
  | _ ->
    print_string "Fail!\n"
