let print_intlist xs =
  let rec loop b = function
    | [] -> print_newline ()
    | x::xs ->
        if b then print_char ' ';
        print_int x;
        loop true xs
  in
  loop false xs
;;

let () = print_intlist (List.sort (-) [3;2;1;5;4])
