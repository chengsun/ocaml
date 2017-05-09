let f () = ()

let g () = raise Not_found

let _ = try f () with Not_found -> (print_string "wrong: Not_found"; print_newline ())

let _ = try g () with Division_by_zero -> (print_string "wrong: Division_by_zero"; print_newline ())
