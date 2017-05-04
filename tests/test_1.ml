module A = struct
  let f x y = y
end

let _ =
  (* the compiler needs to know that this is only a partial application! *)
  let g = A.f 0 in
  print_int (g 1); print_newline ()

