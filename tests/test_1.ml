module A = struct
  let f x y = y
end

let _ =
  (* the compiler needs to know that this is only a partial application! *)
  let g = A.f 0 in
  Printf.printf "%d\n" (g 1)

