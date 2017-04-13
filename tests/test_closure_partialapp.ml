let f p =
  (* here g is itself a closure *)
  let g accu x = p x in
  (* and it is partially applied *)
  g ()

let _ =
  (*
  Printf.printf "%d\n" (f (fun x -> x * 2) 3)
  *)
  let h = f (fun x -> x * 2) in
  Printf.printf "%d\n" (h 3)
