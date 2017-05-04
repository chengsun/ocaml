let () =
  let i = ref 1 in
  while !i < 10 do
    print_int !i; print_newline ();
    i := !i * 2
  done

