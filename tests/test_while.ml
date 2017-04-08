let () =
  let i = ref 1 in
  while !i < 10 do
    Printf.printf "%d\n" !i;
    i := !i * 2
  done

