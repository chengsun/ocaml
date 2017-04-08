let f lo =
  for k = 2 downto lo do
    Printf.printf "< %d\n" k
  done

let () =
  for i = 3 to 7 do
    Printf.printf "> %d\n" i
  done;
  f (-3)
