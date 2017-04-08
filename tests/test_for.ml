let f lo =
  for k = 2 downto lo do
    Printf.printf "< %d\n" k
  done

exception Ohno

let () =
  for i = 3 to 7 do
    Printf.printf "> %d\n" i
  done;
  f (-3);
  for i = 10 to 7 do
    raise Ohno
  done
