let f lo =
  for k = 2 downto lo do
    print_string "< "; print_int k; print_newline ()
  done

exception Ohno

let () =
  for i = 3 to 7 do
    print_string "> "; print_int i; print_newline ()
  done;
  f (-3);
  for i = 10 to 7 do
    raise Ohno
  done
