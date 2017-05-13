let () =
  print_int ((+8) lsr 2); print_newline ();
  print_int ((-8) lsr 2); print_newline ();
  print_int ((-8) lsr 48); print_newline ();
  print_int ((+8) asr 2); print_newline ();
  print_int ((-8) asr 2); print_newline ();

  print_int (44 + (-2)); print_newline ();
  print_int (40 - (-2)); print_newline ();
  print_int (21 * 2); print_newline ();
  print_int ((-84) / (-2)); print_newline ();

  print_float (44. +. (-2.)); print_newline ();
  print_float (40. -. (-2.)); print_newline ();
  print_float (21. *. 2.); print_newline ();
  print_float ((-84.) /. (-2.)); print_newline ();
