let () =
  Printf.printf "%d\n" ((+8) lsr 2);
  (* TODO: printf %d emulation broken *)
  (* Printf.printf "%d\n" ((-8) lsr 2); *)
  Printf.printf "%d\n" ((-8) lsr 62);
  Printf.printf "%d\n" ((+8) asr 2);
  Printf.printf "%d\n" ((-8) asr 2);
