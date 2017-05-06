let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let () = print_int (fib 40); print_newline ()
