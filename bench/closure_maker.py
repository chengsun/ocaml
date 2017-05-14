for i in range(1, 16):
    args = " ".join(("u{}".format(j) for j in range(i-1)))
    vals = " 0"*(i-1)
    open("closure_invoke_{}.ml".format(i), "w").write("""
let make_counter () =
  let counter = ref 0 in
  fun {} x -> (
    counter := !counter + x;
    !counter
  )

let _ =
  let closure = make_counter () in
  let values = ref 0 in
  for i = 1 to 100_000_000 do
    values := !values + (closure {} 1)
  done
    """.format(args, vals))

    open("closure_create_{}.ml".format(i), "w").write("""
let f x = fun {} y -> x + y

let _ =
  let r = ref (f 0) in
  for i = 1 to 10_000_000 do
    r := f i;
  done
    """.format(args))

    open("closure_capture_{}.ml".format(i), "w").write("""
let f {} = fun y -> if {} then y else 0

let _ =
  let closure = f {} in
  for i = 1 to 100_000_000 do
    assert (closure 1 = 0)
  done
    """.format(" ".join(("u{}".format(j) for j in range(i))),
               " && ".join(("u{}".format(j) for j in range(i))),
               " false"*i))

