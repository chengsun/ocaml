
let f u0 = fun y -> if u0 then y else 0

let _ =
  let closure = f  false in
  for i = 1 to 100_000_000 do
    assert (closure 1 = 0)
  done
    