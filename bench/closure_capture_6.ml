
let f u0 u1 u2 u3 u4 u5 = fun y -> if u0 && u1 && u2 && u3 && u4 && u5 then y else 0

let _ =
  let closure = f  false false false false false false in
  for i = 1 to 100_000_000 do
    assert (closure 1 = 0)
  done
    