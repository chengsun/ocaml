
let f x = fun u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 y -> x + y

let _ =
  let r = ref (f 0) in
  for i = 1 to 10_000_000 do
    r := f i;
  done
    