
let f x = fun u0 u1 y -> x + y

let _ =
  let r = ref (f 0) in
  for i = 1 to 10_000_000 do
    r := f i;
  done
    