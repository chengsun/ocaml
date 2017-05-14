
let make_counter () =
  let counter = ref 0 in
  fun u0 u1 u2 u3 u4 x -> (
    counter := !counter + x;
    !counter
  )

let _ =
  let closure = make_counter () in
  let values = ref 0 in
  for i = 1 to 100_000_000 do
    values := !values + (closure  0 0 0 0 0 1)
  done
    