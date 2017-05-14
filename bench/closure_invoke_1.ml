
let make_counter () =
  let counter = ref 0 in
  fun  x -> (
    counter := !counter + x;
    !counter
  )

let _ =
  let closure = make_counter () in
  let values = ref 0 in
  for i = 1 to 100_000_000 do
    values := !values + (closure  1)
  done
    