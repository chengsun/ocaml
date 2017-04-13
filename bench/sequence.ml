let return x k = k x
let map f seq k = seq (fun x -> k (f x))
let flat_map f seq k = seq (fun x -> f x k)
let filter p seq k = seq (fun x -> if p x then k x)
let of_list l k = List.iter k l
let range i j k = for x = i to j do k x done

let fold f init seq =
  let r = ref init in
  seq (fun elt -> r := f !r elt);
  !r

let map_fold n =
  range 1 n
  |> map (fun x -> n - x)
  |> fold (+) 0

let map_fold_base n =
  let sum = ref 0 in
  for i = 1 to n do
    let x = n - i in
    sum := !sum + x
  done;
  !sum

let flat_map_fold n =
  range 1 n
  |> flat_map (fun x -> range x n)
  |> filter (fun x -> x mod 10 <> 0)
  |> fold (+) 0

let flat_map_fold_base n =
  let sum = ref 0 in
  for i = 1 to n do
    for j = i to n do
      if j mod 10 <> 0 then sum := !sum + j
    done
  done;
  !sum

(* benchs *)

let test f f' n =
  let x = f n in
  let x' = f' n in
  Printf.printf "%d should be equal to %d\n" x x'

let _ =
  let n = 10_000 in
  test map_fold map_fold_base n;
  test flat_map_fold flat_map_fold_base n
