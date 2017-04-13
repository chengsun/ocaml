let max = 1_000_000

let list_fold_range max ~f =
  let rec loop accum i =
    if i > max
    then accum
    else loop (f accum i) (succ i)
  in
  loop 0 1
;;

let realdivs =
    list_fold_range max ~f:(fun accum divisor -> accum + divisor * (max / divisor))
;;

let rec gcd a b =
    match b with
    | 0 -> a
    | _ -> gcd b (a mod b)
;;

exception Done of int
exception Impossible

let gaussdivs =
    list_fold_range max ~f:(fun accum a ->
        accum + (
          try
            list_fold_range max ~f:(fun accum b ->
                if gcd a b > 1 then accum
                else (
                  let divisor = a * a + b * b in
                  if divisor > max then raise (Done accum)
                  else
                      let rec multipleloop accum k =
                          if k * divisor > max then accum
                          else multipleloop (accum + 2 * k * a * (max / (k * divisor))) (succ k)
                      in
                      multipleloop accum 1
                )
            );
            raise Impossible
          with Done x -> x
        )
    )
;;

let () = Printf.printf "%d + %d = %d\n" realdivs gaussdivs (realdivs + gaussdivs)
