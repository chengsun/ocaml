let rec gcd x y = if y = 0 then x else gcd y (x mod y)

let rec remove_power k base =
    if k mod base = 0 then remove_power (k / base) base else k

let reduce (n, d) = let g = gcd n d in (n/g, d/g)

let e = exp 1.

let d n =
    let n' = float_of_int n in
    let pmax = n' /. e in
    let pfloor = floor pmax in
    let pceil = ceil pmax in
    let logcalc p = p *. (log n' -. log p) in
    let p' = if logcalc pfloor > logcalc pceil then pfloor else pceil in
    let (_, den) = reduce (n, int_of_float p') in
    let den = remove_power den 2 in
    let den = remove_power den 5 in
    if den > 1 then n else -n

let () =
    let ans = ref 0 in
    for n = 5 to 10000 do
        ans := !ans + d n
    done;
    Printf.printf "%d\n" !ans
