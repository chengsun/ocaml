let rec gcd x y = if y = 0 then x else gcd y (x mod y)

let solve p q =
    let ans = ref 0 in
    let phi = (p-1)*(q-1) in
    for e = 1 to phi do
        if gcd e phi = 1 && gcd (e-1) phi = 2 then
            ans := !ans + e;
    done;
    !ans

let () = Printf.printf "%d\n" (solve 1009 3643)
