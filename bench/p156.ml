let pow b e =
    let rec loop x = function
        | 0 -> x
        | e -> loop (x * b) (e - 1)
    in
    loop 1 e

(* in range [start, start + 10^power), where
 *   10^power | start,
 *   startd = number of d in start,
 *   startacc = f(start - 1, d) *)
let rec count d start startd startacc power =
    let range = pow 10 power in
    let subrange = range / 10 in
    let end_ = start + range - 1 in
    let endacc = startacc + startd * range + power * subrange in (* = f(end_, d) *)
    let result =
        if end_ < startacc || start > endacc then
            0
        else if power = 0 then (
                if end_ = endacc then end_ else 0
        ) else (
            let acc = ref startacc in
            let counter = ref 0 in
            for i = 0 to 9 do
                let (endacc, result) =
                    count d
                        (start + i * subrange)
                        (startd + (if i = d then 1 else 0))
                        !acc
                        (power - 1)
                in
                acc := endacc;
                counter := !counter + result;
            done;
            assert (!acc = endacc);
            !counter
        )
    in
    (endacc, result)

let () =
    let final_result = ref 0 in
    for d = 1 to 9 do
        let (_, result) = count d 0 0 0 11 in
        final_result := !final_result + result
    done;
    print_int !final_result; print_newline ()
