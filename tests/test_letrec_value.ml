let rec a = 1::b and b = 2::a

let () =
  match a with
  | v1::v2::v3::v4::v5::_ ->
    Printf.printf "%d %d %d %d %d\n" v1 v2 v3 v4 v5
  | _ ->
    Printf.printf "Fail!\n"
