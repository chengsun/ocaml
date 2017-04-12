let a = [1;2;3;4;5]

let () =
  match a with
  | v1::v2::v3::_ ->
      Printf.printf "%d %d %d\n" v1 v2 v3
  | _ ->
      Printf.printf "Fail!\n"
