let a = (3,ref 4)
let b = 1::[2]
let c = b

let e = true
let f = 1
let g = 2

let d = if (if e then g else f) = g then f + !(snd a) else g - 3

let _ = Printf.printf "%d" d
