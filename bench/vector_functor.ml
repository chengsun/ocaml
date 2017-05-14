module type Vector_base = sig
  type t
  type elt
  type index

  val fold_index : (index -> elt -> 'a) -> ('a -> index -> elt -> 'a) -> t -> 'a
  val fold_index_2 : (index -> elt -> elt -> 'a) -> ('a -> index -> elt -> elt -> 'a) -> t -> t -> 'a
  val map : (index -> elt -> elt) -> t -> t

end

module Vect2 = struct
  type t = float * float
  type elt = float
  type index = Fst | Snd

  let fold_index init f (fst, snd) =
    f (init Fst fst) Snd snd
  let fold_index_2 init f (fst1, snd1) (fst2, snd2) =
    f (init Fst fst1 fst2) Snd snd1 snd2
  let map f (fst, snd) =
    (f Fst fst, f Snd snd)
end

module Vect3 = struct
  type t = float * float * float
  type elt = float
  type index = Fst | Snd | Trd

  let fold_index init f (fst, snd, trd) =
    f (f (init Fst fst) Snd snd) Trd trd
  let fold_index_2 init f (fst1, snd1, trd1) (fst2, snd2, trd2) =
    f (f (init Fst fst1 fst2) Snd snd1 snd2) Trd trd1 trd2
  let map f (fst, snd, trd) =
    (f Fst fst, f Snd snd, f Trd trd)

end

module Vect2_record = struct
  type t = { x : float; y : float; ph : unit }
  type elt = float
  type index = X | Y

  let fold_index init f { x; y } =
    f (init X x) Y y
  let fold_index_2 init f v1 v2 =
    f (init X v1.x v2.x) Y v1.y v2.y
  let map f { x; y } =
    { x = f X x; y = f Y y; ph = () }
end

module Vect3_record = struct
  type t = { x : float; y : float; z : float; ph : unit }
  type elt = float
  type index = X | Y | Z

  let fold_index init f { x; y; z } =
    f (f (init X x) Y y) Z z
  let fold_index_2 init f v1 v2 =
    f (f (init X v1.x v2.x) Y v1.y v2.y) Z v1.z v2.z
  let map f { x; y; z } =
    { x = f X x; y = f Y y; z = f Z z; ph = () }
end

module Vector_operations (V : Vector_base with type elt = float) = struct

  type elt = V.elt
  type t = V.t

  let norm v =
    let sum_sq =
      V.fold_index
        (fun _ elt -> elt *. elt)
        (fun acc _ elt -> acc +. elt *. elt)
        v
    in
    sqrt sum_sq

  let scale s v =
    V.map (fun _ x -> x *. s) v

  let dot v1 v2 =
    V.fold_index_2
      (fun _ f1 f2 -> f1 *. f2)
      (fun acc _ f1 f2 -> acc +. f1 *. f2) v1 v2

  let are_orthogonal v1 v2 =
    dot v1 v2 = 0. (* < epsilon ? *)

end

module IV2 = Vector_operations(Vect2)

module IV3 = Vector_operations(Vect3)

module IV2R = Vector_operations(Vect2_record)

module IV3R = Vector_operations(Vect3_record)

module VR2 = struct
  type t = Vect2_record.t = { x : float; y : float; ph : unit }
  type elt = float

  let norm { x; y } = sqrt (x *. x +. y *. y)
  let scale s { x; y } = { x = x *. s; y = y *. s; ph = () }
  let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y
  let are_orthogonal v1 v2 = dot v1 v2 = 0.

end


let (=!=) (a: float) b = if a = b then () else failwith ""

let _ =
  for i = 0 to 10_000_000; do
    let ans2 = (float i) *. (float i) +. (float (i+1)) *. (float (i+1)) in
    let x2 = { VR2.x = float i; VR2.y = float (i + 1); ph = () } in
    let y2 = (float i, float (i + 1)) in

    ans2 =!= IV2R.dot x2 x2;
    ans2 =!= VR2.dot x2 x2;
    ans2 =!= IV2.dot y2 y2;


    let ans3 = (float i) *. (float i) +. (float (i+1)) *. (float (i+1)) +. (float (i+2)) *. (float (i+2)) in
    let x3 = { Vect3_record.x = float i; y = float (i + 1); z = float (i + 2); ph = () } in
    let y3 = (float i, float (i + 1), float (i + 2)) in

    ans3 =!= IV3R.dot x3 x3;
    ans3 =!= IV3.dot y3 y3
  done
