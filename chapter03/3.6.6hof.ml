(* 3.13 *)
let rec pow n x =
  if n = 1 then x else x * pow (n - 1) x
let cube = pow 3

let rec pow2 x n =
  if n = 1 then x else x * pow2 x (n - 1)
let cube2 x = pow2 x 3

(* 3.14 *)
let integral f a b =
  let n = 1000000 in
  let delta = (b -. a) /. float_of_int n in
  let rec loop i s =
    if i = n then s
    else
      let fi = float_of_int i in
      let trapezoid = (f (a +. fi *. delta) +. f (a +. (fi +. 1.) *. delta)) *.
        delta /. 2.
      in loop (i + 1) (s +. trapezoid)
  in loop 0 0.

let s = integral sin 0. 3.1415926535

(* 3.15 *)
(* int -> int -> int -> int *)
let add_three_ints a b c = a + b + c
(* (int -> int) -> int -> int *)
let rec sigma f n =
  if n = 1 then f 1 else f n + sigma f (n - 1)
(* (int -> int -> int) -> int *)
let apply_to_0_and_1 (f: int -> int -> int) = f 0 1
(* TODO: proper example of this function  *)
