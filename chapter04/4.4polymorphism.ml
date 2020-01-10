(* 4.1 *)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

(* 4.2 *)
let rec repeat f n x =
  if n > 0 then repeat f (n - 1) (f x) else x
let fib n =
  let (fibn, _) = repeat (fun (a, b) -> (b, a + b)) n (0, 1) in fibn

(* 4.3 *)
let id x = x
let ($) f g x = f (g x)
let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f
(* 'funny' returns a function that applies f, n times. *)

(* 4.4 *)
let s x y z = x z (y z)
let k x y = x
(* s k k 1 -> k 1 (k 1)
 *         -> k 1 (y -> 1)
 *         -> 1            *)

(* 4.5 *)
let twice f x = f (f x)
(* twice twice f x -> twice (f (f x))
 *                 -> f (f (f (f x))) *)

(* 4.6 *)
let f x y = k (s k k) x y
