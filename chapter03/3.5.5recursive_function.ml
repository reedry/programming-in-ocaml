(* 3.7 *)
let rec pow1 (x, n) =
  if n = 0 then 1 else x * pow1 (x, n - 1)

let rec pow2 (x, n) =
  if n = 0 then 1
  else if n mod 2 = 0 then pow2 (x * x, n / 2)
  else x * pow2 (x * x, n / 2)

(* 3.8 *)
let iterpow (x, n) =
  let rec iter (n, res) =
    if n = 0 then res
    else iter (n - 1, x * res)
  in iter (n, 1)

(* 3.9 *)
let cond (b, e1, e2): int = if b then e1 else e2
let rec fact n = cond ((n = 1), 1, n * fact (n - 1))
(* fact 4 -> cond ((4 = 1), 1, 4 * fact 3)
 *  because of call-by-value, OCaml always evaluates fact
 *  before evaluating cond. This will be stack overflow. *)

(* 3.10 *)
let rec fib n =
  if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2)
(* fib 4 -> fib 3 + fib 2
 *       -> fib 1 + fib 2 + 1
 *       -> 1 + 1 + 1
 *       -> 3 *)

(* 3.11 *)
let rec gcd (a, b) =
  let (m, n) = if a < b then (a, b) else (b, a) in
  if m = n then m else gcd (n - m, m)

let rec comb (n, m) =
  if m = 0 || m = n then 1 else comb (n - 1, m) + comb (n - 1, m - 1)

let iterfib n =
  let rec iter (n, n1, n2) =
    if n = 1 then n1 else iter (n - 1, n2, n1 + n2) in
  iter (n, 1, 1)

let max_ascii s =
  let rec iter (i, max) =
    if i = 0 then max
    else let code = int_of_char s.[i - 1] in
    if code > max then iter (i - 1, code) else iter (i - 1, max)
  in char_of_int (iter (String.length s, 0))

(* 3.12 *)
let rec arctan n =
  if n < 0 then 0.0
  else arctan (n - 1) +. 1.0 /. (float_of_int (4 * n + 1))
                      -. 1.0 /. (float_of_int (4 * n + 3))
