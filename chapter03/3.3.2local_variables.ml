(* 3.1 *)
let round x =
  let floor_of_x = floor x in
  int_of_float floor_of_x + if x -. floor_of_x < 0.5 then 0 else 1

let yen_of_dollar dollar = round (dollar *. 114.32)

let dollar_of_yen yen = float_of_int (round (float_of_int yen /. 114.32 *. 100.0)) /. 100.0

let exchange dollar =
  let yen = yen_of_dollar dollar in
  string_of_float dollar ^ " dollars are " ^ string_of_int yen ^ " yen."

let capitalize c =
  let int_c = int_of_char c in
  let capitalized_int = if 97 <= int_c && int_c <= 122 then int_c - 32 else
    int_c in
  char_of_int capitalized_int

(* 3.2 *)
let myand1 b1 b2 = if b1 then b2 else false
let myor1 b1 b2 = if b1 then true else b2

(* 3.3 *)
let myand2 b1 b2 = not (not b1 || not b2)
let myor2 b1 b2 = not (not b1 && not b2)

(* 3.4 *)
let q1 = let x = 1 in let x = 3 in let x = x + 2 in x * x
let q2 = let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y
let q3 = let x = 2 in let y = 3 in let y = x in let z = y in x * y * z

(* 3.5
 * let x = 1 and y = x ;; -> Error
 * let x = 1 let y = x ;; -> x = 1, y = 1 *)
