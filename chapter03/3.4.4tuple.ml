(* 3.6 *)
let geo_mean (x, y) = sqrt (x *. y)

let bmi (name, height, weight) =
  let index = weight /. (height *. height) in
  let state = if index < 18.5 then "やせています"
    else if index < 25.0 then "標準です"
    else if index < 30.0 then "肥満です"
    else "高度肥満です" in
  name ^ "さんは" ^ state

let sum_and_diff (x, y) = (x + y, x - y)
let f (a, b) = ((a + b) / 2, (a - b) / 2)
