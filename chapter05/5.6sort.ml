let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed in
  t -. m *. floor (t /. m)

let rec randlist n seed tail =
  if n = 0 then (seed, tail)
  else randlist (n - 1) (nextrand seed) (seed :: tail)


let rec quick_sort tail = function
  [] -> tail
| [n] -> n :: tail
| pivot :: rest ->
    let rec partition left right = function
      [] -> (quick_sort (pivot :: (quick_sort tail right)) left)
    | y :: ys ->
        if pivot < y then partition left (y :: right) ys
                     else partition (y :: left) right ys
    in
    partition [] [] rest

