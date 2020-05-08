let prod_list l =
  let rec prod_list' = function
      [] -> 1
    | 0 :: xs -> raise (Failure "Zero")
    | x :: xs -> x * prod_list' xs
  in try prod_list' l with Failure _ -> 0
