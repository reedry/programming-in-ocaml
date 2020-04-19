let squares r =
  let rec find_x tail = function
    x when x*x > r -> tail
  | x ->
      let rec find_y = function
        y when y > x -> None
      | y when x*x + y*y = r -> Some (x, y)
      | y -> find_y (y + 1)
      in
      match find_y 0 with
        None -> find_x tail (x + 1)
      | Some tup -> find_x (tup :: tail) (x + 1)
  in
  find_x [] 0
