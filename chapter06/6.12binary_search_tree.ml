type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec add' x = function
    Lf -> Br (x, Lf, Lf)
  | (Br (y, left, right) as whole) when x = y -> whole
  | Br (y, left, right) when x < y -> Br (y, add' x left, right)
  | Br (y, left, right) -> Br (y, left, add' x right)


let t01 = Lf |> add' 1 |> add' 2 |> add' 3 |> add' 4
let t02 = Lf |> add' 1 |> add' 3 |> add' 2 |> add' 4
let t03 = Lf |> add' 1 |> add' 4 |> add' 2 |> add' 3
let t04 = Lf |> add' 1 |> add' 4 |> add' 3 |> add' 2
let t05 = Lf |> add' 2 |> add' 1 |> add' 3 |> add' 4
let t06 = Lf |> add' 2 |> add' 1 |> add' 4 |> add' 3
let t07 = Lf |> add' 3 |> add' 4 |> add' 1 |> add' 2
let t08 = Lf |> add' 3 |> add' 4 |> add' 2 |> add' 1
let t09 = Lf |> add' 4 |> add' 3 |> add' 2 |> add' 1
let t10 = Lf |> add' 4 |> add' 2 |> add' 1 |> add' 3
let t11 = Lf |> add' 4 |> add' 1 |> add' 3 |> add' 2
let t12 = Lf |> add' 4 |> add' 1 |> add' 2 |> add' 3

