type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec size = function
    Lf -> 0
  | Br (_, left, right) -> 1 + size left + size right

let rec depth = function
    Lf -> 0
  | Br (_, left, right) -> 1 + max (depth left) (depth right)

let rec comptree x = function
    0 -> Lf
  | n -> Br (x, comptree x (n - 1), comptree x (n - 1))

let comptree' n =
  let rec comptree_rec x = function
      0 -> Lf
    | d -> Br (x, comptree_rec (x * 2) (d - 1), comptree_rec (x * 2 + 1) (d - 1))
  in
  comptree_rec 1 n

let rec preord t l =
  match t with
    Lf -> l
  | Br (x, left, right) -> x :: (preord left (preord right l))

let rec inord t l =
  match t with
    Lf -> l
  | Br (x, left, right) -> inord left (x :: inord right l)

let rec postord t l =
  match t with
    Lf -> l
  | Br (x, left, right) -> postord left (postord right (x :: l))
