type nat = Zero | OneMoreThan of nat

let rec to_nat = function
    0 -> Zero
  | n -> OneMoreThan (to_nat (n - 1))

let rec arabic_num = function
    Zero -> 0
  | OneMoreThan n' -> 1 + arabic_num n'

let rec add m n =
  match m with Zero -> n | OneMoreThan m' -> OneMoreThan (add m' n)

let rec mul m = function
    Zero -> Zero
  | OneMoreThan n' -> add m (mul m n')

let rec monus m n =
  match (m, n) with
    (Zero, _) -> Zero
  | (_, Zero) -> m
  | (OneMoreThan m', OneMoreThan n') -> monus m' n'

let rec minus m n =
  match (m, n) with
    (Zero, _) -> None
  | (_, Zero) -> Some m
  | (OneMoreThan m', OneMoreThan n') -> minus m' n'
