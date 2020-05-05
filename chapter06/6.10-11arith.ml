type arith = Const of int | Add of arith * arith | Mul of arith * arith

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5))

let rec eval = function
    Const n -> n
  | Mul (exp1, exp2) -> (eval exp1) * (eval exp2)
  | Add (exp1, exp2) -> (eval exp1) + (eval exp2)

let rec string_of_arith = function
    Const n -> string_of_int n
  | Mul ((Add _ as a), (Add _ as b))
    -> "(" ^ string_of_arith a ^ ")*(" ^ string_of_arith b ^ ")"
  | Mul (exp1, exp2) -> string_of_arith exp1 ^ "*" ^ string_of_arith exp2
  | Add (exp1, exp2) -> string_of_arith exp1 ^ "+" ^ string_of_arith exp2

let rec expand = function
    Mul (Add (a, b), Add (c, d))
      -> Add (Add (Mul (expand a, expand c), Mul(expand a, expand d)),
          Add (Mul (expand b, expand c), Mul (expand b, expand d)))
  | Mul (a, b) -> Mul (expand a, expand b)
  | Add (a, b) -> Add (expand a, expand b)
  | Const n -> Const n
