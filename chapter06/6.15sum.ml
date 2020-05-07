type ('a, 'b) sum = Left of 'a | Right of 'b

let f1 = function
    (a, Left b) -> Left (a, b)
  | (a, Right c)-> Right (a, c)

let f2 = function
    Left (a, b) -> (a, Left b)
  | Right (a, c) -> (a, Right c)

let f3 = function
    (Left a, Left c) -> Left (Left (a, c))
  | (Right b, Left c) -> Left (Right (b, c))
  | (Left a, Right d) -> Right (Left (a, d))
  | (Right b, Right d) -> Right (Right (b, d))

let f4 = function
    Left (Left (a, c)) -> (Left a, Left c)
  | Left (Right (b, c)) -> (Right b, Left c)
  | Right (Left (a, d)) -> (Left a, Right d)
  | Right (Right (b, d)) -> (Right b, Right d)

let f5 (fl, fr) = function
    Left a -> fl a
  | Right c -> fr c

let f6 f = ((fun a -> f (Left a)), (fun b -> f (Right b)))

let f7 f a =
  match f with
    Left fb -> Left (fb a)
  | Right fc -> Right (fc a)
