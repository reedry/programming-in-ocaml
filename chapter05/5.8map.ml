let rec map f = function
    [] -> []
  | x :: rest -> f x :: map f rest

let map2 f l =
  let rec tailmap tail f = function
      [] -> tail
    | x :: rest -> tailmap ((f x) :: tail) f rest
  in
  List.rev (tailmap [] f l)
