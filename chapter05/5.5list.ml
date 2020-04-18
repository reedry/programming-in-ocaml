(* 5.2 *)
(* 1 *)
let rec downto1 = function
    0 -> []
  | n -> n :: downto1 (n - 1)

(* 2 *)
let rec roman def n =
  let rec lookup ls n =
    match ls with
      [] -> (0, "")
    | (num, str) :: rest when num <= n -> (num, str)
    | _ :: rest -> lookup rest n
  in match n with
    0 -> ""
  | n -> let (num, str) = lookup def n in
    str ^ (roman def (n - num))

(* 3 *)
let rec nested_length = function
    [] -> 0
  | first :: rest -> List.length first + nested_length rest

(* 4 *)
let rec concat = function
    [] -> []
  | first :: rest ->
      let rec concat_sub a b = match a with
          [] -> b
        | f :: r -> f :: concat_sub r b
      in concat_sub first (concat rest)

(* 5 *)
let rec zip a b = match (a, b) with
  ([], []) -> []
| ([], bf :: br) -> []
| (af :: ar, []) -> []
| (af :: ar, bf :: br) -> (af, bf) :: zip ar br

(* 6, to be improved *)
let unzip l =
  let rec iter lz la lb = match lz with
    [] -> (la, lb)
  | (a, b) :: rest -> iter rest (a :: la) (b :: lb)
  in let (la, lb) = iter l [] [] in
  (List.rev la, List.rev lb)

(* 7 *)
let rec filter f = function
    [] -> []
  | first :: rest -> if f first then first :: (filter f rest)
                                else filter f rest

(* 8 *)
let rec take n l = match (n, l) with
  (0, _) -> []
| (n, []) -> []
| (n, first :: rest) -> first :: (take (n - 1) rest)

let rec drop n l = match (n, l) with
  (0, l) -> l
| (n, []) -> []
| (n, _ :: rest) -> drop (n - 1) rest

(* 9 *)
let max_list = function
  [] -> 0
| first :: rest ->
    let rec max_rec mx = function
      [] -> mx
    | f :: r -> if f > mx then max_rec f r else max_rec mx r
    in max_rec first rest

(* 5.3 *)
(* 1 *)
let rec mem s a = match s with
  [] -> false
| first :: rest ->
    if first = a then true else mem rest a

(* 2 *)
let intersect s1 s2 = filter (mem s1) s2

(* 3 *)
let union s1 s2 =
  let rec not_mem s a = match s with
    [] -> true
  | first :: rest -> if first = a then false else not_mem rest a
  in
  s1 @ (filter (not_mem s1) s2)

(* 4 *)
let diff s1 s2 =
  let rec not_mem s a = match s with
    [] -> true
  | first :: rest -> if first = a then false else not_mem rest a
  in
  filter (not_mem s2) s1

(* 5.4 *)
(* map (fun x -> f (g x)) l *)

(* 5.5 *)
let rec fold_right f l e =
  match l with
    [] -> e
  | x :: rest -> f x (fold_right f rest e)

let concat_fold ll = fold_right (fun x a -> x @ a) ll []

let forall_fold p l = fold_right (fun x b -> p x && b) l true

let exists_fold p l = fold_right (fun x b -> p x || b) l false
