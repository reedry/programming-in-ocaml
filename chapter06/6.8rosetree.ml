type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list

let rtree =
  RBr ("a", [
    RBr ("b", [
      RBr ("c", [RLf]);
      RLf;
      RBr ("d", [RLf])]);
      RBr ("e", [RLf]);
      RBr ("f", [RLf])])

let rec tree_of_rtree = function
    RLf -> Br (None, Lf, Lf)
  | RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
    [] -> Lf
  | rtree :: rest ->
      (match tree_of_rtree rtree with
        Br (a, left, Lf) -> Br (a, left, tree_of_rtreelist rest)
      | _ -> Lf)

let rec rtree_of_tree = false
