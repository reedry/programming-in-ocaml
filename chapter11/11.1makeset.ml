module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module MakeSet (Order : OrderedType) =
  struct
    type elt = Order.t
    type t = elt list

    let empty = []

    let rec mem elt = function
        [] -> false
      | x :: rest ->
          let r = Order.compare elt x in
          (r = 0) || ((r > 0) && mem elt rest)

    let rec add elt = function
        [] -> [elt]
      | x :: rest as s ->
          match Order.compare elt x with
            0 -> s
          | r when r < 0 -> elt :: s
          | _ -> x :: add elt rest
  end
