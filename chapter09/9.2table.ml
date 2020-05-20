module type TABLE2 =
  sig
    type ('a, 'b) t
    val empty : ('a, 'b) t
    val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    val retrieve : 'a -> ('a, 'b) t -> 'b option
    val dump : ('a, 'b) t -> ('a * 'b) list
  end

module TableBT : TABLE2 =
  struct
    type ('a, 'b) t = Lf | Br of ('a * 'b) * ('a, 'b) t * ('a, 'b) t

    let empty = Lf

    let rec add key datum = function
        Lf -> Br ((key, datum), Lf, Lf)
      | Br ((k, d), left, right) as whole when k = key -> whole
      | Br ((k, d), left, right) when k < key ->
          Br((k, d), left, add key datum right)
      | Br ((k, d), left, right) -> Br((k, d), add key datum left, right)

    let rec retrieve key = function
        Lf -> None
      | Br ((k, d), left, right) ->
          if k = key then Some d
          else if key < k then retrieve key left else retrieve key right

    let rec dump = function
        Lf -> []
      | Br ((k, d), left, right) -> dump left @ (k, d) :: dump right
  end
