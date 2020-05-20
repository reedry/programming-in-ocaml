module type QUEUE =
  sig
    type 'a t
    exception Empty
    val empty : 'a t
    val add : 'a t -> 'a -> 'a t
    val take : 'a t -> 'a * 'a t
    val peek : 'a t -> 'a
  end

module Queue1 : QUEUE =
  struct
    type 'a t = 'a list
    exception Empty
    let empty = []
    let add q x = q @ x :: []
    let take = function [] -> raise Empty | x :: rest -> x, rest
    let peek = function [] -> raise Empty | x :: rest -> x
  end

module Queue2 : QUEUE =
  struct
    type 'a t = Queue of ('a list * 'a list)
    exception Empty
    let empty = Queue ([], [])
    let add q x =
      match q with
        Queue ([], []) -> Queue (x :: [], [])
      | Queue (y, z) -> Queue (y, x :: z)
    let take = function
        Queue ([], _) -> raise Empty
      | Queue (x :: [], y) -> x, Queue (List.rev y, [])
      | Queue (x :: rest, y) -> x, Queue (rest, y)
    let peek = function
        Queue ([], _) -> raise Empty
      | Queue (x :: _, _) -> x
  end
