type intseq = Cons of int * (int -> intseq)

let rec f x = Cons (x + 1, f)
let rec step n x = Cons (x + n, step (n + 1))

let rec fib_seq n x = Cons (x + n, fib_seq x)
let fib = fib_seq 1 0

let rec nthseq n (Cons (x, f)) =
  if n = 1 then x
  else nthseq (n - 1) (f x)
