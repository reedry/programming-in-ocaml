type intseq = Cons of int * (int -> intseq)

let rec nthseq n (Cons (x, f)) =
  if n = 1 then x
  else nthseq (n - 1) (f x)

let is_prime x =
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n - 1))
  in not (is_divisible_from_2_to (x - 1))

let is_prime1 x =
  let rec is_divisible_from_2_to n =
    (n < x) && ((x mod n = 0) || is_divisible_from_2_to (n + 1))
  in not (is_divisible_from_2_to 2)

let is_prime2 x =
  let sqrt_x = int_of_float (floor (float_of_int x)) in
  let rec is_divisible_from_2_to n =
    (n < sqrt_x) && ((x mod n = 0) || is_divisible_from_2_to (n + 1))
  in not (is_divisible_from_2_to 2)

let rec next_prime x =
  if is_prime (x + 1) then x + 1 else next_prime (x + 1)

let rec prime_seq x =
  if is_prime2 (x + 1) then Cons (x + 1, prime_seq) else prime_seq (x + 1)

let is_prime3 primes x =
  let rec is_divisible_by = function
      [] -> false
    | y :: ys -> (x mod y = 0) || is_divisible_by ys
  in not (is_divisible_by primes)

let is_prime4 primes x =
  let sqrt_x = int_of_float (floor (float_of_int x)) in
  let rec is_divisible_by = function
      [] -> false
    | y :: ys -> (y < sqrt_x) && ((x mod y = 0) || is_divisible_by ys)
  in not (is_divisible_by (List.rev primes))

let rec prime_seq2 primes x =
  if is_prime4 primes (x + 1) then Cons (x + 1, prime_seq2 ((x + 1) :: primes))
    else prime_seq2 primes (x + 1)
