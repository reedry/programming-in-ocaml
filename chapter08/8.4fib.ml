let while_fib x =
  let n, a, b = ref 1, ref 0, ref 1 in
  while (!n < x) do
    let tmp = !b in
    b := !a + !b;
    a := tmp;
    n := !n + 1
  done;
  !b

let for_fib x =
  let a, b = ref 0, ref 1 in
  for n = 1 to x - 1 do
    let tmp = !b in
    b := !a + !b;
    a := tmp;
  done;
  !b

