(* tail recursive method that accumulates all previous numbers
 * in the fibonacci series until n decrements 1 *)
let rec fib_helper n pp p =
  if n = 1 then p
  else fib_helper (n-1) p (pp + p)

(* add all even fibs from the start'th fib to the fib equalling 4 million *)
let rec add_even_fibs start pp p acc =
  let fib = fib_helper start pp p in
  if (fib > 4000000) then acc
  else if (fib mod 2 = 0) then add_even_fibs (start + 1) pp p (acc + fib)
  else add_even_fibs (start + 1) pp p (acc)

let answer = add_even_fibs 1 0 1 0
