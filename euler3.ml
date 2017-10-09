(* taken from https://ocaml.org/learn/tutorials/99problems.html *)
let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2

open Int64

let rec all_prime_factors start n acc =
  if List.length acc > 1 then acc
  else if
    let is_prime = is_prime (to_int n) in
    (is_prime && rem start n = zero) then all_prime_factors start (div start n) (n :: acc)
  else all_prime_factors start (sub n one) acc

let answer = List.nth (all_prime_factors 600851475143L 600851475143L []) 1
