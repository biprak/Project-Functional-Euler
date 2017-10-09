(* tail recursive method *)
let rec mult_of_three_five start acc =
  match start with
  | 0 -> acc
  | n ->
    if (n mod 3 = 0 || n mod 5 = 0) then mult_of_three_five (n - 1) (acc + n)
    else mult_of_three_five (n - 1) acc

let answer = mult_of_three_five 999 0
