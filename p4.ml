(* Find the number of elements of a list. *)

let length lst =
  let rec count acc = function
    | [] -> acc
    | h :: t -> count (acc + 1) t
  in count 0 lst
