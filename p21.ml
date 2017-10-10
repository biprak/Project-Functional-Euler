(* Insert an element at a given position into a list *)

let rec insert_at e n = function
  | [] -> if n >= 0 then e :: [] else []
  | h :: t ->
    if n = 0 then e :: h :: t else h :: insert_at e (n - 1) t
