(* Reverse a list *)

let rev lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] lst
  
