(* Duplicate the elements of a list. *)

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t 
