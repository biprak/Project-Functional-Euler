(* Duplicate the elements of a list. *)

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

(* using fold_left *)
let duplicate lst = List.fold_left (fun a x -> x :: x :: a) [] (List.rev lst)

(* using fold_right *)
let duplicate lst = List.fold_right (fun x a -> x :: x :: a) lst []
