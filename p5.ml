(* Reverse a list *)

let rev lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] lst

let rev lst = List.fold_left (fun a x -> x :: a) [] lst

let rev lst = List.fold_right (fun x a -> x :: a ) (List.rev lst) []
