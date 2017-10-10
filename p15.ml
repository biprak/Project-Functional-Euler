(* Replicate the elements of a list a given number of times *)

let replicate lst n =
  let rec prepend n acc a =
    if n = 0 then acc else prepend (n - 1) (a :: acc) a  in
  List.fold_left (prepend n) [] (List.rev lst)
  
