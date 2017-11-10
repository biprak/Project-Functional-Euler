(* 16. Drop every N'th element from a list. (medium) *)

let drop lst n =
  let rec aux curr acc= function
    | [] -> acc
    | h :: t ->
      if curr = n then aux 1 acc t
      else aux (curr + 1) (h :: acc) t
  in
  List.rev (aux 1 [] lst)
