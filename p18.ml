(* Extract a slice from a list. (medium) *)

let slice lst i k =
  let rec aux curr acc = function
    | [] -> acc
    | h :: t ->
      if (curr >= i && curr <= k) then
        aux (curr + 1) (h :: acc) t
      else
        aux (curr + 1) acc t
  in
  List.rev (aux 0 [] lst)
