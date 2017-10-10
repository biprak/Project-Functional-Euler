(* Eliminate consecutive duplicates of list elements. *)
let rev lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] lst

let compress lst =
  let rec aux acc = function
    | [] -> acc
    | a :: (b :: c as t)->
      if (a <> b) then aux (a :: acc) t
      else aux (acc) t
    | a :: [] -> a :: acc
  in rev (aux [] lst)

(* non tail recursive another  solution *)
let rec compress' = function
  | a :: (b :: c as t) -> if a = b then compress' t else a :: compress' t
  | smaller -> smaller
