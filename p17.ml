(* Split a list into two parts; the length of the first part is given. *)

let split lst n =
  let rec aux count n (acc1, acc2)  = function
    | h :: t ->
      if count <= n then aux (count + 1) n (h :: acc1, acc2) t
      else aux (count + 1) n (acc1, h :: acc2) t
    | x -> (List.rev acc1, List.rev acc2)
  in aux 1 n ([], []) lst


(* another solution *)

let split' lst n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l ->
      if i = 0 then List.rev acc, l
      else aux (i - 1) (h :: acc) t
  in aux n [] lst
