(* Find the number of elements of a list. *)

let length lst =
  let rec count acc = function
    | [] -> acc
    | h :: t -> count (acc + 1) t
  in count 0 lst

let length lst = List.fold_left (fun a _ -> a + 1) 0 lst

let length lst = List.fold_right (fun _ a -> a + 1) lst 0
