(* Write a function last : 'a list -> 'a option that returns the last element of a list. *)

let rec last = function
  | [] -> None
  | h :: t -> begin
      match last t with
      | None -> Some h
      | Some e -> last t
    end


(* another solution *)

let rec last' = function
  | [] -> None
  | h :: [] -> Some h
  | h :: t -> last' t 
