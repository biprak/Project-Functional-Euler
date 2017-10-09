(* Find the last but one (last and penultimate) elements of a list. *)

let rec last_two = function
  | [] | (_ :: []) -> None
  | a :: b :: [] -> Some (a,b)
  | _ :: t -> last_two t

(* another solution *)

let rec last_two' = function
  | [] | _ :: [] -> None
  | a :: b :: t -> begin
      match last_two t with
      | None -> Some (a,b)
      | Some (_, _) -> last_two' t
    end
