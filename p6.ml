let rev lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] lst


(* Find out whether a list is a palindrome *)
let is_palindrome lst =
  lst = rev lst 
