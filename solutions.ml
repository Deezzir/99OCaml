(* Problem 01. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last (xs : 'a list): 'a option = 
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest
;;

assert(last ["a" ; "b" ; "c" ; "d"] = Some "d");;
assert(last [] = None);;