(* Problem 01. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last (xs : 'a list): 'a option = 
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest
;;

assert(last ["a" ; "b" ; "c" ; "d"] = Some "d");;
assert(last [] = None);;

(* Problem 02. Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two (xs: 'a list): ('a * 'a) option = 
  match xs with
  | [] | [_] -> None
  | [x; y] -> Some(x, y)
  | _ :: rest -> last_two rest
;;

assert(last_two ["a"; "b"; "c"; "d"] = Some ("c", "d"));;
assert(last_two ["a"] = None);;