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

(* Problem 03. Find the K'th element of a list. (easy) *)

let rec at (k: int) (xs: 'a list): 'a option =
  match xs with
  | [] -> None
  | x :: rest -> if k = 1 then Some x else at (k-1) rest 
;;

assert(at 3 ["a"; "b"; "c"; "d"; "e"] = Some "c");;
assert(at 3 ["a"] = None);;