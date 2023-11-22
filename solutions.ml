(* Problem 01. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last (xs : 'a list): 'a option = 
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest
;;

(* Problem 02. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two (xs: 'a list): ('a * 'a) option = 
  match xs with
  | [] | [_] -> None
  | [x; y] -> Some(x, y)
  | _ :: rest -> last_two rest
;;

(* Problem 03. Find the K'th element of a list. (easy) *)
let rec at (k: int) (xs: 'a list): 'a option =
  match xs with
  | [] -> None
  | x :: rest -> if k = 1 then Some x else at (k-1) rest 
;;

(* Problem 04. Find the number of elements of a list. (easy) *)
let rec length_naive (xs: 'a list): int =
  match xs with
  | [] -> 0
  | _ :: rest -> length_naive rest + 1
;;

let length (xs: 'a list): int =
  let rec _length (acc: int) (xs: 'a list) =
    match xs with
    | [] -> acc
    | _ :: rest -> _length (acc+1) rest
  in _length 0 xs
;;

(* Problem 05. Reverse a list. (easy) *)
let rev (xs: 'a list): 'a list =
  let rec _rev acc = function
    | [] -> acc
    | h :: t -> _rev (h :: acc) t in
  _rev [] xs
;;

(* Problem 06. Find out whether a list is a palindrome. (easy) *)
let is_palindrome (xs: 'a list): bool = 
  let rec _rev acc = function
    | [] -> acc
    | h :: t -> _rev (h :: acc) t in
  _rev [] xs = xs
;;

let () = 
  print_endline "Checking solution for Problem 01";
  assert(last ["a" ; "b" ; "c" ; "d"] = Some "d");
  assert(last [] = None);

  print_endline "Checking solution for Problem 02";
  assert(last_two ["a"; "b"; "c"; "d"] = Some ("c", "d"));
  assert(last_two ["a"] = None);

  print_endline "Checking solution for Problem 03";
  assert(at 3 ["a"; "b"; "c"; "d"; "e"] = Some "c");
  assert(at 3 ["a"] = None);

  print_endline "Checking solution for Problem 04";
  assert(length ["a"; "b"; "c"] = 3);
  assert(length [] = 0);

  print_endline "Checking solution for Problem 05";
  assert(rev ["a"; "b"; "c"] = ["c"; "b"; "a"]);

  print_endline "Checking solution for Problem 06";
  assert(is_palindrome ["x"; "a"; "m"; "a"; "x"] = true);
  assert(not (is_palindrome ["a"; "b"]) = true);
;;