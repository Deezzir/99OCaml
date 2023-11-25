(* Problem 01. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last (xs : 'a list) : 'a option =
  match xs with [] -> None | [ x ] -> Some x | _ :: rest -> last rest

(* Problem 02. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two (xs : 'a list) : ('a * 'a) option =
  match xs with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: rest -> last_two rest

(* Problem 03. Find the K'th element of a list. (easy) *)
let rec at (k : int) (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | x :: rest -> if k = 1 then Some x else at (k - 1) rest

(* Problem 04. Find the number of elements of a list. (easy) *)
let rec length' (xs : 'a list) : int =
  match xs with [] -> 0 | _ :: rest -> length' rest + 1

let length (xs : 'a list) : int =
  let rec _length acc = function
    | [] -> acc
    | _ :: rest -> _length (acc + 1) rest
  in
  _length 0 xs

(* Problem 05. Reverse a list. (easy) *)
let rec rev' (xs : 'a list) : 'a list =
  match xs with [] -> [] | x :: rest -> rev' rest @ [ x ]

let rev (xs : 'a list) : 'a list =
  let rec _rev acc = function [] -> acc | h :: t -> _rev (h :: acc) t in
  _rev [] xs

(* Problem 06. Find out whether a list is a palindrome. (easy) *)
let is_palindrome (xs : 'a list) : bool =
  let rec _rev acc = function [] -> acc | h :: t -> _rev (h :: acc) t in
  _rev [] xs = xs

(* Problem 07. Flatten a nested list structure. (medium) *)
type 'a node = One of 'a | Many of 'a node list

let flatten' (xs : 'a node list) : 'a list =
  let rec _flatten acc = function
    | [] -> acc
    | [ One x ] -> x :: acc
    | [ Many x ] -> _flatten acc x
    | x :: rest -> _flatten (_flatten acc [ x ]) rest
  in
  List.rev (_flatten [] xs)

let flatten (xs : 'a node list) : 'a list =
  let rec _flatten acc = function
    | [] -> acc
    | One x :: rest -> _flatten (x :: acc) rest
    | Many x :: rest -> _flatten (_flatten acc x) rest
  in
  List.rev (_flatten [] xs)

(* Problem 08. Eliminate consecutive duplicates of list elements. (medium) *)
let compress' (xs : 'a list) : 'a list =
  let rec _compress acc = function
    | [] -> acc
    | x :: rest ->
        if (not (List.is_empty rest)) && x = List.hd rest then
          _compress acc rest
        else x :: _compress acc rest
  in
  _compress [] xs

let rec compress (xs : 'a list) : 'a list =
  match xs with
  | x :: (y :: _ as rest) -> if x = y then compress rest else x :: compress rest
  | rest -> rest

(* Problem 09. Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack (xs : 'a list) : 'a list list =
  let rec _pack inner acc = function
    | [] -> acc
    | x :: (y :: _ as rest) ->
        let inner = x :: inner in
        if x = y then _pack inner acc rest else _pack [] (inner :: acc) rest
    | [ x ] -> (x :: inner) :: acc
  in
  List.rev (_pack [] [] xs)

(* Problem 10. Run-length encoding of a list. (easy) *)
let encode' xs = List.map (fun l -> (List.length l, List.hd l)) (pack xs)

let encode (xs : 'a list) : (int * 'a) list =
  let rec _encode acc = function
    | [] -> []
    | [ x ] -> [ (acc + 1, x) ]
    | x :: (y :: _ as rest) ->
        if x = y then _encode (acc + 1) rest else (acc + 1, x) :: _encode 0 rest
  in
  _encode 0 xs

(* Problem 11. Modified run-length encoding. (easy) *)
type 'a rle = One of 'a | Many of int * 'a

let encode_rle' xs =
  List.map
    (fun l ->
      let len = List.length l in
      let v = List.hd l in
      match len with 1 -> One v | l -> Many (l, v))
    (pack xs)

let encode_rle (xs : 'a list) : 'a rle list =
  let _make cnt x = if cnt = 1 then One x else Many (cnt, x) in
  let rec _encode_rle cnt acc = function
    | [] -> []
    | [ x ] -> _make (cnt + 1) x :: acc
    | x :: (y :: _ as rest) ->
        if x = y then _encode_rle (cnt + 1) acc rest
        else _encode_rle 0 (_make (cnt + 1) x :: acc) rest
  in
  List.rev (_encode_rle 0 [] xs)

(* Problem 12. Decode a run-length encoded list. (medium) *)
let decode_rle (xs : 'a rle list) : 'a list =
  let rec _decode_rle acc = function
    | [] -> acc
    | Many (l, x) :: rest when l != 1 ->
        _decode_rle (x :: acc) (Many (l - 1, x) :: rest)
    | (One x | Many (_, x)) :: rest -> _decode_rle (x :: acc) rest
  in
  List.rev (_decode_rle [] xs)

(* Problem 13. Run-length encoding of a list (direct solution). (medium) *)
let encode_dir (xs : 'a list) : 'a rle list =
  let _make cnt x = if cnt = 0 then One x else Many (cnt + 1, x) in
  let rec _encode_dir cnt acc = function
    | [] -> []
    | [ x ] -> _make cnt x :: acc
    | x :: (y :: _ as rest) ->
        if x = y then _encode_dir (cnt + 1) acc rest
        else _encode_dir 0 (_make cnt x :: acc) rest
  in
  List.rev (_encode_dir 0 [] xs)

(* Problem 14. Duplicate the elements of a list. (easy) *)
let rec duplicate (xs : 'a list) : 'a list =
  match xs with [] -> [] | x :: rest -> x :: x :: duplicate rest

(* Problem 15. Replicate the elements of a list a given number of times. (medium) *)
let replicate' list n =
  let rec _prepend n acc x =
    if n = 0 then acc else _prepend (n - 1) (x :: acc) x
  in
  List.fold_left (_prepend n) [] (List.rev list)

let replicate (xs : 'a list) (n : int) : 'a list =
  let rec _replicate cnt acc = function
    | [] -> acc
    | x :: rest ->
        if cnt > 0 then _replicate (cnt - 1) (x :: acc) (x :: rest)
        else _replicate n acc rest
  in
  List.rev (_replicate n [] xs)

(* Problem 16. Drop every N'th element from a list. (medium) *)
let drop (xs : 'a list) (n : int) : 'a list =
  let rec _drop cnt = function
    | [] -> []
    | x :: rest -> if cnt = n then _drop 1 rest else x :: _drop (cnt + 1) rest
  in
  _drop 1 xs

(* Problem 17. Split a list into two parts; the length of the first part is given. (easy) *)
let split' (xs : 'a list) (n : int) : 'a list * 'a list =
  if n >= List.length xs then (xs, [])
  else
    let rec _split cnt acc = function
      | [] -> acc
      | x :: rest ->
          if cnt = n then
            let hd, _ = acc in
            (x :: hd, rest)
          else
            let hd, tl = _split (cnt + 1) acc rest in
            (x :: hd, tl)
    in
    _split 1 ([], []) xs

let split (xs : 'a list) (n : int) : 'a list * 'a list =
  let rec _split cnt acc = function
    | [] -> (List.rev acc, [])
    | x :: rest as l ->
        if cnt = n then (List.rev acc, l) else _split (cnt + 1) (x :: acc) rest
  in
  _split 0 [] xs

(* Problem 18. Extract a slice from a list. (medium) *)
let slice' list i k =
  let rec fold_until f acc n = function
    | [] -> (acc, [])
    | h :: t as l ->
        if n = 0 then (acc, l) else fold_until f (f acc h) (n - 1) t
  in
  let _, list = fold_until (fun _ _ -> []) [] i list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
  List.rev taken

let slice (xs : 'a list) (s : int) (e : int) : 'a list =
  let rec _slice cnt acc = function
    | [] -> List.rev acc
    | x :: rest ->
        if cnt > e then List.rev acc
        else if cnt >= s then _slice (cnt + 1) (x :: acc) rest
        else _slice (cnt + 1) acc rest
  in
  _slice 0 [] xs

(* Problem 19. Rotate a list N places to the left. (medium) *)
let rotate (xs : 'a list) (r : int) : 'a list =
  let len = List.length xs in
  let n = if len = 0 then 0 else ((r mod len) + len) mod len in
  if n = 0 then xs
  else
    let rec _rotate cnt acc = function
      | [] -> List.rev acc
      | x :: rest ->
          if cnt = n then x :: _rotate cnt acc rest
          else _rotate (cnt + 1) (x :: acc) rest
    in
    _rotate 0 [] xs

(* Problem 20. Remove the K'th element from a list. (easy) *)
let rec remove_at (at : int) (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | x :: rest -> if at = 0 then rest else x :: remove_at (at - 1) rest

(* Problem 21. Insert an element at a given position into a list. (easy) *)
let insert_at (_ : 'a) (_ : int) (_ : 'a list) : 'a list = failwith "TODO"

(* Problem 22. Create a list containing all integers within a given range. (easy) *)
let range (_ : int) (_ : int) : int list = failwith "TODO"

(* Problem 23. Extract a given number of randomly selected elements from a list. (medium) *)
let rand_select (_ : 'a list) (_ : int) : 'a list = failwith "TODO"

(* Problem 24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let lotto_select (_ : int) (_ : int) : int list = failwith "TODO"

(* Problem 25. Generate a random permutation of the elements of a list. (easy) *)
let permutation (_ : 'a list) : 'a list = failwith "TODO"
