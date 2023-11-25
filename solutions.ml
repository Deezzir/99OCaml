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
    | x :: rest ->
        if cnt = n then _drop 1 rest else x :: _drop (cnt + 1) rest
  in
  _drop 1 xs

(* Main *)
let () =
  print_endline "Checking solution for Problem 01";
  assert (last [ "a"; "b"; "c"; "d" ] = Some "d");
  assert (last [] = None);

  print_endline "Checking solution for Problem 02";
  assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
  assert (last_two [ "a" ] = None);

  print_endline "Checking solution for Problem 03";
  assert (at 3 [ "a"; "b"; "c"; "d"; "e" ] = Some "c");
  assert (at 3 [ "a" ] = None);

  print_endline "Checking solution for Problem 04";
  assert (length [ "a"; "b"; "c" ] = 3);
  assert (length [] = 0);

  print_endline "Checking solution for Problem 05";
  assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]);

  print_endline "Checking solution for Problem 06";
  assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true);
  assert ((not (is_palindrome [ "a"; "b" ])) = true);

  print_endline "Checking solution for Problem 07";
  assert (
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    = [ "a"; "b"; "c"; "d"; "e" ]);

  print_endline "Checking solution for Problem 08";
  assert (
    compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ "a"; "b"; "c"; "a"; "d"; "e" ]);

  print_endline "Checking solution for Problem 09";
  assert (
    pack [ "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e" ]
    = [
        [ "a"; "a"; "a" ];
        [ "b" ];
        [ "c"; "c" ];
        [ "a"; "a" ];
        [ "d"; "d" ];
        [ "e"; "e"; "e" ];
      ]);

  print_endline "Checking solution for Problem 10";
  assert (
    encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]);

  print_endline "Checking solution for Problem 11";
  assert (
    encode_rle
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]);

  print_endline "Checking solution for Problem 12";
  assert (
    decode_rle
      [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]
    = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]);

  print_endline "Checking solution for Problem 13";
  assert (
    encode_dir
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]);

  print_endline "Checking solution for Problem 14";
  assert (
    duplicate [ "a"; "b"; "c"; "c"; "d" ]
    = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]);

  print_endline "Checking solution for Problem 15";
  assert (
    replicate [ "a"; "b"; "c" ] 3
    = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]);

  print_endline "Checking solution for Problem 15";
  assert (
    drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ])
