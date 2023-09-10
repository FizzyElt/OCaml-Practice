(* Tail fo List *)
let rec last = function [] -> None | x :: [] -> Some x | _ :: xs -> last xs

(* Last Two Elements of a List *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

(* N'th Element of a List *)
let rec at k = function
  | [] -> None
  | x :: xs -> if k = 0 then Some x else at (k - 1) xs

(* Length of a List *)
let length list =
  let rec aux n = function [] -> n | _ :: xs -> aux (n + 1) xs in
  aux 0 list

(* Reverse a List *)
let rev list =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: acc) xs in
  aux [] list

(* Palindrome *)
let is_palindrome list = list = List.rev list

type 'a node = One of 'a | Many of 'a node list

(* Flatten a List *)
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  aux [] list

(* Eliminate Duplicates *)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

(* Pack Consecutive Duplicates *)
let pack list =
  let rec aux group acc = function
    | [] -> []
    | [ a ] -> (a :: group) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: group) acc t else aux [] ((a :: group) :: acc) t
  in
  aux [] [] list

(* Run-Length Encoding *)
let encode list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | x :: y :: t ->
        if x = y then aux (count + 1) acc t else aux 0 ((count + 1, x) :: acc) t
  in
  List.rev (aux 0 [] list)

type 'a rle = One of 'a | Many of int * 'a

(* Modified Run-Length Encoding *)
let encode2 list =
  let to_rle count x = if count = 1 then One x else Many (count, x) in
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> to_rle (count + 1) x :: acc
    | x :: y :: t ->
        if x = y then aux (count + 1) acc t
        else aux 0 (to_rle (count + 1) x :: acc) t
  in
  List.rev (aux 0 [] list)

(* Decode a Run-Length Encoded List *)
let decode list =
  let rec many acc n x = if n = 0 then acc else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
  aux [] (List.rev list)

(* Duplicate the Elements of a List *)
let rec duplicate = function [] -> [] | x :: xs -> x :: x :: duplicate xs

let split list n =
  let rec aux n acc = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if n = 0 then (List.rev acc, l) else aux (n - 1) (h :: acc) t
  in
  aux n [] list

let remove_at n list =
  let rec aux i acc = function
    | [] -> acc
    | x :: xs ->
        if i + 1 = n then aux (i + 1) acc xs else aux (i + 1) (x :: acc) xs
  in
  aux 0 [] list

let rec insert_at x n = function
  | [] -> [ x ]
  | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n - 1) t

let range a b =
  let rec aux a b = if a > b then [] else a :: aux (a + 1) b in

  if a > b then List.rev (aux b a) else aux a b
