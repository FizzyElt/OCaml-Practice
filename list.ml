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

(* Replicate the Elements of a List a Given Number of Times *)
let replicate list n =
  let rec prepend n acc x =
    if n = 0 then acc else prepend (n - 1) (x :: acc) x
  in
  let rec aux acc = function [] -> acc | x :: t -> aux (prepend n acc x) t in
  aux [] (List.rev list)

(* Drop Every N'th Element From a List *)
let drop list n =
  let rec aux i = function
    | [] -> []
    | x :: t -> if i = n then aux 1 t else x :: aux (i + 1) t
  in
  aux 1 list

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split list n =
  let rec aux n acc = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if n = 0 then (List.rev acc, l) else aux (n - 1) (h :: acc) t
  in
  aux n [] list

(* Extract a Slice From a List *)
let slice list a b =
  let rec aux i = function
    | [] -> []
    | _ when i >= b -> []
    | x :: t -> if i >= a then x :: aux (i + 1) t else aux (i + 1) t
  in
  aux 0 list

(* Rotate a List N Places to the Left *)
let rotate list n =
  let len = List.length list in
  let n = if len = 0 then 0 else ((n mod len) + len) mod len in
  if n = 0 then list
  else
    let a, b = split list n in
    b @ a

(* Remove the K'th Element From a List *)
let remove_at n list =
  let rec aux i acc = function
    | [] -> acc
    | x :: xs ->
        if i + 1 = n then aux (i + 1) acc xs else aux (i + 1) (x :: acc) xs
  in
  aux 0 [] list

(* Insert an Element at a Given Position Into a List *)
let rec insert_at x n = function
  | [] -> [ x ]
  | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n - 1) t

(* Create a List Containing All Integers Within a Given Range *)
let range a b =
  let rec aux a b = if a > b then [] else a :: aux (a + 1) b in
  if a > b then List.rev (aux b a) else aux a b

(* Extract a Given Number of Randomly Selected Elements From a List *)
let rand_select list n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | x :: t -> if n = 0 then (x, acc @ t) else extract (x :: acc) (n - 1) t
  in
  let extract_rand list len = extract [] (Random.int len) list in
  let rec aux n acc list len =
    if n = 0 then acc
    else
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1)
  in
  let len = List.length list in
  aux (min n len) [] list len

(* Lotto: Draw N Different Random Numbers From the Set 1..M *)
let lotto_select n m = rand_select (range 1 m) n

let permutation list =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len = extract [] (Random.int len) list in
  let rec aux acc list len =
    if len = 0 then acc
    else
      let picked, rest = extract_rand list len in
      aux (picked :: acc) rest (len - 1)
  in
  aux [] list (List.length list)

(* Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List *)
let rec extract k list =
  if k <= 0 then [ [] ]
  else
    match list with
    | [] -> []
    | x :: tl ->
        let with_x = List.map (fun l -> x :: l) (extract (k - 1) tl) in
        let without_x = extract k tl in
        with_x @ without_x

(* Group the Elements of a Set Into Disjoint Subsets *)
let group list sizes =
  let initial = List.map (fun size -> (size, [])) sizes in

  let prepend p list =
    let emit l acc = l :: acc in
    let rec aux emit acc = function
      | [] -> emit [] acc
      | ((n, l) as h) :: t ->
          let acc = if n > 0 then emit ((n - 1, p :: l) :: t) acc else acc in
          aux (fun l acc -> emit (h :: l) acc) acc t
    in
    aux emit [] list
  in

  let rec aux = function
    | [] -> [ initial ]
    | h :: t -> List.concat_map (prepend h) (aux t)
  in

  let all = aux list in
  let complete = List.filter (List.for_all (fun (x, _) -> x = 0)) all in
  List.map (List.map snd) complete

(* Sorting a List of Lists According to Length of Sublists *)
let rec insert cmp e = function
  | [] -> [ e ]
  | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t

let rec sort cmp = function [] -> [] | h :: t -> insert cmp h (sort cmp t)

let length_sort lists =
  let lists = List.map (fun list -> (List.length list, list)) lists in
  let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
  List.map snd lists

let rle list =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((a, count + 1) :: acc) t
  in
  aux 0 [] list

let frequency_sort lists =
  let lengths = List.map List.length lists in
  let freq = rle (sort compare lengths) in
  let by_freq =
    List.map (fun list -> (List.assoc (List.length list) freq, list)) lists
  in
  let sorted = sort (fun a b -> compare (fst a) (fst b)) by_freq in
  List.map snd sorted
