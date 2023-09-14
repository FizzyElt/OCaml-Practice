type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

(* Truth Tables for Logical Expressions (2 Variables)  *)
let rec eval2 a val_a b val_b = function
  | Var x ->
      if x = a then val_a
      else if x = b then val_b
      else failwith "The expression contains an invalid variable"
  | Not e -> not (eval2 a val_a b val_b e)
  | And (e1, e2) -> eval2 a val_a b val_b e1 && eval2 a val_a b val_b e2
  | Or (e1, e2) -> eval2 a val_a b val_b e1 || eval2 a val_a b val_b e2

let table2 a b expr =
  [
    (true, true, eval2 a true b true expr);
    (true, false, eval2 a true b false expr);
    (false, true, eval2 a false b true expr);
    (false, false, eval2 a false b false expr);
  ]

(* Truth Tables for Logical Expressions *)
let rec eval val_vars = function
  | Var x -> List.assoc x val_vars
  | Not e -> not (eval val_vars e)
  | And (e1, e2) -> eval val_vars e1 && eval val_vars e2
  | Or (e1, e2) -> eval val_vars e1 || eval val_vars e2

let rec table_make val_vars vars expr =
  match vars with
  | [] -> [ (List.rev val_vars, eval val_vars expr) ]
  | v :: tl ->
      table_make ((v, true) :: val_vars) tl expr
      @ table_make ((v, false) :: val_vars) tl expr

let table vars expr = table_make [] vars expr

(* Gray Code *)
let gray n =
  let rec gray_next_level k l =
    if k < n then
      let first_half, second_half =
        List.fold_left
          (fun (acc1, acc2) x -> (("0" ^ x) :: acc1, ("1" ^ x) :: acc2))
          ([], []) l
      in
      gray_next_level (k + 1) (List.rev_append first_half second_half)
    else l
  in
  gray_next_level 1 [ "0"; "1" ]

(* Huffman Code *)
module Pq = struct
  type 'a t = { data : 'a list array; mutable first : int }

  let make () = { data = Array.make 101 []; first = 101 }

  let add q p x =
    q.data.(p) <- x :: q.data.(p);
    q.first <- min p q.first

  let get_min q =
    if q.first = 101 then None
    else
      match q.data.(q.first) with
      | [] -> assert false
      | x :: tl ->
          let p = q.first in
          q.data.(q.first) <- tl;
          while q.first < 101 && q.data.(q.first) = [] do
            q.first <- q.first + 1
          done;
          Some (p, x)
end

type tree = Leaf of string | Node of tree * tree

let rec huffman_tree q =
  match (Pq.get_min q, Pq.get_min q) with
  | Some (p1, t1), Some (p2, t2) ->
      Pq.add q (p1 + p2) (Node (t1, t2));
      huffman_tree q
  | Some (_, t), None | None, Some (_, t) -> t
  | None, None -> assert false

let rec prefixes_of_tree prefix = function
  | Leaf s -> [ (s, prefix) ]
  | Node (t0, t1) ->
      prefixes_of_tree (prefix ^ "0") t0 @ prefixes_of_tree (prefix ^ "1") t1

let rec huffman fs =
  if List.fold_left (fun s (_, p) -> s + p) 0 fs <> 100 then
    failwith "huffman: sum of weights must be 100";
  let q = Pq.make () in
  List.iter (fun (s, f) -> Pq.add q f (Leaf s)) fs;
  prefixes_of_tree "" (huffman_tree q)
