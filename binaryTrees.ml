type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

(* Construct Completely Balanced Binary Trees *)
let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right
  in
  List.fold_left add_right_tree all left

let rec cbal_tree n =
  if n = 0 then [ Empty ]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    add_trees_with t t []
  else
    let t1 = cbal_tree ((n / 2) - 1) in
    let t2 = cbal_tree (n / 2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 [])

(* Symmetric Binary Trees *)
let rec is_mirror t1 t2 =
  match (t1, t2) with
  | Empty, Empty -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror r1 l2
  | _ -> false

let is_symmetric = function Empty -> true | Node (_, l, r) -> is_mirror l r

(* Binary Search Trees (Dictionaries) *)
let rec insert tree x =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (y, l, r) ->
      if x = y then tree
      else if x < y then Node (y, insert l x, r)
      else Node (y, l, insert r x)

let construct l = List.fold_left insert Empty l

(* Generate-and-Test Paradigm *)
let sym_cbal_trees n = List.filter is_symmetric (cbal_tree n)

(* Construct Height-Balanced Binary Trees *)
let rec hbal_tree n =
  if n = 0 then [ Empty ]
  else if n = 1 then [ Node ('x', Empty, Empty) ]
  else
    let t1 = hbal_tree (n - 1) and t2 = hbal_tree (n - 2) in
    add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []))

(* Construct Height-Balanced Binary Trees With a Given Number of Nodes *)
let rec min_nodes_loop m0 m1 h =
  if h <= 1 then m1 else min_nodes_loop m1 (m1 + m0 + 1) (h - 1)

let rec min_nodes h = if h <= 0 then 0 else min_nodes_loop 0 1 h
let max_nodes h = min_nodes (h + 1)
let min_height n = int_of_float (ceil (log (float (n + 1)) /. log 2.))

let rec max_height_search h m_h m_h1 n =
  if m_h <= n then max_height_search (h + 1) m_h1 (m_h1 + m_h + 1) n else h - 1

let max_height n = max_height_search 0 0 1 n

let rec fold_range ~f ~init n0 n1 =
  if n0 > n1 then init else fold_range ~f ~init:(f init n0) (n0 + 1) n1

let rec add_swap_left_right trees =
  List.fold_left
    (fun a n ->
      match n with Node (v, t1, t2) -> Node (v, t2, t1) :: a | Empty -> a)
    trees trees

let rec hbal_tree_nodes_height h n =
  assert (min_nodes h <= n && n <= max_nodes h);
  if h = 0 then [ Empty ]
  else
    let acc = add_hbal_tree_node [] (h - 1) (h - 2) n in
    let acc = add_swap_left_right acc in
    add_hbal_tree_node acc (h - 1) (h - 1) n

and add_hbal_tree_node l h1 h2 n =
  let min_n1 = max (min_nodes h1) (n - 1 - max_nodes h2) in
  let max_n1 = min (max_nodes h1) (n - 1 - min_nodes h2) in
  fold_range min_n1 max_n1 ~init:l ~f:(fun l n1 ->
      let t1 = hbal_tree_nodes_height h1 n1 in
      let t2 = hbal_tree_nodes_height h2 (n - 1 - n1) in
      List.fold_left
        (fun l t1 -> List.fold_left (fun l t2 -> Node ('x', t1, t2) :: l) l t2)
        l t1)

let hbal_tree_nodes n =
  fold_range (min_height n) (max_height n) ~init:[] ~f:(fun l h ->
      List.rev_append (hbal_tree_nodes_height h n) l)

(* Count the Leaves of a Binary Tree *)
let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r

(* Collect the Leaves of a Binary Tree in a List *)
let leaves tree =
  let rec leaves_aux t acc =
    match t with
    | Empty -> acc
    | Node (x, Empty, Empty) -> x :: acc
    | Node (_, l, r) -> leaves_aux l (leaves_aux r acc)
  in
  leaves_aux tree []

(* Collect the Internal Nodes of a Binary Tree in a List *)
let internals t =
  let rec internals_aux t acc =
    match t with
    | Empty -> acc
    | Node (x, Empty, Empty) -> acc
    | Node (x, l, r) -> internals_aux l (x :: internals_aux r acc)
  in
  internals_aux t []

(* Collect the Nodes at a Given Level in a List *)
let at_level tree h =
  let rec at_level_aux t acc count =
    match t with
    | Empty -> acc
    | Node (x, l, r) ->
        if count = h then x :: acc
        else at_level_aux l (at_level_aux r acc (count + 1)) (count + 1)
  in
  at_level_aux tree [] 1

(* Construct a Complete Binary Tree *)
let rec split_n lst acc n =
  match (n, lst) with
  | 0, _ -> (List.rev acc, lst)
  | _, [] -> (List.rev acc, [])
  | _, h :: t -> split_n t (h :: acc) (n - 1)

let rec myflatten p c =
  match (p, c) with
  | p, [] -> List.map (fun x -> Node (x, Empty, Empty)) p
  | x :: t, [ y ] -> Node (x, y, Empty) :: myflatten t []
  | ph :: pt, x :: y :: t -> Node (ph, x, y) :: myflatten pt t
  | _ -> invalid_arg "myflatten"

let complete_binary_tree = function
  | [] -> Empty
  | lst ->
      let rec aux l = function
        | [] -> []
        | lst ->
            let p, c = split_n lst [] (1 lsl l) in
            myflatten p (aux (l + 1) c)
      in
      List.hd (aux 0 lst)

(* Layout a Binary Tree (1) *)
let layout_binary_tree_1 tree =
  let rec aux tree x y =
    match tree with
    | Empty -> (Empty, x)
    | Node (v, l, r) ->
        let l', l_x_max = aux l x (y + 1) in
        let r', r_x_max = aux r (l_x_max + 1) (y + 1) in
        (Node ((v, l_x_max, y), l', r'), r_x_max)
  in
  fst (aux tree 1 1)

(* Layout a Binary Tree (2) *)
let layout_binary_tree_2 t =
  let rec height = function
    | Empty -> 0
    | Node (_, l, r) -> 1 + max (height l) (height r)
  in
  (* tree height *)
  let tree_height = height t in
  let rec find_missing_left depth = function
    | Empty -> tree_height - depth
    | Node (_, l, _) -> find_missing_left (depth + 1) l
  in
  let translate_dst = (1 lsl find_missing_left 0 t) - 1 in
  let rec layout depth x_root = function
    | Empty -> Empty
    | Node (x, l, r) ->
        let spacing = 1 lsl (tree_height - depth - 1) in
        let l' = layout (depth + 1) (x_root - spacing) l in
        let r' = layout (depth + 1) (x_root + spacing) r in
        Node ((x, x_root, depth), l', r')
  in
  layout 1 ((1 lsl (tree_height - 1)) - translate_dst) t

(* Layout a Binary Tree (3) *)
let layout_binary_tree_3 =
  let rec translate_x d = function
    | Empty -> Empty
    | Node ((v, x, y), l, r) ->
        Node ((v, x + d, y), translate_x d l, translate_x d r)
  in
  let rec dist lr rl =
    match (lr, rl) with
    | lrx :: ltl, rlx :: rtl -> max (lrx - rlx) (dist ltl rtl)
    | [], _ | _, [] -> 0
  in
  let rec merge_profiles p1 p2 =
    match (p1, p2) with
    | x1 :: tl1, _ :: tl2 -> x1 :: merge_profiles tl1 tl2
    | [], _ -> p2
    | _, [] -> p1
  in
  let rec layout depth = function
    | Empty -> ([], Empty, [])
    | Node (v, l, r) ->
        let ll, l', lr = layout (depth + 1) l in
        let rl, r', rr = layout (depth + 1) r in
        let d = 1 + (dist lr rl / 2) in
        let ll = List.map (fun x -> x - d) ll
        and lr = List.map (fun x -> x - d) lr
        and rl = List.map (( + ) d) rl
        and rr = List.map (( + ) d) rr in
        ( 0 :: merge_profiles ll rl,
          Node ((v, 0, depth), translate_x (-d) l', translate_x d r'),
          0 :: merge_profiles rr lr )
  in
  fun t ->
    let l, t', _ = layout 1 t in
    let x_min = List.fold_left min 0 l in
    translate_x (1 - x_min) t'

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node
    ( 'a',
      Node ('b', leaf 'd', leaf 'e'),
      Node ('c', Empty, Node ('f', leaf 'g', Empty)) )

(* A String Representation of Binary Trees *)
let rec buffer_of_tree buf = function
  | Empty -> ()
  | Node (x, l, r) -> (
      Buffer.add_char buf x;
      match (l, r) with
      | Empty, Empty -> ()
      | _, _ ->
          Buffer.add_char buf '(';
          buffer_of_tree buf l;
          Buffer.add_char buf ',';
          buffer_of_tree buf r;
          Buffer.add_char buf ')')

let string_of_tree t =
  let buf = Buffer.create 128 in
  buffer_of_tree buf t;
  Buffer.contents buf

let tree_of_string =
  let rec make ofs s =
    if ofs >= String.length s || s.[ofs] = ',' || s.[ofs] = ')' then (Empty, ofs)
    else
      let v = s.[ofs] in
      if ofs + 1 < String.length s && s.[ofs + 1] = '(' then
        let l, ofs = make (ofs + 2) s in
        let r, ofs = make (ofs + 1) s in
        (Node (v, l, r), ofs + 1)
      else (Node (v, Empty, Empty), ofs + 1)
  in
  fun s -> fst (make 0 s)

let rec preorder = function
  | Empty -> []
  | Node (v, l, r) -> (v :: preorder l) @ preorder r

let rec inorder = function
  | Empty -> []
  | Node (v, l, r) -> inorder l @ (v :: inorder r)
