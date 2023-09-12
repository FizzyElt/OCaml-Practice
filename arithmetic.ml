(* Determine Whether a Given Integer Number Is Prime *)
let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n <> 1 && is_not_divisor 2

(* Determine the Greatest Common Divisor of Two Positive Integer Numbers *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(* Determine Whether Two Positive Integer Numbers Are Coprime *)
let coprime a b = gcd a b = 1

(* Calculate Euler's Totient Function Φ(m) *)
let phi n =
  let rec count_coprime acc d =
    if d < n then count_coprime (if coprime n d then acc + 1 else acc) (d + 1)
    else acc
  in
  if n = 1 then 1 else count_coprime 0 1

(* Determine the Prime Factors of a Given Positive Integer *)
let factors n =
  let rec aux d n =
    if n = 1 then []
    else if n mod d = 0 then d :: aux d (n / d)
    else aux (d + 1) n
  in
  aux 2 n

(* Determine the Prime Factors of a Given Positive Integer (2) *)
let factors2 n =
  let rec aux d n =
    if n = 1 then []
    else if n mod d = 0 then
      match aux d (n / d) with
      | (h, n) :: t when h = d -> (h, n + 1) :: t
      | l -> (d, 1) :: l
    else aux (d + 1) n
  in
  aux 2 n
