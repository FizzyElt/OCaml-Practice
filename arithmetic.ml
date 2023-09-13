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

(* Calculate Euler's Totient Function Φ(m) (Improved) *)
let rec pow n p = if p < 1 then 1 else n * pow n (p - 1)

(* Calculate Euler's Totient Function Φ(m) (Improved)  *)
let phi_improved n =
  let rec aux acc = function
    | [] -> acc
    | (p, m) :: t -> aux ((p - 1) * pow p (m - 1) * acc) t
  in
  aux 1 (factors2 n)

(* A List of Prime Numbers *)
let rec all_primes a b =
  if a > b then []
  else
    let rest = all_primes (a + 1) b in
    if is_prime a then a :: rest else rest

(* Goldbach's Conjecture  *)
let goldbach n =
  let rec aux d =
    if is_prime d && is_prime (n - d) then (d, n - d) else aux (d + 1)
  in
  aux 2

(* A List of Goldbach Compositions  *)
let rec goldbach_list a b =
  if a > b then []
  else if a mod 2 = 1 then goldbach_list (a + 1) b
  else (a, goldbach a) :: goldbach_list (a + 2) b

let goldbach_limit a b lim =
  List.filter (fun (_, (a, b)) -> a > lim && b > lim) (goldbach_list a b)
