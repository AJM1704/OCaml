let rec first_digit (n: int) : int =
    if n = 0 then 0  
    else if abs n < 10 then abs n  
    else first_digit (abs (n / 10))  

let rec product_to (n: int) : int =
    if n < 0 then raise (Failure "invalid input to product_to")
    else if n = 0 then 0  
    else if n = 1 then 1  
    else n * product_to (n - 1)

let rec is_prime (n: int) : bool =
    if n < 0 then raise (Failure "invalid input to is_prime")
    else if n = 0 then false
    else if n = 1 then false
    else let rec is_divisible k =
      if k * k > n then true
      else if n mod k = 0 then false
      else is_divisible (k + 1)
    in is_divisible 2 = true

(* Using last_digit from warmup.ml to help with following function *)
let last_digit (n: int) : int =
  let n = abs n in
    n mod 10
(*  *)

let rec last_digits (lst: int list) : int list =
    match lst with
    | [] -> []
    | h::t -> last_digit h :: last_digits t


let rec all_primes (lst: int list) : int list =
    match lst with
    | [] -> []
    | h::t -> 
        if is_prime h = true then h :: all_primes t
        else all_primes t

let rec sum_pairs (pairs: (int * int) list) : int =
    match pairs with
    | [] -> 0
    | (x, y) :: t -> x + y + sum_pairs t

let rec max_pairs (pairs: (int * int) list) : int =
    match pairs with
    | [] -> raise (Failure "invalid input to max_pairs")
    | [(x, y)] -> max x y
    | (x, y) :: t -> max (max x y) (max_pairs t)
