let is_prime_faster (n: int) : bool =
  if n < 0 then raise (Failure "invalid input to is_prime_faster")
  else if n <= 1 then false
  else let rec check_divisor i =
    if i * i > n then true
    else if n mod i = 0 then false
    else check_divisor (i + 1) in check_divisor 2

let rec big_squares (lst: int list) : int list = 
    match lst with
    | [] -> []
    | h::t ->
        let square = h * h in
        if square >= 50 then square :: big_squares t
        else big_squares t

let rec sum_of_squares (lst: int list) : int = 
    match lst with
    | [] -> 0
    | h::t -> (h * h) + sum_of_squares t

let rec product_of_primes (lst: int list) : int = 
    match lst with
    | [] -> 1
    | h::t ->
        if is_prime_faster h = true then
            h * product_of_primes t
        else product_of_primes t


let rec max_int_string_pairs (pair: (string * int) list) : (string * int) list =
  let rec find_max pair max_age max_name =
    match pair with
    | [] -> [(max_name, max_age)]
    | (name, age) :: t ->
        if age > max_age then
          find_max t age name
        else if age = max_age then
          (name, age) :: find_max t max_age max_name
        else
          find_max t max_age max_name
  in
  match pair with
  | [] -> raise (Failure "invalid input to max_int_string_pairs")
  | (name, age) :: t -> find_max t age name