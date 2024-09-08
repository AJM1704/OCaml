let dec (n: int) : int = 
    n - 1

let cube (n: int) : int = 
    n * n * n

let last_digit (n: int) : int =
  let n = abs n in
    n mod 10

let rec sum (lst: int list) : int =
    match lst with
    | [] -> 0
    | h::t -> h + sum t

let rec product (lst: int list) : int =
    match lst with
    | [] -> 1
    | h::t -> h * product t

let rec all_odds (lst: int list) : int list =
    match lst with
    | [] -> []
    | h::t ->
        if h mod 2 = 1 then h :: all_odds t
        else all_odds t

let rec square_all (n: int list) : int list = 
    match n with 
    | [] -> []
    | h::t -> (h * h) :: square_all t