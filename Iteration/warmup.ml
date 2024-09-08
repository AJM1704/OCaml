let sum (lst : int list) : int =
  List.fold_left (+) 0 lst

let product (lst : int list) : int =
  List.fold_left ( * ) 1 lst

let all_odds (lst : int list) : int list =
  List.filter (fun x -> x mod 2 <> 0) lst

let square_all (lst : int list) : int list =
  List.map (fun x -> x * x) lst

let negate_odd (lst : int list) : int list =
  List.map (fun x -> if x mod 2 <> 0 then 
  -x else 
  x) lst

let char_cons (x : char) (str : string) : string = (String.make 1 x) ^ str

let string_of_chars lst = List.fold_right (fun x str -> char_cons x str) lst "";;

let sum_pairs (pairs : (int * int) list) : int =
  List.fold_left (fun acc (x, y) -> acc + x + y) 0 pairs

let max_pairs (pairs : (int * int) list) : int =
  match pairs with
  | [] -> raise (Failure "invalid input to max_pairs")
  | _ -> List.fold_left (fun acc (x, y) -> max acc (max x y)) min_int pairs

let max_opt (lst : int list) : int option =
  match lst with
  | [] -> None
  | _ -> Some (List.fold_left max min_int lst)

let max_pairs_opt (pairs : (int * int) list) : int option =
  match pairs with
  | [] -> None
  | _ -> Some (List.fold_left (fun acc (x, y) -> max acc (max x y)) min_int pairs)