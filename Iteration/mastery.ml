
type 'a tree = Empty | Fork of 'a tree * 'a * 'a tree
type 'a ltree = Leaf of 'a | Branch of 'a ltree * 'a ltree

let rec map_ltree (f : 'a -> 'b) (t : 'a ltree) : 'b ltree =
  match t with
  | Leaf x -> Leaf (f x)
  | Branch (left, right) -> Branch (map_ltree f left, map_ltree f right)

let rec reduce_ltree (e : 'a -> 'b) (f : ('b -> 'b -> 'b)) (t : 'a ltree) : 'b =
  match t with
  | Leaf x -> e x
  | Branch (left, right) ->
      let left_result = reduce_ltree e f left in
      let right_result = reduce_ltree e f right in
      f left_result right_result

let max_int_string_pairs (pairs : (string * int) list) : (string * int) list =
  match pairs with
  | [] -> []
  | (name, age) :: t ->
      let max_age = List.fold_left (fun acc (_, a) -> max acc a) age pairs in
      List.filter (fun (_, a) -> a = max_age) pairs

let min_ltree (t : int ltree) : int =
  let rec collect_leaves = function
    | Leaf x -> [x]
    | Branch (left, right) -> (collect_leaves left) @ (collect_leaves right)
  in
  match collect_leaves t with
  | [] -> failwith "Empty tree"
  | hd :: tl -> List.fold_left min hd tl

let max_ltree (t : int ltree) : int =
  let rec collect_leaves = function
    | Leaf x -> [x]
    | Branch (left, right) -> (collect_leaves left) @ (collect_leaves right)
  in
  match collect_leaves t with
  | [] -> failwith "Empty tree"
  | hd :: tl -> List.fold_left max hd tl

let range_ltree (t : int ltree) : (int * int) =
  let rec collect_leaves = function
    | Leaf x -> [x]
    | Branch (left, right) -> (collect_leaves left) @ (collect_leaves right)
  in
  match collect_leaves t with
  | [] -> failwith "Empty tree"
  | hd :: tl -> (List.fold_left min hd tl, List.fold_left max hd tl)

let range (n : int) : int list = List.init (n - 2) (fun i -> i + 2)

let rev (lst : 'a list) : 'a list =
  List.fold_left (fun acc x -> x :: acc) [] lst

let elem (x : 'a) (lst : 'a list) : bool =
  List.fold_left (fun acc y -> if y = x then true else acc) false lst

let sieve (n : int) (lst : int list) : int list =
  List.filter (fun x -> x mod n <> 0) lst

let primes (lst : int list) : int list =
  let rec sieve primes_lst = function
    | [] -> primes_lst
    | hd :: tl ->
        let filtered_list = List.filter (fun x -> x mod hd <> 0) tl in
        sieve (hd :: primes_lst) filtered_list
  in
  List.rev (sieve [] lst)
