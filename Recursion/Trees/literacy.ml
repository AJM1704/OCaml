type 'a tree = Leaf of 'a 
  | Branch of 'a tree * 'a tree

type 'a nonempty_list
  = One of 'a
  | Cons of 'a * 'a nonempty_list

type ('key, 'value) maptree =
  | Node of ('key, 'value) maptree * 'key * 'value * ('key, 'value) maptree
  | End

let chars_of_string (str : string) : char list =
  List.of_seq (String.to_seq str)



let rec append_nonempty : 'a nonempty_list -> 'a nonempty_list -> 'a nonempty_list = fun lst1 lst2 ->
  match lst1 with
  | One x -> Cons (x, lst2)
  | Cons (x, rest) -> Cons (x, append_nonempty rest lst2)

let rec eq_nonempty : 'a nonempty_list -> 'a nonempty_list -> bool = fun lst1 lst2 ->
  match (lst1, lst2) with
  | (One x, One y) -> x = y
  | (Cons (x, rest1), Cons (y, rest2)) -> x = y && eq_nonempty rest1 rest2
  | (_, _) -> false

let rec normalize_shape : 'a tree -> 'a nonempty_list = fun tree ->
  match tree with
  | Leaf x -> One x
  | Branch (left, right) -> append_nonempty (normalize_shape left) (normalize_shape right)

let same_fringe : 'a tree -> 'a tree -> bool = fun t1 t2 ->
  eq_nonempty (normalize_shape t1) (normalize_shape t2)




let rec map_lookup : ('key, 'value) maptree -> 'key -> 'value option = fun tree key ->
  match tree with
  | End -> None  
  | Node (left, k, v, right) ->
      if key = k then Some v  
      else if key < k then map_lookup left key  
      else map_lookup right key  

let rec map_insert : 'key -> 'value -> ('key, 'value) maptree -> ('key, 'value) maptree = fun key value tree ->
  match tree with
  | End -> Node (End, key, value, End) 
  | Node (left, k, v, right) ->
      if key = k then Node (left, key, value, right)  
      else if key < k then Node (map_insert key value left, k, v, right)  
      else Node (left, k, v, map_insert key value right)  
