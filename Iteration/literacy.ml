type 'a ltree = Leaf of 'a 
              | Branch of 'a ltree * 'a ltree

let rec map_ltree (f : 'a -> 'b) (t : 'a ltree) : 'b ltree =
  match t with
  | Leaf x -> Leaf (f x)
  | Branch (left, right) -> Branch (map_ltree f left, map_ltree f right)

let rec reduce_ltree (f : 'a -> 'b) (g : 'b -> 'b -> 'b) (t : 'a ltree) : 'b =
  match t with
  | Leaf x -> f x
  | Branch (left, right) -> g (reduce_ltree f g left) (reduce_ltree f g right)

let square_all_ltree (t : int ltree) : int ltree =
  map_ltree (fun x -> x * x) t

let lengths_ltree (t : string ltree) : int ltree =
  map_ltree String.length t

let rec sum_ltree (t : int ltree) : int =
  reduce_ltree (fun x -> x) (+) t

let rec product_ltree (t : int ltree) : int =
  reduce_ltree (fun x -> x) ( * ) t

type 'a tree = Empty
             | Fork of 'a tree * 'a * 'a tree

let rec map_tree (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Empty -> Empty
  | Fork (left, x, right) -> Fork (map_tree f left, f x, map_tree f right)

let rec reduce_tree (e : 'b) (f : 'b -> 'a -> 'b -> 'b) (t : 'a tree) : 'b =
  match t with
  | Empty -> e
  | Fork (left, x, right) -> f (reduce_tree e f left) x (reduce_tree e f right)

let square_all_tree (t : int tree) : int tree =
  map_tree (fun x -> x * x) t

let sum_tree (t : int tree) : int =
  reduce_tree 0 (fun l x r -> l + x + r) t

let product_tree (t : int tree) : int =
  reduce_tree 1 (fun l x r -> l * x * r) t

let lengths_tree (t : string tree) : int tree =
  map_tree String.length t


let tree1 = Fork(
  Fork (Empty, Fork (Empty, 7, Empty), Empty),
  Fork (Empty, 1, Empty),
  Fork (Empty, Fork (Empty, 2, Empty), Empty))

let tree2 = Fork(
  Fork (Empty, Fork (Empty, 7, Empty), Empty),
  Fork (Empty, 1, Empty),
  Fork (Empty, Fork (Fork (Empty, 4, Empty), 4, Empty), Empty))