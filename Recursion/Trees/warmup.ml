type 'a tree = Leaf of 'a 
    | Branch of 'a tree * 'a tree


let rec tree_product (t: int tree) : int = 
  match t with
  | Leaf x -> x
  | Branch (lb, rb) -> (tree_product lb) * (tree_product rb)

let rec tree_min (t: int tree) : int =
    match t with
    | Leaf x -> x
    | Branch (lb, rb) -> min (tree_min lb) (tree_min rb)

let rec tree_max (t: int tree) : int =
    match t with
    | Leaf x -> x
    | Branch (lb, rb) -> max (tree_max lb) (tree_max rb)

let rec tree_range (t: int tree) : int * int =
    match t with
    | Leaf x -> (x, x)   
    | Branch (lb, rb) ->
        let (min_left, max_left) = tree_range lb in  
        let (min_right, max_right) = tree_range rb in  
            (min min_left min_right, max max_left max_right)  

let rec tree_depth_range (t: 'a tree) : int * int =
    match t with
    | Leaf _ -> (1, 1)  
    | Branch (lb, rb) ->
        let (min_left, max_left) = tree_depth_range lb in  
        let (min_right, max_right) = tree_depth_range rb in  
            (1 + min min_left min_right, 1 + max max_left max_right) 

let rec same_shape (l: 'a tree) (r: 'a tree) : bool =
    match (l, r) with
    | (Leaf _, Leaf _) -> true
    | (Leaf _, Branch _) | (Branch _, Leaf _) -> false
    | (Branch (l1, r1), Branch (l2, r2)) -> 
        (same_shape l1 l2) && (same_shape r1 r2)
    