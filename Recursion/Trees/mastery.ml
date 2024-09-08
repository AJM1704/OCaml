type 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree

type value = Bool of bool | Int of int

type expr =
  | Lit of value
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Lte of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr

type ty = BoolTy | IntTy

let tree1 = Branch (Leaf 'A', Branch (Branch (Branch (Leaf 'B', Leaf 'C'), Branch (Leaf 'D', Leaf 'E')), Leaf 'F'))

let tree2 = Branch (Branch (Branch (Leaf (Char.chr 48), Branch (Leaf (Char.chr 54), Leaf (Char.chr 56))), Branch (Leaf (Char.chr 51), Branch (Leaf (Char.chr 57), Leaf (Char.chr 55)))), Branch (Leaf (Char.chr 49), Branch (Branch (Leaf (Char.chr 52), Leaf (Char.chr 53)), Leaf (Char.chr 50))))

let tree3 = Branch (Branch (Branch (Leaf (Char.chr 101), Branch (Leaf (Char.chr 115), Leaf (Char.chr 104))), Branch (Branch (Leaf (Char.chr 110), Branch (Branch (Branch (Leaf (Char.chr 226), Branch (Branch (Branch (Branch (Branch (Branch (Branch (Branch (Branch (Branch (Leaf (Char.chr 91), Branch (Leaf (Char.chr 175), Branch (Leaf (Char.chr 160), Leaf (Char.chr 239)))), Branch (Branch (Leaf (Char.chr 36), Branch (Leaf (Char.chr 37), Leaf (Char.chr 187))), Branch (Branch (Leaf (Char.chr 161), Leaf (Char.chr 191)), Branch (Leaf (Char.chr 180), Leaf (Char.chr 35))))), Branch (Leaf (Char.chr 166), Leaf (Char.chr 182))), Leaf (Char.chr 56)), Branch (Leaf (Char.chr 57), Leaf (Char.chr 55))), Branch (Leaf (Char.chr 75), Branch (Leaf (Char.chr 195), Branch (Leaf (Char.chr 38), Branch (Leaf (Char.chr 168), Branch (Leaf (Char.chr 90), Leaf (Char.chr 47))))))), Branch (Leaf (Char.chr 85), Leaf (Char.chr 49))), Leaf (Char.chr 58)), Branch (Leaf (Char.chr 42), Branch (Leaf (Char.chr 89), Branch (Branch (Branch (Leaf (Char.chr 52), Leaf (Char.chr 53)), Leaf (Char.chr 152)), Branch (Leaf (Char.chr 81), Branch (Leaf (Char.chr 40), Leaf (Char.chr 41))))))), Branch (Leaf (Char.chr 157), Branch (Leaf (Char.chr 76), Leaf (Char.chr 33))))), Branch (Leaf (Char.chr 118), Leaf (Char.chr 73))), Branch (Leaf (Char.chr 44), Leaf (Char.chr 103)))), Branch (Branch (Branch (Branch (Leaf (Char.chr 107), Branch (Branch (Leaf (Char.chr 153), Leaf (Char.chr 156)), Branch (Branch (Branch (Leaf (Char.chr 82), Leaf (Char.chr 78)), Leaf (Char.chr 120)), Leaf (Char.chr 72)))), Leaf (Char.chr 121)), Leaf (Char.chr 108)), Leaf (Char.chr 111)))), Branch (Branch (Branch (Leaf (Char.chr 97), Branch (Branch (Leaf (Char.chr 99), Leaf (Char.chr 102)), Leaf (Char.chr 100))), Branch (Leaf (Char.chr 116), Branch (Branch (Branch (Branch (Branch (Leaf (Char.chr 59), Branch (Branch (Leaf (Char.chr 79), Leaf (Char.chr 86)), Branch (Leaf (Char.chr 74), Leaf (Char.chr 69)))), Branch (Leaf (Char.chr 84), Branch (Branch (Branch (Branch (Leaf (Char.chr 50), Branch (Branch (Branch (Branch (Branch (Leaf (Char.chr 194), Leaf (Char.chr 148)), Branch (Leaf (Char.chr 125), Leaf (Char.chr 169))), Branch (Branch (Leaf (Char.chr 171), Leaf (Char.chr 93)), Branch (Leaf (Char.chr 123), Leaf (Char.chr 163)))), Leaf (Char.chr 54)), Leaf (Char.chr 132))), Branch (Branch (Leaf (Char.chr 88), Leaf (Char.chr 48)), Branch (Leaf (Char.chr 162), Leaf (Char.chr 51)))), Leaf (Char.chr 66)), Leaf (Char.chr 83)))), Branch (Leaf (Char.chr 45), Branch (Branch (Branch (Leaf (Char.chr 63), Leaf (Char.chr 106)), Leaf (Char.chr 95)), Branch (Leaf (Char.chr 77), Leaf (Char.chr 87))))), Leaf (Char.chr 10)), Branch (Leaf (Char.chr 13), Leaf (Char.chr 109))))), Branch (Branch (Branch (Branch (Branch (Leaf (Char.chr 46), Leaf (Char.chr 98)), Leaf (Char.chr 119)), Leaf (Char.chr 114)), Branch (Branch (Branch (Leaf (Char.chr 112), Branch (Branch (Branch (Branch (Branch (Leaf (Char.chr 70), Leaf (Char.chr 122)), Leaf (Char.chr 71)), Branch (Leaf (Char.chr 80), Leaf (Char.chr 113))), Branch (Branch (Leaf (Char.chr 68), Leaf (Char.chr 67)), Leaf (Char.chr 65))), Leaf (Char.chr 128))), Leaf (Char.chr 117)), Leaf (Char.chr 105))), Leaf (Char.chr 32))))

let string_of_chars (chars : char list) : string =
  String.of_seq (List.to_seq chars)



let rec huffman_decode_one (bits: bool list) (tree: 'a tree) : (bool list * 'a) option =
  match (bits, tree) with
  | [], Leaf x -> Some ([], x) 
  | [], _ -> None 
  | _, Leaf x -> Some (bits, x) 
  | false :: tl, Branch (left, _) -> huffman_decode_one tl left 
  | true :: tl, Branch (_, right) -> huffman_decode_one tl right 
  | _ :: _, _ -> None

let huffman_decode (bits: bool list) (tree: 'a tree) : 'a list option =
  let rec decode acc remaining_bits =
    match huffman_decode_one remaining_bits tree with
    | Some (bits_left, x) -> decode (x :: acc) bits_left
    | None -> if List.length remaining_bits = 0 then Some (List.rev acc) else None
  in
  decode [] bits

let huffman_decode_string (bits: bool list) (tree: char tree) : string option =
  match huffman_decode bits tree with
  | Some chars -> Some (string_of_chars chars)
  | None -> None

let rec eval : expr -> value = fun expr ->
  match expr with
  | Lit v -> v
  | Add (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match (v1, v2) with
       | (Int i1, Int i2) -> Int (i1 + i2)
       | _ -> raise (Failure "type error"))
  | Sub (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match (v1, v2) with
       | (Int i1, Int i2) -> Int (i1 - i2)
       | _ -> raise (Failure "type error"))
  | Mul (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match (v1, v2) with
       | (Int i1, Int i2) -> Int (i1 * i2)
       | _ -> raise (Failure "type error"))
  | Lte (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match (v1, v2) with
       | (Int i1, Int i2) -> Bool (i1 <= i2)
       | _ -> raise (Failure "type error"))
  | Eq (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match (v1, v2) with
       | (Int i1, Int i2) -> Bool (i1 = i2)
       | _ -> raise (Failure "type error"))
  | If (cond, e1, e2) ->
      let cond_val = eval cond in
      match cond_val with
      | Bool b -> if b then eval e1 else eval e2
      | _ -> raise (Failure "type error")

let rec expr_has_type : expr -> ty -> bool = fun expr ty ->
  match (expr, ty) with
  | (Lit (Bool _), BoolTy)
  | (Lit (Int _), IntTy) -> true  (* Literal values match the specified type *)
  | (Add (e1, e2), IntTy)
  | (Sub (e1, e2), IntTy)
  | (Mul (e1, e2), IntTy)
  | (Lte (e1, e2), BoolTy)
  | (Eq (e1, e2), BoolTy) -> expr_has_type e1 IntTy && expr_has_type e2 IntTy  
  | (If (cond, e1, e2), ty) -> expr_has_type cond BoolTy && expr_has_type e1 ty && expr_has_type e2 ty  
  | _ -> false  
