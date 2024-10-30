type expr = 
| True
| False
| Num of int
| Or of expr * expr
| Add of expr * expr
| IfThenElse of expr * expr * expr

type ty =
  | Int
  | Bool

let rec type_of e =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Or (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Some Bool, Some Bool) -> Some Bool
       | _ -> None)
  | Add (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Some Int, Some Int) -> Some Int
       | _ -> None)
  | IfThenElse (e1, e2, e3) ->
      (match type_of e1 with
       | Some Bool ->
           (match (type_of e2, type_of e3) with
            | (Some Int , Some Int) -> Some Int
            | (Some Bool , Some Bool) -> Some Bool
            | _ -> None)
       | _ -> None)