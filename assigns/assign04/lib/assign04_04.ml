type ident = string

type expr' = 
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' = 
  | Int
  | Bool

type context = (ident * ty') list

let rec type_of' (lst : context) (e : expr') : ty' option =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Var x -> List.assoc_opt x lst
  | Let (x, e1, e2) -> 
      (match type_of' lst e1 with 
      | Some t1 -> type_of' ((x, t1) :: lst) e2
      | None -> None)
  | Add (e1, e2) ->
      (match (type_of' lst e1, type_of' lst e2) with
       | (Some Int, Some Int) -> Some Int
       | _ -> None)
  | Or (e1, e2) -> 
      (match (type_of' lst e1, type_of' lst e2) with
       | (Some Bool, Some Bool) -> Some Bool
       | _ -> None)
  | IfThenElse (e1, e2, e3) ->
      (match type_of' lst e1 with
      | Some Bool ->
          (match (type_of' lst e2, type_of' lst e3) with
              | (Some Int, Some Int) -> Some Int
              | (Some Bool, Some Bool) -> Some Bool
              | _ -> None)
      | _ -> None)
