type ident = string

type ty =
  | Unit
  | Arr of ty * ty

type expr =
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

let rec helper x gamma  =
  match gamma with
  | [] -> None
  | (i, t) :: rest -> if x = i then Some t else helper x rest

let rec type_of (gamma:ctxt) e =
  match e with
  | Var x ->
      helper x gamma

  | Fun (x, t, e) ->
      (match type_of ((x, t) :: gamma) e with
       | Some t2 -> Some (Arr (t, t2))
       | None -> None)

  | App (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some (Arr (t1, t)), Some t2 when t1 = t2 -> Some t
       | _ -> None)


