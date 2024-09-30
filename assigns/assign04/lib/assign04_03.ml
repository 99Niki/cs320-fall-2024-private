open Assign04_02


type value = 
| VNum of int
| VBool of bool

let rec eval e =
  match e with
  | True -> VBool true
  | False -> VBool false
  | Num n -> VNum n
  | Or (e1, e2) ->
      (match (eval e1, eval e2) with
       | (VBool t1,VBool t2) when t1 = t2-> VBool t1
       | (VBool t1,VBool t2) when t1 <> t2-> VBool true
       | _ -> failwith "Type error")
  | Add (e1, e2) ->
      (match (eval e1, eval e2) with
       | (VNum a, VNum b) -> VNum (a+b)
       | _ -> failwith "Type error")
  | IfThenElse (e1, e2, e3) ->
      (match eval e1 with
       | VBool true -> eval e2
       | VBool false -> eval e3
       | _ -> failwith "Type error")