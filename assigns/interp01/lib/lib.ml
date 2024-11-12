open Utils

let parse (s: string) : expr option = My_parser.parse (s)

let replace_var x y =
  let rec go = function
    | Var z -> if z = y then Var x else Var z
    | App (e1, e2) -> App (go e1, go e2)
    | Fun (z, e) -> Fun (z, go e)
    | Let (z, e1, e2) -> Let(z, go e1, go e2)
    | Unit -> Unit
    | True -> True
    | False -> False
    | Num m -> Num m
    | Bop (op,e1,e2)-> Bop(op, go e1,go e2)
    | If (e1,e2,e3)-> If (go e1, go e2,go e3)
  in 
  go
  
let rec subst (v:value) (x:string) (e:expr):expr =
  let ve = 
    match v with
  | VNum n -> (Num n)
  | VBool b -> (if b then True else False)
  | VUnit -> Unit
  | VFun (p,b) -> Fun (p,b)
  in
  match e with
  | Var y -> if y=x then ve else e
  | App(e1,e2) -> App(subst v x e1, subst v x e2)
  | Bop(op,e1,e2)-> Bop (op, subst v x e1, subst v x e2)
  | If (c, e1,e2) -> If (subst v x c, subst v x e1, subst v x e2)
  | Let (e1,e2,e3) -> 
      (
        if e1 = x then Let (e1,subst v x e2, e3)
        else
          let z = gensym () in
        Let (z, subst v x e2, subst v x (replace_var z e1 e3))
      )
  | Fun (p,b) -> 
    (
      if p = x then e
      else
        let z = gensym() in
        Fun (z,subst v x (replace_var z p b))
        )
  | _ -> e 

let ( let* ) = Option.bind

let rec eval (e:expr): (value,error)result=
  match e with
  | Num n -> Ok(VNum n)
  | Var v -> Error (UnknownVar v)
  | Unit -> Ok (VUnit)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | App (e1,e2) -> 
    (
      match eval e1 with
      | Ok(VFun (p,b))->
        (
          match eval e2 with
          | Ok v -> eval (subst v p b)
          | _ -> Error InvalidApp
        )
      | Ok _ -> Error InvalidApp
      | Error e -> Error e 
    )
  | If (e1, e2, e3) ->
    (match eval e1 with
     | Ok (VBool true) -> eval e2
     | Ok (VBool false) -> eval e3
     | _ -> Error InvalidIfCond
     )
  | Let (x, e1, e2) ->
      (match eval e1 with
      | Ok v1 -> eval (subst v1 x e2)
      | Error e -> Error e)
  | Fun (x, e) -> Ok (VFun (x, e))
  | Bop (op, e1, e2) -> (
      match eval e1 with
      | Ok v1 -> (
          match eval e2 with
          | Ok v2 -> eval_bop op v1 v2
          | Error err -> Error err)
      | Error err -> Error err)

and eval_bop op v1 v2 =
  match op, v1, v2 with
  | Add, VNum n1, VNum n2 -> Ok (VNum (n1 + n2))
  | Sub, VNum n1, VNum n2 -> Ok (VNum (n1 - n2))
  | Mul, VNum n1, VNum n2 -> Ok (VNum (n1 * n2))
  | Div, VNum n1, VNum n2 -> 
      if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
  | Mod, VNum n1, VNum n2 -> 
      if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
  | Lt, VNum n1, VNum n2 -> Ok (VBool (n1 < n2))
  | Lte, VNum n1, VNum n2 -> Ok (VBool (n1 <= n2))
  | Gt, VNum n1, VNum n2 -> Ok (VBool (n1 > n2))
  | Gte, VNum n1, VNum n2 -> Ok (VBool (n1 >= n2))
  | Eq, VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
  | Neq, VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
  | And, VBool b1, VBool b2 -> Ok (VBool (b1 && b2))
  | Or, VBool b1, VBool b2 -> Ok (VBool (b1 || b2))
  | _ -> Error (InvalidArgs op)
  
  let interp (s : string) : (value, error) result =
    match parse s with
    | Some e -> eval e
    | _ -> Error ParseFail
    
  
  