open Utils

let parse (s: string) : expr option =
  try
    match My_parser.parse s with
    | Some e -> e 
    | None -> None
  with
  | _ -> None

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
      | _ -> Error InvalidApp
    )
  | Bop (op,e1,e2) -> 
    (
        match (eval e1, eval e2) with
      | (Ok (VNum n1), Ok (VNum n2)) -> 
        (
          match op with
          | Add -> Ok (VNum (n1 + n2))
          | Sub -> Ok (VNum (n1 - n2))
          | Mul -> Ok (VNum (n1 * n2))
          | Div -> (if n2 = 0 then Error DivByZero 
                    else Ok (VNum (n1 / n2)))
          | Mod -> (if n2 = 0 then Error DivByZero 
                    else Ok (VNum (n1 mod n2)))
          | Lt -> Ok (VBool (n1 < n2))
          | Lte -> Ok (VBool (n1 <= n2))
          | Gt -> Ok (VBool (n1 > n2))
          | Gte -> Ok (VBool (n1 >= n2))
          | Eq -> Ok (VBool (n1 = n2))
          | Neq -> Ok (VBool (n1 <> n2))
          | _ -> Error (InvalidArgs op)
        )
      | (Ok (VBool b1), Ok (VBool b2)) -> 
              (match op with
                | And -> Ok (VBool (b1 && b2))
                | Or -> Ok (VBool (b1 || b2))
                | _ -> Error (InvalidArgs op)
                )
      | _ -> Error (InvalidArgs op)
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
  
  
  
  let interp (s : string) : (value, error) result =
    match parse s with
    | Some prog -> eval prog
    | _ -> Error ParseFail
    
  
  