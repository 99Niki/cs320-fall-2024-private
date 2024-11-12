open Utils

let parse = My_parser.parse

let rec subst (v:value) (x:string) (e:expr):expr =
  match e with
  | Num n -> (Num n)
  | Var y -> if y = x then 
              (match v with
                | VNum n -> Num n
                | VBool b -> if b then True else False
                | VUnit -> Unit
                | VFun (v1, e1) -> Fun (v1, e1)
                )
              else Var y
  | Unit -> Unit
  | True -> True
  | False -> False
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) ->
      if y = x then Let (y, subst v x e1, e2)
      else Let (y, subst v x e1, subst v x e2)
  | Fun (x2, e2) ->
      if x2 = x then Fun (x2, e2)
      else Fun (x2, subst v x e2)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)

let eval_bop op e1 e2 =
match (eval e1, eval e2) with
| (Ok (VNum n1), Ok (VNum n2)) -> (
    match op with
    | Add -> Ok (VNum (n1 + n2))
    | Sub -> Ok (VNum (n1 - n2))
    | Mul -> Ok (VNum (n1 * n2))
    | Div -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
    | Mod -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
    | Lt -> Ok (VBool (n1 < n2))
    | Lte -> Ok (VBool (n1 <= n2))
    | Gt -> Ok (VBool (n1 > n2))
    | Gte -> Ok (VBool (n1 >= n2))
    | Eq -> Ok (VBool (n1 = n2))
    | Neq -> Ok (VBool (n1 <> n2))
    | _ -> Error InvalidArgs)
| (Ok (VBool b1), Ok (VBool b2)) -> 
        (match op with
          | And -> Ok (VBool (b1 && b2))
          | Or -> Ok (VBool (b1 || b2))
          | Eq -> Ok (VBool (b1 = b2))
          | Neq -> Ok (VBool (b1 <> b2))
          | _ -> Error InvalidArgs)
| _ -> Error InvalidArgs)


let rec eval (e:expr): (value,error)result=
  match e with
  | Num n -> Ok(VNum n)
  | Var v -> Error (UnknownVar v)
  | Unit -> Ok VUnit
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | App (e1,e2) -> 
    (
      match eval e1 with
      | Ok(VFun (x,e))->
        (
          match eval e2 with
          | Ok v -> subst v x e
          | Error e -> Error e
        )
      | Ok _ -> Error InvalidApp
      | Error e -> Error e
    )
  | Bop (op,e1,e2) -> eval_op e1 e2
  | If (e1, e2, e3) ->
    (match eval e1 with
     | Ok (VBool true) -> eval e2
     | Ok (VBool false) -> eval e3
     | Ok _ -> Error InvalidIfCond
     | Error e -> Error e)
  | Let (x, e1, e2) ->
      (match eval e1 with
      | Ok v1 -> eval (subst v1 x e2)
      | Error e -> Error e)
  | Fun (x, e) -> Ok (VFun (x, e))

  let interp (s : string) : (value, error) result =
    match Parser.parse s with
    | None -> Error ParseFail
    | Some prog -> eval prog
  
  