open Utils

let parse = My_parser.parse

let rec desugar_toplets (tlst:toplet list) : sfexpr = 
  match tlst with
  | [] -> SUnit
  | h::t -> SLet {is_rec = h.is_rec; name = h.name; args = h.args; ty = h.ty; value = h.value; body = desugar_toplets t }

let rec desugar_expr (e : sfexpr) : expr =
  match e with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar v -> Var v
  | SFun f -> 
    (
    let (arg_name, arg_ty) = f.arg in
    match f.args with
    | []   -> Fun(arg_name, arg_ty, (desugar_expr (f.body)))
    | h::t -> Fun(arg_name, arg_ty, (desugar_expr (SFun{arg = h; args = t; body = f.body})))
   )
  | SApp (e1, e2) -> App(desugar_expr e1, desugar_expr e2)
  | SLet l   -> 
    (
    match l.args with
    | [] -> Let{ is_rec = l.is_rec; name = l.name; ty = l.ty;value = desugar_expr(l.value); body = desugar_expr(l.body) }
    | h::t -> let (arg_name, arg_type) = h in
            match (desugar_expr (SLet{  is_rec = l.is_rec; name = l.name; args = t;ty = l.ty;value = l.value;body = l.body })) with 
            | Let dl -> 
              Let{ dl with 
                ty = FunTy(arg_type, dl.ty); 
                value = Fun(arg_name, arg_type, dl.value) 
                 }
            | _   -> Let{  is_rec = l.is_rec; name = l.name;ty = l.ty;value = Unit; body = Unit }
    )
  | SIf (e1, e2, e3) -> If(desugar_expr e1, desugar_expr e2, desugar_expr e3)
  | SBop (b, e1, e2) -> Bop(b, desugar_expr e1, desugar_expr e2)
  | SAssert e -> Assert(desugar_expr e)

let desugar (p : prog) : expr = desugar_expr (desugar_toplets (p))

let rec find_in_ctx (var_name : string) (ctx : (string * ty) list) : ty option =
  match ctx with
  | [] -> None
  | (name, var_ty) :: t ->
      if name = var_name then Some var_ty
      else find_in_ctx var_name t

let type_of (e : expr) : (ty, error) result =
let rec type_of_impl (e : expr) (ctx : (string * ty) list) : (ty, error) result =
  match e with
  | Unit -> Ok UnitTy
  | True -> Ok BoolTy
  | False -> Ok BoolTy
  | Num _ -> Ok IntTy

  | Var v -> 
      (match find_in_ctx v ctx with
      | Some var_ty -> Ok var_ty
      | None -> Error (UnknownVar v))

  | If (ec, et, ef) ->
      (match type_of_impl ec ctx with
      | Ok tc when tc = BoolTy ->
          (match type_of_impl et ctx with
          | Ok tt ->
              (match type_of_impl ef ctx with
              | Ok tf when tt = tf -> Ok tt
              | Ok tf -> Error (IfTyErr (tt, tf))
              | Error er -> Error er)
          | Error er -> Error er)
      | Ok tc -> Error (IfCondTyErr tc)
      | Error er -> Error er)

  | Bop (op, e1, e2) ->
      let check_arithmetic_op e1 e2 =
        match type_of_impl e1 ctx with
        | Ok t1 when t1 = IntTy ->
            (match type_of_impl e2 ctx with
            | Ok t2 when t2 = IntTy -> Ok IntTy
            | Ok t2 -> Error (OpTyErrR (op, IntTy, t2))
            | Error er -> Error er)
        | Ok t1 -> Error (OpTyErrL (op, IntTy, t1))
        | Error er -> Error er
      in

      let check_comparison_op e1 e2 =
        match type_of_impl e1 ctx with
        | Ok t1 when t1 = IntTy ->
            (match type_of_impl e2 ctx with
            | Ok t2 when t2 = IntTy -> Ok BoolTy
            | Ok t2 -> Error (OpTyErrR (op, IntTy, t2))
            | Error er -> Error er)
        | Ok t1 -> Error (OpTyErrL (op, IntTy, t1))
        | Error er -> Error er
      in

      let check_boolean_op e1 e2 =
        match type_of_impl e1 ctx with
        | Ok t1 when t1 = BoolTy ->
            (match type_of_impl e2 ctx with
            | Ok t2 when t2 = BoolTy -> Ok BoolTy
            | Ok t2 -> Error (OpTyErrR (op, BoolTy, t2))
            | Error er -> Error er)
        | Ok t1 -> Error (OpTyErrL (op, BoolTy, t1))
        | Error er -> Error er
      in

      (match op with
      | Add | Sub | Mul | Div | Mod -> check_arithmetic_op e1 e2
      | Lt | Lte | Gt | Gte | Eq | Neq -> check_comparison_op e1 e2
      | And | Or -> check_boolean_op e1 e2)

  | Fun (x, ty, bd) ->
      (match type_of_impl bd ((x, ty) :: ctx) with
      | Ok bd_ty -> Ok (FunTy (ty, bd_ty))
      | Error er -> Error er)

  | App (e1, e2) ->
      (match type_of_impl e1 ctx with
      | Ok (FunTy (t_in, t_out)) ->
          (match type_of_impl e2 ctx with
          | Ok t2 when t2 = t_in -> Ok t_out
          | Ok t2 -> Error (FunArgTyErr (t_in, t2))
          | Error er -> Error er)
      | Ok t1 -> Error (FunAppTyErr t1)
      | Error er -> Error er)

  | Let l ->
      let check_let_binding ctx value_ctx =
        match type_of_impl l.value value_ctx with
        | Ok t1 when t1 = l.ty ->
            (match type_of_impl l.body ((l.name, t1) :: ctx) with
            | Ok t2 -> Ok t2
            | Error er -> Error er)
        | Ok t1 -> Error (LetTyErr (l.ty, t1))
        | Error er -> Error er
      in

      if l.is_rec 
      then check_let_binding ctx ((l.name, l.ty) :: ctx)
      else check_let_binding ctx ctx

  | Assert e ->
      (match type_of_impl e ctx with
      | Ok t when t = BoolTy -> Ok UnitTy
      | Ok t -> Error (AssertTyErr t)
      | Error er -> Error er)
in
type_of_impl e []

exception AssertFail
exception DivByZero
exception TypeError

let eval (e : expr) : value =
  let rec eval_impl (e : expr) (ev : value env) : value =
      match e with
      | Unit             -> VUnit
      | True             -> VBool(true)
      | False            -> VBool(false)
      | Num n            -> VNum(n)
      | Var v            -> Env.find (v) (ev)
      | If  (ec, et, ef) -> (
          match (eval_impl (ec) (ev)) with
          | VBool cond -> if (cond) then (eval_impl (et) (ev)) else (eval_impl (ef) (ev))
          | _          -> raise TypeError
      )
      | Bop (op, e1, e2) -> (
          match (eval_impl (e1) (ev)) with
          (* Arithmetic & Comparison Operations *)
          | VNum n1  -> (
              match (eval_impl (e2) (ev)) with
              | VNum n2  -> (
                  match op with
                  | Add -> VNum(n1 + n2)
                  | Sub -> VNum(n1 - n2)
                  | Mul -> VNum(n1 * n2)
                  | Div -> if (n2 = 0) then (raise DivByZero) else VNum(n1 / n2)
                  | Mod -> VNum(n1 mod n2)
                  | Lt  -> VBool(n1 < n2)
                  | Lte -> VBool(n1 <= n2)
                  | Gt  -> VBool(n1 > n2)
                  | Gte -> VBool(n1 >= n2)
                  | Eq  -> VBool(n1 = n2)
                  | Neq -> VBool(n1 <> n2)
                  | _   -> raise TypeError
              )
              | _        -> raise TypeError
          )
          (* Boolean Operations *)
          | VBool b1 -> (
              match op with
              | And -> if b1 then (eval_impl (e2) (ev)) else VBool(false)
              | Or  -> if b1 then VBool(true) else (eval_impl (e2) (ev))
              | _   -> raise TypeError
          )
          | _        -> raise TypeError
      )
      | Fun (arg, _, bd) -> VClos{ name = None; arg = arg; body = bd; env = ev }
      | App (e1, e2)     -> (
          match (eval_impl (e1) (ev)) with
          | VClos clos -> (
              match clos.name with
              | Some s -> (eval_impl (clos.body) (Env.add (clos.arg) (eval_impl (e2) (ev)) (Env.add (s) (VClos{ name = clos.name; arg = clos.arg; body = clos.body; env = clos.env }) (clos.env))))
              | _      -> (eval_impl (clos.body) (Env.add (clos.arg) (eval_impl (e2) (ev)) (clos.env)))
          )
          | _          -> raise TypeError
      )
      | Let l            -> (
          let v1 = eval_impl (l.value) (ev)
          in
          let v1 = if l.is_rec then match v1 with
                  | VClos clos -> VClos{ clos with name = Some l.name }
                  | _          -> raise TypeError
              else v1
          in
          eval_impl (l.body) (Env.add (l.name) (v1) (ev))
      )
      | Assert e         -> (
          match (eval_impl (e) (ev)) with
          | VBool cond -> if cond then VUnit else (raise AssertFail)
          | _          -> raise TypeError
      )
  in
  eval_impl (e) (Env.empty)


let interp (s : string) : (value, error) result =
  match (parse (s)) with
    | Some p -> 
      (
        let e = desugar (p)
        in
        match (type_of (e)) with
        | Ok _ty   -> Ok(eval (e))
        | Error er -> Error(er)
        )
    | _ -> Error(ParseErr)