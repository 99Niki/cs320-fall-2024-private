open Utils
include My_parser

let ty_subst t x =
  let rec go = function
    | TUnit  -> TUnit
    | TInt   -> TInt
    | TFloat -> TFloat
    | TBool -> TBool
    | TVar y -> if x = y then t else TVar y
    | TList ty' -> TList(go ty')
    | TOption ty' -> TOption (go ty')
    | TPair (t1, t2) -> TPair (go t1, go t2)
    | TFun (t1, t2) -> TFun (go t1, go t2)
  in go

let ty_subst_c t x (t1, t2) = (ty_subst t x t1, ty_subst t x t2)
let ty_subst_cs t x = List.map (ty_subst_c t x)

let rec fvs = function
  | TUnit -> VarSet.empty
  | TInt -> VarSet.empty
  | TFloat -> VarSet.empty
  | TBool -> VarSet.empty
  | TVar x -> VarSet.of_list [x]
  | TList ty' -> fvs ty'
  | TOption ty' -> fvs ty'
  | TPair (t1, t2) -> VarSet.union (fvs t1) (fvs t2)
  | TFun (t1, t2) -> VarSet.union (fvs t1) (fvs t2)


let unify (t_out : ty) (cs : constr list) : ty_scheme option =
  let rec go cs =
    match cs with
    | [] -> Some (Forall ([], t_out)) (* No constraints left, return the result *)
    | (t1, t2) :: rest when t1 = t2 ->
      go rest (* Skip equivalent types *)
    | (TFun (t1, t2), TFun (t1', t2')) :: rest ->
      go ((t1, t1') :: (t2, t2') :: rest) (* Decompose function types *)
    | (TList t1, TList t2) :: rest ->
      go ((t1, t2) :: rest) (* Decompose list types *)
    | (TOption t1, TOption t2) :: rest ->
      go ((t1, t2) :: rest) (* Decompose option types *)
    | (TPair (t1, t2), TPair (t1', t2')) :: rest ->
      go ((t1, t1') :: (t2, t2') :: rest) (* Decompose pair types *)
    | (TVar x, t) :: rest when not (VarSet.mem x (fvs t)) ->
      let rest' = ty_subst_cs t x rest in
      go rest' (* Substitute type variables in the rest *)
    | (t, TVar x) :: rest ->
      go ((TVar x, t) :: rest) (* Symmetrize TVar handling *)
    | _ -> None (* Types are not unifiable *)
  in
  go cs


let rec type_of' (ctxt : stc_env) (e : expr) : ty * (ty * ty) list =
  let rec go e =
    match e with
    | Int _ -> TInt, []
    | Bop (Add, e1, e2) ->
      let t1, c1 = go e1 in
      let t2, c2 = go e2 in
      ( TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2 )
    | If (e1, e2, e3) ->
      let t1, c1 = go e1 in
      let t2, c2 = go e2 in
      let t3, c3 = go e3 in
      ( t2, [(t1, TBool); (t2, t3)] @ c1 @ c2 @ c3 )
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      let t_matched, c_matched = go matched in
      let t_hd = TVar (gensym ()) in
      let ctxt = Env.add hd_name (Forall ([], t_hd)) ctxt in
      let ctxt = Env.add tl_name (Forall ([], TList t_hd)) ctxt in
      let t_cons, c_cons = type_of' ctxt cons_case in
      let t_nil, c_nil = type_of' ctxt nil_case in
      ( t_cons, (t_matched, TList t_hd) :: (t_nil, t_cons) :: c_matched @ c_cons @ c_nil )
    | ESome e_inner ->
      let t_inner, c_inner = go e_inner in
      ( TOption t_inner, c_inner )
    | OptMatch { matched; some_name; some_case; none_case } ->
      let t_matched, c_matched = go matched in
      let t_some = TVar (gensym ()) in
      let ctxt = Env.add some_name (Forall ([], t_some)) ctxt in
      let t_some_case, c_some_case = type_of' ctxt some_case in
      let t_none_case, c_none_case = type_of' ctxt none_case in
      ( t_some_case, (t_matched, TOption t_some) :: (t_some_case, t_none_case) :: c_matched @ c_some_case @ c_none_case )
    | PairMatch { matched; fst_name; snd_name; case } ->
      let t_matched, c_matched = go matched in
      let t_fst = TVar (gensym ()) in
      let t_snd = TVar (gensym ()) in
      let ctxt = Env.add fst_name (Forall ([], t_fst)) ctxt in
      let ctxt = Env.add snd_name (Forall ([], t_snd)) ctxt in
      let t_case, c_case = type_of' ctxt case in
      ( t_case, (t_matched, TPair (t_fst, t_snd)) :: c_matched @ c_case )
    | _ -> failwith "Unsupported expression"
  in go e

let type_of (ctxt: stc_env) (e: expr) : ty_scheme option =
  let (t,c) = type_of' ctxt e in
  unify t c


exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let rec eval_expr (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Nil -> VList []
  | ENone -> VNone
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x -> (
      match Env.find_opt x env with
      | Some v -> v
      | None -> failwith ("Unbound variable: " ^ x)
    )
  | Assert e -> (
      match eval_expr env e with
      | VBool true -> VUnit
      | VBool false -> raise AssertFail
      | _ -> failwith "Assert expects a boolean"
    )
  | ESome e -> VSome (eval_expr env e)
  | Bop (op, e1, e2) ->
    (
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    let int_op f = match v1, v2 with
      | VInt i1, VInt i2 -> VInt (f i1 i2)
      | _ -> failwith "Type error"
    in
    let float_op f = match v1, v2 with
      | VFloat f1, VFloat f2 -> VFloat (f f1 f2)
      | _ -> failwith "Type error"
    in
    match op with
    | Add -> int_op (fun x y -> x + y)
    | Sub -> int_op (fun x y -> x - y)
    | Mul -> int_op (fun x y -> x * y)
    | Div -> (match v1, v2 with
        | VInt _, VInt 0 -> raise DivByZero
        | VInt x, VInt y -> VInt (x / y)
        | _ -> failwith "Type error")
    | Mod -> (match v1, v2 with
        | VInt _, VInt 0 -> raise DivByZero
        | VInt x, VInt y -> VInt (x mod y)
        | _ -> failwith "Type error")
    | AddF -> float_op ( +. )
    | SubF -> float_op ( -. )
    | MulF -> float_op ( *. )
    | DivF -> (match v1, v2 with
        | VFloat _, VFloat 0.0 -> raise DivByZero
        | VFloat x, VFloat y -> VFloat (x /. y)
        | _ -> failwith "Type error")
    | PowF -> float_op ( ** )
    | Cons -> (match v1, v2 with
        | VInt x, VList xs -> VList (VInt x :: xs)
        | _ -> failwith "Cons requires an integer and a list")
    | Concat -> (match v1, v2 with
        | VList xs1, VList xs2 -> VList (xs1 @ xs2)
        | _ -> failwith "Concat requires two lists")
        | Lt -> (match v1, v2 with
        | VInt x, VInt y -> VBool (x < y)
        | _ -> failwith "Lt requires two integers")
    | Lte -> (match v1, v2 with
        | VInt x, VInt y -> VBool (x <= y)
        | _ -> failwith "Lte requires two integers")
    | Gt -> (match v1, v2 with
        | VInt x, VInt y -> VBool (x > y)
        | _ -> failwith "Gt requires two integers")
    | Gte -> (match v1, v2 with
        | VInt x, VInt y -> VBool (x >= y)
        | _ -> failwith "Gte requires two integers")
    | Eq -> (match v1, v2 with
        | VInt x, VInt y -> VBool (x = y)
        | VFloat x, VFloat y -> VBool (x = y)
        | VBool x, VBool y -> VBool (x = y)
        | VList xs1, VList xs2 -> VBool (xs1 = xs2)
        | VPair (x1, x2), VPair (y1, y2) -> VBool (x1 = y1 && x2 = y2)
        | _ -> failwith "Eq requires comparable types")
    | Neq -> (match v1, v2 with
        | VInt x, VInt y -> VBool (x <> y)
        | VFloat x, VFloat y -> VBool (x <> y)
        | VBool x, VBool y -> VBool (x <> y)
        | VList xs1, VList xs2 -> VBool (xs1 <> xs2)
        | VPair (x1, x2), VPair (y1, y2) -> VBool (x1 <> y1 || x2 <> y2)
        | _ -> failwith "Neq requires comparable types")
    | And -> (match v1, v2 with
      | VInt x, VInt y -> VBool (x <> 0 && y <> 0)
      | _ -> failwith "And requires two integers")
      | Or -> (match v1, v2 with
        | VInt x, VInt y -> VBool (x <> 0 || y <> 0)
        | _ -> failwith "Or requires two integers")
    | Comma -> failwith "Comma is not a valid operation"
    )
  | If (cond, e_then, e_else) -> (
      match eval_expr env cond with
      | VBool true -> eval_expr env e_then
      | VBool false -> eval_expr env e_else
      | _ -> failwith "Condition of if must be a boolean"
    )
  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } -> (
      match eval_expr env matched with
      | VList [] -> eval_expr env nil_case
      | VList (hd :: tl) ->
          let env' = Env.add hd_name hd (Env.add tl_name (VList tl) env) in
          eval_expr env' cons_case
      | _ -> failwith "List match requires a list"
    )
  | OptMatch { matched; some_name; some_case; none_case } -> (
      match eval_expr env matched with
      | VSome v ->
          let env' = Env.add some_name v env in
          eval_expr env' some_case
      | VNone -> eval_expr env none_case
      | _ -> failwith "Option match requires an option"
    )
  | PairMatch { matched; fst_name; snd_name; case } -> (
      match eval_expr env matched with
      | VPair (v1, v2) ->
          let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
          eval_expr env' case
      | _ -> failwith "Pair match requires a pair"
    )
  | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
  | App (f, e) -> (
      match eval_expr env f with
      | VClos { name = _; arg; body; env = clos_env } ->
          let arg_val = eval_expr env e in
          let env' = Env.add arg arg_val clos_env in
          eval_expr env' body
      | _ -> failwith "Application requires a function"
    )
  | Annot (e, _) -> eval_expr env e
  | Let {is_rec=true; name; value; body} ->
    (match value with
     | Fun (arg, _, bdy) ->
       let clos = VClos {name=Some name; arg; body=bdy; env} in
       eval_expr (Env.add name clos env) body
     | _ -> raise RecWithoutArg)
  | Let {is_rec=false; name; value; body} ->
    let v = eval_expr env value in
    eval_expr (Env.add name v env) body



let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
