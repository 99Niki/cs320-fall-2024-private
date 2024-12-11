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
      | [] -> None
      | [TVar "$_out", t] -> Some t  (* Optimization to not build a full solution *)
      | (TPair (t1, t2), TPair (t1', t2')) :: cs ->
          go ((t1, t1') :: (t2, t2') :: cs)
      | (TList t1, TList t2) :: cs | (TOption t1, TOption t2) :: cs ->
          go ((t1, t2) :: cs)
      | (t1, t2) :: cs when t1 = t2 -> go cs
      | (TFun (t1, t2), TFun (t1', t2')) :: cs ->
          go ((t1, t1') :: (t2, t2') :: cs)
      | (TVar x, t) :: cs ->
          if VarSet.mem x (fvs t) then None
          else go (ty_subst_cs t x cs)
      | (t, TVar x) :: cs -> go ((TVar x, t) :: cs)
      | _ -> None
    in
    let tys = go (cs @ [TVar "$_out", t_out]) in
    match tys with
    | None -> None
    | Some t' ->   
      let s = VarSet.to_list (fvs t') in
      Some (Forall (s, t'))

let rec instantiate bnd_vars t =
  match bnd_vars with
  | [] -> t
  | x :: bnd_vars ->
      let b = TVar (gensym ()) in
      instantiate bnd_vars (ty_subst b x t)
      
let type_of' (ctxt : stc_env) (e : expr) : ty * (ty * ty) list =
  let rec go ctxt e =
    match e with
    | Unit -> TUnit, []
    | True | False -> TBool, []
    | Int _ -> TInt, []
    | Float _ -> TFloat, []
    | Var x ->
      let bnd_vars, t = 
        (match Env.find x ctxt with
        | Forall (vars, t) -> vars, t) 
      in
      instantiate bnd_vars t, []
    | ENone -> TOption (TVar (gensym ())), []
    | ESome e -> 
        let t, c = go ctxt e in 
        TOption (t), c
    | Nil -> TList (TVar (gensym ())), []
    | Bop (op, e1, e2) -> 
        let t1, c1 = go ctxt e1 in
        let t2, c2 = go ctxt e2 in
        (match op with
        | Cons -> (TList t1, (t2, TList t1) :: c1 @ c2)
        | Add | Sub | Mul | Div | Mod -> (TInt, [(t1, TInt); (t2, TInt)] @ c1 @ c2)
        | AddF | SubF | MulF | DivF | PowF -> (TFloat, [(t1, TFloat); (t2, TFloat)] @ c1 @ c2)
        | Lt | Lte | Gt | Gte | Eq | Neq ->  (TBool, (t1, t2) :: c1 @ c2)
        | And | Or ->  (TBool, [(t1, TBool); (t2, TBool)] @ c1 @ c2)
        | Concat -> 
            let alpha = TList (TVar (gensym ())) in 
            (alpha, [(t1, alpha); (t2, alpha)] @ c1 @ c2)
        | Comma -> (TPair (t1, t2), c1 @ c2))
    | If (e1, e2, e3) -> 
        let t1, c1 = go ctxt e1 in
        let t2, c2 = go ctxt e2 in
        let t3, c3 = go ctxt e3 in
        (t3, [(t1, TBool); (t2, t3)] @ c1 @ c2 @ c3)
    | Assert False -> TVar (gensym ()), []
    | Assert e -> 
        let t, c = go ctxt e in
        TUnit, (t, TBool) :: c
    | Annot (e, ty) -> 
        let t, c = go ctxt e in
        ty, (ty, t) :: c
    | OptMatch { matched; some_name; some_case; none_case } -> 
        let tm, cm = go ctxt matched in
        let alpha = TVar (gensym ()) in
        let ctxt_cons = Env.add some_name (Forall ([], alpha)) ctxt in
        let ts, cs = go ctxt_cons some_case in
        let tn, cn = go ctxt none_case in
        (tn, [(tm, TOption alpha); (ts, tn)] @ cm @ cs @ cn)
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } -> 
        let tm, cm = go ctxt matched in
        let alpha = TVar (gensym ()) in
        let ctxt_cons = Env.add hd_name (Forall ([], alpha)) (Env.add tl_name (Forall ([], TList alpha)) ctxt) in
        let tc, cc = go ctxt_cons cons_case in
        let tn, cn = go ctxt nil_case in
        (tn, [(tm, TList alpha); (tc, tn)] @ cm @ cc @ cn)
    | PairMatch { matched; fst_name; snd_name; case } ->
        let tm, cm = go ctxt matched in
        let alpha = TVar (gensym ()) in
        let beta = TVar (gensym ()) in
        let ctxt_cons = Env.add fst_name (Forall ([], alpha)) (Env.add snd_name (Forall ([], beta)) ctxt) in
        let tc, cc = go ctxt_cons case in
        (tc, (tm, TPair (alpha, beta)) :: cm @ cc)
    | Fun (x, ty_opt, e) ->
        let x_ty = match ty_opt with Some t -> t | None -> TVar (gensym ()) in
        let ctxt_body = Env.add x (Forall ([], x_ty)) ctxt in
        let t, c = go ctxt_body e in
        (TFun (x_ty, t), c)
    | App (e1, e2) ->
        let t1, c1 = go ctxt e1 in
        let t2, c2 = go ctxt e2 in
        let alpha = TVar (gensym ()) in
        (alpha, (t1, TFun (t2, alpha)) :: c1 @ c2)
    | Let { is_rec; name; value; body } ->
        if is_rec then
            let alpha = TVar (gensym ()) in
            let beta = TVar (gensym ()) in
            let ctxt' = Env.add name (Forall ([], TFun (alpha, beta))) ctxt in
            let tv, cv = go ctxt' value in
            let ctxt'' = Env.add name (Forall ([], tv)) ctxt in
            let tb, cb = go ctxt'' body in
            (tb, (tv, TFun (alpha, beta)) :: cv @ cb)
        else 
            let tv, cv = go ctxt value in
            let tb, cb = go (Env.add name (Forall ([], tv)) ctxt) body in
            (tb, cv @ cb)
  in go ctxt e
  
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
  | Var x -> ( Env.find x env)
  | Assert e -> (
      match eval_expr env e with
      | VBool true -> VUnit
      | _ -> raise AssertFail
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
  | Let { is_rec; name; value; body } ->
    if is_rec then
      (match eval_expr env value with
      | VClos { name=n; arg = param; body = nbody; env = closure_env } -> 
        if (n = None) then
          let value_val = VClos {name=Some name; arg = param; body=nbody; env = closure_env} in
          eval_expr (Env.add name value_val env) body
        else raise RecWithoutArg
      | _ -> failwith "6")
    else
      let value_val = eval_expr env value in
      eval_expr (Env.add name value_val env) body




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
