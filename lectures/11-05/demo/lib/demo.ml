open Utils
<<<<<<< HEAD
(*include My_parser*)
let parser = My_parser.parser
=======
>>>>>>> upstream/main

let parse = My_parser.parse

let desugar =
  let rec go = function
  | [] -> Fun("x", Var "x")
  | [_, e] -> e
  | (x, e) :: ls -> Let (x, e, go ls)
  in go

let expr_of_val (VFun (x, e)) = Fun (x, e)

<<<<<<< HEAD
*)
let expr_of_val v =
  match v with
  | VFun (x,e) -> Fun (x,e)

(* [v / x] y = if x = y then v else x
=======
let replace_var x y =
  let rec go = function
    | Var z -> if z = y then Var x else Var z
    | App (e1, e2) -> App (go e1, go e2)
    | Fun (z, e) -> if z = y then Fun (z, e) else Fun (z, go e)
    | Let (z, e1, e2) -> if z = y then Let (z, go e1, e2) else Let(z, go e1, go e2)
  in go
>>>>>>> upstream/main

let subst v x =
  let rec go = function
    | Var y -> if x = y then expr_of_val v else Var y
    | App (e1, e2) -> App (go e1, go e2)
    | Fun (y, e) ->
      if x = y
      then Fun (y, e)
      else
        let z = gensym () in
        Fun (z, go (replace_var z y e))
    | Let (y, e1, e2) ->
      if x = y
      then Let (y, go e1, e2)
      else
        let z = gensym () in
        Let (z, go e1, go (replace_var z y e2))
  in go

let ( let* ) = Option.bind

<<<<<<< HEAD
*)
let rec rec replace_var x y e =
  match e with
  | Var z -> if z=y then Var x else Var z
  | App (e1,e2) -> App (replace_var z y e1, replace_var z, y, e2)
  | Fun (z,e) -> Fun (z, replace_var x y e)


let rec subst v x e =
  match e with
  | Var y -> if x=y then expr_of_val v else Var x
  | App (e1,e2) -> App (subst v x e1, subst v s e2)
  | Fun (y,e) -> 
        if x =y then VFun (y,e) 
         else let z = gensym() in VFun (z, subst v x (replace_var z x e))

let rec eval e =
  match e with
  | Var x -> None
  | Fun(x,e) -> Some(VFun(x,e))
  | App (e1,e2) -> 
    (
      match eval e1 with
      | Some(VFun(x,e)) -> 
        (
          match eval e2 with
          | Some v2  -> eval (subst v2,x,e)
          | _ -> None
        )

    )

=======
let string_of_expr =
  let rec go = function
  | Var x -> x
  | App (e1, e2) -> "(" ^ go e1 ^ " " ^ go e2 ^ ")"
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ go e ^ ")"
  | Let (x, e1, e2) -> "(let " ^ x ^ " = " ^ go e1 ^ " in " ^ go e2 ^ ")"
 in go
>>>>>>> upstream/main

let eval =
  let rec go = function
    | Var _ -> None
    | Fun (x, e) -> Some (VFun (x, e))
    | App (e1, e2) ->
      let* (VFun (x, e)) = go e1 in
      let* v2 = go e2 in
      go (subst v2 x e)
    | Let (x, e1, e2) ->
      let* v = go e1 in
      go (subst v x e2)
  in go

let interp str =
  let* prog = parse str in
  let expr = desugar prog in
  eval expr

let interp' str =
  match interp str with
  | Some v -> string_of_expr (expr_of_val v)
  | None -> "ERROR"
