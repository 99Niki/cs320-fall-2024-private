open Utils
(*include My_parser*)
let parser = My_parser.parser

(*

--------------------------
fun x -> e \|/ fun x -> e

e1 \|/ fun x -> e      e2 \|/ v2      [v2 / x] e \|/ v
------------------------------------------------------
              e1 e2 \|/ v

*)
let expr_of_val v =
  match v with
  | VFun (x,e) -> Fun (x,e)

(* [v / x] y = if x = y then v else x

[v / x] (e1 e2) = ([v / x] e1) ([v / x] e2)

[v / x] (fun y -> e) =
  if x = y then
    fun y -> x
  else
    fun z -> [v / x] ([z / y] e)

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


let _ = ignore (Var "x")
