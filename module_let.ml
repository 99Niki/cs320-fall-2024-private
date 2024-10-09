module type STORE =
sig
  type 'a t
  val new_store:'a t
  val push : 'a ->'a t -> 'a t
  val pop : 'a t -> 'a option * 'a t
  val top : 'a t -> 'a option
end

module Stack: STORE = 
struct
  type 'a t ='a list
  let new_store = []
  let push x s = x::s
  let pop s =
    match s with
    | [] -> (None, [])
    | h::t -> (Some h, t)
  let top s =
    match s with
    | [] -> None
    | h::_ -> Some h
end

module Queue: STORE = 
struct
  type 'a t ='a list
  let new_store = []
  let push x s = s@[x]
  let pop s =
    match s with
    | [] -> (None, [])
    | h::t -> (Some h, t)
  let top s =
    match s with
    | [] -> None
    | h::_ -> Some h
end

type expr =
| Add of expr*expr
| Sub of expr*expr
| Mult of expr*expr
| Div of expr*expr
| Num of int

(*after pushing it into the ml file, we can call it into the main file*)
(*let x = module_let.Stack...*)
(*in the case of without create a mli file, then all the function in ml willl be public
But with the mli file, without in mli it will private in ml*)