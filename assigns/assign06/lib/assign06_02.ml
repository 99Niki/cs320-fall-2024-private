open Utils
   
let  pop stack n =
   let rec aux stack n acc =
   if n = 0 then Some (List.rev acc, stack)
   else match stack with
      | h::t -> aux t (n-1) (h::acc)
      | [] -> None
 in 
 aux stack n []

let parse lst =
   let rec helper lst stack=
   match lst with
   | [] -> (match stack with
            | [e] ->Some e
            | _ -> None )
   | TNum a :: rest -> helper rest (Num a ::stack)
   | TAdd ::rest  -> (match pop stack 2 with
                     | Some ([e1;e2],remaining) -> helper rest (Add(e2,e1)::remaining)
                     | _ -> None)
   | TLt::rest -> (match pop stack 2  with
                     | Some ([e1;e2],remaining) -> helper rest (Lt(e2,e1)::remaining)
                     | _ -> None)
   | TIte::rest -> (match pop stack 3 with
                        | Some ([e1;e2;e3],remaining) -> helper rest (Ite(e3,e2,e1)::remaining)
                        | _ -> None)
 in
 helper lst []

