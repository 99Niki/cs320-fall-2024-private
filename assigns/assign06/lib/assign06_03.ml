open Utils

let rec type_of (e:expr) :ty option =
  match e with
  | Num _ -> Some TInt
  | Add (e1,e2) -> ( match type_of e1, type_of e2 with
                    | Some TInt , Some TInt -> Some TInt
                    | _ -> None)
  | Lt (e1,e2) -> ( match type_of e1, type_of e2 with
                    | Some TInt , Some TInt -> Some TBool
                    | _ -> None)
  | Ite (eb, e1,e2) -> ( match type_of eb,type_of e1, type_of e2 with
                          | Some TBool,Some t1 , Some t2 when t1=t2 -> Some t1
                          | _ -> None)