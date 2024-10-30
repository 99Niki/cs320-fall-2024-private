open Utils

type value = 
| VNum of int
| VBool of bool


let rec eval (e:expr) : value =
    match e with
    | Num v -> VNum v
    | Add (e1,e2) -> ( match eval e1, eval e2 with
                      | VNum v1 , VNum v2-> VNum (v1+v2)
                      | _ -> failwith "Mismatch type expression"
                        )
    | Lt (e1,e2) -> ( match eval e1, eval e2 with
                        | VNum v1 , VNum v2-> VBool(v1<v2)
                        | _ -> failwith "Mismatch type expression "
                        )
    | Ite (eb, e1,e2) -> ( match eval eb with
                            | VBool vb -> if vb then eval e1 else eval e2
                            | _ -> failwith "Mismatch type expression "
                            )