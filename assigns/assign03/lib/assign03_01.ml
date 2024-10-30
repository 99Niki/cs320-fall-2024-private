let mk_unique_keys:(string * int )list -> (string * int )list =
   let rec insert (k,v) l =
        match l with
        | [] -> []
        | (y,w)::t -> if k=y then (k,w+v)::t else (y,w):: insert (k,v)l
   in
   let rec check_list out l=
    match l with 
    | [] -> []
    | h::t -> check_list (insert h out) t 
    in
    check_list []
