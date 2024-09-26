
let rec update_value k v lst=
    match lst with 
    | [] -> [(k,v)]
    | hd::tl -> let key = fst hd in
                let value = snd hd in
                    if  k = key then (key, v+value)::tl
                    else (key,value)::update_value k v tl


let rec mk_unique_keys (alst : (string * int) list) =
    match alst with
    | [] ->[]
    | (k,v)::tl -> let updated_list = update_value k v (mk_unique_keys tl)
                     in
                     updated_list
