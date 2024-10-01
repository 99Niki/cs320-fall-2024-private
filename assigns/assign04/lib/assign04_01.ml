(*funcs begin with start, stop with fail pred, and return the last_function_standing*)

(*count the steps*)
let rec count_step f x pred steps=
    if steps = 1000  then 1000  (*infinite*)
    else if pred (f x) then (steps+1)
    else count_step f (f x) pred (steps+1)

let rec find_max funcs start pred max_fun max_span =
  match funcs with
  | [] -> max_fun  
  | h :: t -> 
      let span = count_step h start pred 0 in
      if span =1000 then find_max t start pred None max_span 
      else if span > max_span then
        find_max t start pred (Some h) span  
      else if span = max_span then
        find_max t start pred None max_span  
      else
        find_max t start pred max_fun max_span  


let last_function_standing (funcs: ('a -> 'a) list) (start: 'a) (pred:('a -> bool)) =
    match funcs with
    | [] -> None
    | _ -> find_max funcs start pred None (-1)
    


