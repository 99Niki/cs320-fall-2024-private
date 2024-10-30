type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree


  let sum_tr tr =
    match tr with 
    | Leaf -> 0
    | _ ->
        let rec help_sum tr acc =
        match tr with
        | Leaf -> acc
        | Node (x, l, r) -> help_sum l (help_sum r (acc + x))
        in help_sum tr 0
  