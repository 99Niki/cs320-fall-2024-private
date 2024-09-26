let gen_fib (lst:int list) (k:int) :int=
  match lst with
  | [] -> -1
  | _ when k < 0 -> -1
  | _ ->
    let rec get_next i current_list =
      let l = List.length current_list in
      if i = k then List.nth current_list 0
      else
        let next_value = cal_next current_list l in
        get_next (i + 1) (current_list @ [next_value])
    and cal_next lst len =
      match lst with
      | [] -> 0
      | _ ->
        let rec sum_last_elements lst count acc =
          if count = 0 then acc
          else
            match lst with
            | [] -> acc
            | hd :: tl -> sum_last_elements tl (count - 1) (acc + hd)
        in
        sum_last_elements lst len 0
        in
    get_next (List.length lst) lst