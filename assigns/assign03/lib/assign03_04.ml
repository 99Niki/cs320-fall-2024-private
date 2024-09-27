let opposite_signs a b =
  (a < 0 && b > 0) || (a > 0 && b < 0)

let group lst =
match lst with
| [] -> None
| 0::_ -> None
| _ ->
  let rec aux current acc = function
    | [] -> if current = [] then Some (List.rev acc) else Some (List.rev (List.rev current :: acc))
    | a :: t ->
        if a <> 0 then
          match current with
          | [] -> aux [a] acc t 
          | hd :: _ ->
              if (a < 0 && hd < 0) || (a > 0 && hd > 0) then
                aux (a :: current) acc t 
              else None 
        else
          match (current, t) with
          | ([], _) -> None 
          | (_, []) -> None 
          | (current_hd :: _, x :: _) ->
              if opposite_signs current_hd x then
                (* Move the current group to acc and start a new group *)
                aux [] (List.rev current :: acc) t
              else None
  in
  aux [] [] lst

