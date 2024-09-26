let opposite_signs a b =
  (a < 0 && b > 0) || (a > 0 && b < 0)

let group lst =
  let rec aux lst current_group acc =
    match lst, current_group with
    | [], [] -> Some (List.rev acc)  (* If both the list and group are empty, we're done *)
    | [], _ -> Some (List.rev (List.rev current_group :: acc))  (* End of list, add last group *)
    | 0 :: (n1 :: _ as rest), x :: _ when opposite_signs x n1 ->
      (* Handle valid zeros with opposite-signed neighbors *)
      aux rest [] (List.rev current_group :: acc)
    | 0 :: _, _ -> None  (* Invalid zero case *)
    | x :: rest, _ when x <> 0 ->
      (* Add nonzero element to the current group *)
      if current_group = [] || (match current_group with y :: _ -> y * x > 0 | [] -> true) then
        aux rest (x :: current_group) acc
      else
        None  (* Invalid: Switching between signs without a zero *)
    | _ -> None
  in
  if lst = [] then Some []  (* Empty list is valid *)
  else aux lst [] []
