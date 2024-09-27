(* Define the tree type *)
type tree =
  | Leaf of int
  | Node of tree list

(* Calculate the height of the tree *)
let rec height t =
  match t with
  | Leaf _ -> 0
  | Node [] -> 0
  | Node children -> 1 + List.fold_left (fun acc subtree -> max acc (height subtree)) 0 children

(* Custom flatten function *)
let rec flatten lst =
  match lst with
  | [] -> []
  | x :: xs ->
      match x with
      | [] -> flatten xs
      | _ -> x @ flatten xs  (* Use @ to concatenate lists *)

(* Collapse function *)
let rec collapse h t =
  if h <= 0 then
    failwith "Height must be positive"
  else
    match t with
    | Leaf v -> Leaf v  (* Leaves are unchanged *)
    | Node children ->
        if height t <= h then
          t  (* No need to collapse if the height is less than or equal to h *)
        else
          let new_children = 
            flatten (List.map (fun subtree ->
              if height subtree = (h - 1) then
                match subtree with
                | Leaf v -> [Leaf v]  (* Keep leaf nodes as they are *)
                | Node child_list -> child_list  (* Replace Node with its children *)
              else
                [collapse h subtree]  (* Recursively collapse other subtrees *)
            ) children)
          in
          Node new_children

