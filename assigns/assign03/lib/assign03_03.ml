type tree =
  | Leaf of int
  | Node of tree list

let rec height t =
  match t with
  | Leaf _ -> 0
  | Node [] -> 0
  | Node children -> 1 + List.fold_left (fun acc child -> max acc (height child)) 0 children


(*if the height of the tree is already h or greater than it, it don NOT need to collapse*)
(*else start at the h+1 level, the node of tree list all become the leaf _*)
let rec collapse h t =
  let l = height t in
  if h >= l then t  (* If the height is already h or greater, return the tree unchanged *)
  else 
    let rec helper current_height t =
      match t with
      | Leaf v -> t  (* A leaf remains a leaf *)
      | Node [] -> t  (* An empty node remains an empty node *)
      | Node children ->
          if current_height > h then
            Node []  (* Collapse to an empty node if the current height exceeds h *)
          else
            let new_children = List.map (helper (current_height + 1)) children in
            Node new_children  (* Process and return children *)
    in
    helper (h + 1) t  (* Start processing from level h + 1 *)