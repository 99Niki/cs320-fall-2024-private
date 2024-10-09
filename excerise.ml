type 'a node =
| One of 'a
| Many of 'a node list

let rec flattern acc lst =
  match lst with
  | [] -> List.rev acc
  | One x ::t-> flattern (x::acc) t
  | Many l ::t-> flattern (flattern acc l) l
  