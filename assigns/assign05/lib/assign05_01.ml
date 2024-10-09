type 'a test = 
| TestCase of 'a
| TestList of 'a test list

let rec fold_left op acc l =
  match l with
  | TestCase x -> op acc x
  | TestList [] -> acc
  | TestList (h::t) -> fold_left op (fold_left op acc h) (TestList t)