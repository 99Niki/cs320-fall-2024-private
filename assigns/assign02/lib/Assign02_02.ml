type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let mk_matrix (lst: float list) ((r, c): int * int) : matrix =
if r=0 || c=0 then{ entries =[]; rows=r; cols=c}
else
  let rec filling_rows lst remaining_rows =
    if remaining_rows = 0 then []
    else
      let rec extract_row lst c acc =
        match lst, c with
        | _, 0 -> (List.rev acc, lst)  
        | x::xs, c -> extract_row xs (c - 1) (x :: acc)  
        | [], _ -> (List.rev acc, [])  
      in
      let (new_row, remaining_lst) = extract_row lst c [] 
      in
      new_row :: filling_rows remaining_lst (remaining_rows - 1) 
  in
  { entries = filling_rows lst r; rows = r; cols = c }
