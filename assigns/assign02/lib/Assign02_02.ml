type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let rec remove_n lst n =
  match lst with
   | [] -> [] 
   | _ :: tl -> 
     if n = 0 then lst 
     else remove_n tl (n - 1) 
    
let mk_matrix lst (r,c)=
if r =0 || c=0 then {entries =[];rows=r;cols =c}
else
let rec n_rows r lst =
if r =0 then []
else
 let rec mk_row lst c =
    match lst with
     | [] -> []
     | hd::tl ->
            if c =  0 then [] 
            else hd:: mk_row tl (c-1)
 in
        let row = mk_row lst c in
        let remain_lst = remove_n lst c in
        row :: n_rows (r - 1) (remain_lst) 
in
    { entries = n_rows r lst; rows = r; cols = c };;
