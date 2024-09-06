let sqrt n =
  if n <=0 then 0
  else
   let rec find_sqrt k =
     if n <= k*k  then k
     else find_sqrt (k + 1) in find_sqrt 0
