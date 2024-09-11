let pow n k = 
if k<0 then 0
else let rec cal_pow n k = 
 if k=0 then 1
  else n* cal_pow n (k-1) in cal_pow n k;;
