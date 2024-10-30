let pow n k = 
if k<0 then 0
else let rec cal_pow n k = 
 if k=0 then 1
  else n* cal_pow n (k-1) in cal_pow n k

let is_prime n =
  if n < 2 then false
  else if n = 2 then true
  else
    let rec check_divisors d =
      if d * d > n then true
      else if n mod d =0 then false
      else check_divisors(d+1) in check_divisors 2

let nth_prime n =
if n<0 then 0
else let rec find_prime count num =
     if is_prime num then
         if count = n then num
         else find_prime (count+1) (num+1)
     else find_prime count (num+1) in find_prime 0 2


let nth s n =
  let num = nth_prime n in
    let rec sequence s count =
        if s mod num <> 0 then count
           else sequence (s/num) (count+1) in sequence s 0

let print_list lst =
  "[" ^ (String.concat "; " (List.map string_of_int lst)) ^ "]"

let to_string s =
 if s < 2 then "[]"
 else let rec decompose s n lst=
      let prime = nth_prime n in
      let k = nth s n in
      let lst =  lst@[k] in
      let power = pow prime k in
         if power = s then print_list lst
         else decompose (s/power) (n+1) lst in decompose s 0 [];;
