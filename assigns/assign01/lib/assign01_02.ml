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
     else find_prime count (num+1) in find_prime 0 2;;
