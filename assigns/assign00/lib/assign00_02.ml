let is_prime n =
  if n < 2 then false
  else if n = 2 then true
  else
    let rec check_divisors d =
      if d * d > n then true
      else if n mod d =0 then false
      else check_divisors(d+1) in check_divisors 2;; 
