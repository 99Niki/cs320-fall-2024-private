
type set_info = {
ind : int -> bool;
mn : int;
mx : int;
}

module ListSet = struct
  type t = int list

  let rec mem x lst =
    match lst with
    | [] -> false
    | h::t -> if x = h then true
              else if x < h then false
              else mem x t

  let empty = []

  let singleton x = [x]

  let card = List.length

  let rec union s1 s2 =
    match (s1, s2) with
    | [], l -> l
    | l, [] -> l
    | h1::t1, h2::t2 ->
        if h1 < h2 then h1 :: union t1 s2
        else if h1 > h2 then h2 :: union s1 t2
        else h1 :: union t1 t2 
end



module FuncSet = struct
  type t = set_info

  let mem x lst = lst.ind x

  let empty =
    {
      ind = (fun _ -> false);
      mn = 1;
      mx = 0;
    }
  
  let singleton x =
    {
      ind = (fun y -> y=x);
      mn =x;
      mx =x;
    }

    let card s =
      if s.ind s.mn then s.mx - s.mn + 1 else 0
  
    let union s1 s2 = {
      ind = (fun x -> s1.ind x || s2.ind x); 
      mn = min s1.mn s2.mn;
      mx = max s1.mx s2.mx;
    }
 end
