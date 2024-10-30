let lex s =
  let lst = Utils.split s in
   let rec helper lst acc =
    match lst with
    | [] -> Some (List.rev acc)
    | h::t -> 
      let tk = Utils.tok_of_string_opt h in
      match tk with
      | None -> None
      | Some a -> helper t (a::acc)
   in
    helper lst []

