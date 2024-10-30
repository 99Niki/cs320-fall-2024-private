type dir = 
| North
| South
| East
| West

type path = dir list

let dist (p:path) : float =
    let move (x,y) dirs=
        match dirs with
            | North -> (x,y+1)
            | South -> (x,y-1)
            | East ->  (x+1,y)
            | West ->  (x-1,y)
    in
    let (x,y) = List.fold_left move (0,0) p 
    in
    sqrt (float_of_int (x * x + y * y))
