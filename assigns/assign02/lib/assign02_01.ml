type piece = 
 | X
 | O

type pos = 
| Piece of piece
| Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
| Top
| Middle
| Bottom

type col_index = 
| Left
| Middle
| Right

type pos_index = row_index * col_index

let get_pos (b:board) ((r,c):pos_index)  : pos =
 match (r,c)with 
  |(Top,Left)-> let((p1,_,_),_,_)=b in p1
  |(Top,Middle)-> let((_,p2,_),_,_)=b in p2
  |(Top,Right)-> let((_,_,p3),_,_)=b in p3
  |(Middle,Left)-> let(_,(p4,_,_),_)=b in p4
  |(Middle,Middle)-> let(_,(_,p5,_),_)=b in p5
  |(Middle,Right)-> let(_,(_,_,p6),_)=b in p6
  |(Bottom,Left)-> let(_,_,(p7,_,_))=b in p7
  |(Bottom,Middle)-> let(_,_,(_,p8,_))=b in p8
  |(Bottom,Right)-> let(_,_,(_,_,p9))=b in p9

let winner (b: board) :bool =
 let ((p1,p2,p3),(p4,p5,p6),(p7,p8,p9)) = b in
 let same_pos (a,b,c) = 
    match (a,b,c) with
    | (Piece X, Piece X, Piece X) -> true
    | (Piece O, Piece O, Piece O) -> true
    | _ -> false
 in
    same_pos(p1,p2,p3)|| same_pos(p4,p5,p6)|| same_pos(p7,p8,p9)
 || same_pos(p1,p4,p7)|| same_pos(p2,p5,p8)|| same_pos(p3,p6,p9)
 || same_pos(p1,p5,p9)|| same_pos(p3,p5,p7)  
