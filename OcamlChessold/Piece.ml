
type t = 
  | King of Color.t
  | Queen of Color.t
  | Rook of Color.t
  | Bishop of Color.t
  | Knight of Color.t
  | Pawn of Color.t
  | EmptySquare;;

let getColor (p: t) : Color.t =
  match p with
  | EmptySquare -> failwith "blank has no color"
  | King(White) | Queen(White) | Rook(White) | Bishop(White) | Knight(White) | Pawn(White) -> Color.White
  | _ -> Color.Black;;
  
let valueC (p: t) : int =
  match p with 
  | King(_) -> 0 (* not important*)
  | Queen(White) -> 900
  | Queen(Black) -> -900
  | Rook(White) -> 500
  | Rook(Black) -> -500
  | Bishop(White) -> 300
  | Bishop(Black) -> -300
  | Knight(White) -> 300
  | Knight(Black) -> -300
  | Pawn(White) -> 100
  | Pawn(Black) -> -100
  | EmptySquare -> 0;;

let value (p: t) : int = abs (valueC p);;

let piece_of_char (s: char) : t =
  match s with
  | 'K' -> King(White)
  | 'Q' -> Queen(White)
  | 'R' -> Rook(White)
  | 'N' -> Knight(White)
  | 'B' -> Bishop(White)
  | 'P' -> Pawn(White)
  | 'k' -> King(Black)
  | 'q' -> Queen(Black)
  | 'r' -> Rook(Black)
  | 'n' -> Knight(Black)
  | 'b' -> Bishop(Black)
  | 'p' -> Pawn(Black)
  | ' ' -> EmptySquare
  | _ -> failwith "Not a picece ...";;

  let char_of_piece (p: t) : char =
  match p with
  | King(White) -> 'K'
  | Queen(White) -> 'Q'
  | Rook(White) -> 'R'
  | Knight(White) -> 'N'
  | Bishop(White) -> 'B'
  | Pawn(White) -> 'P'
  | King(Black) -> 'k'
  | Queen(Black) -> 'q'
  | Rook(Black) -> 'r'
  | Knight(Black) -> 'n'
  | Bishop(Black) -> 'b'
  | Pawn(Black) -> 'p'
  | EmptySquare -> '_';;
