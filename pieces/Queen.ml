open Chessinternals

let queenAttackMap (b: Board.t) (fs: int) (a: int array) : unit = 
  Rook.rookAttackMap b fs a; Bishop.bishopAttackMap b fs a;;

let queenLegalMoves (b: Board.t) (fs: int) (lst: Move.t list) : Move.t list =
  let rec long (map: int list) (co: Color.t) (acc: Move.t list) : Move.t list = 
    match map with
    | [] -> acc
    | h::t -> begin
    match b.repr.(h) with
    | EmptySquare -> long t co (Move(fs, h)::acc)
    | a when Piece.getColor a = co -> acc
    | _ -> Move(fs, h)::acc
    end
  in begin
    let cps = Piece.getColor (b.repr.(fs)) in 
  long Rook.slideRight.(fs) cps (long Rook.slideLeft.(fs) cps (long Rook.slideUp.(fs) cps (long Rook.slideDown.(fs) cps
  (long Bishop.slideTopRight.(fs) cps (long Bishop.slideTopLeft.(fs) cps (long Bishop.slideBottomRight.(fs) cps (long Bishop.slideBottomLeft.(fs) cps lst)))))))
  end;;