open Chessinternals

let attackMapForColor (b: Board.t) (a: int array) (t: Color.t) : unit = 
  let rec aux (i: int) : unit = 
    if i = 64 then () else begin
      match b.repr.(i) with 
      | Piece.Pawn(c) when c = t -> Pawn.pawnAttackMap b i a; aux (i+1) 
      | Piece.Bishop(c) when c = t -> Bishop.bishopAttackMap b i a; aux (i+1) 
      | Piece.Knight(c) when c = t -> Knight.knightAttackMap b i a; aux (i+1) 
      | Piece.Rook(c) when c = t -> Rook.rookAttackMap b i a; aux (i+1) 
      | Piece.Queen(c) when c = t -> Queen.queenAttackMap b i a; aux (i+1) 
      | Piece.King(c) when c = t -> King.kingAttackMap b i a; aux (i+1) 
      | _ -> aux (i+1)
    end
  in aux 0;;

let genAttackMap (b: Board.t) (a: int array) : unit = attackMapForColor b a (b.turnToPlay);;
let genAttackMapForOpponent (b: Board.t) (a: int array) : unit = attackMapForColor b a (Color.invert (b.turnToPlay));;