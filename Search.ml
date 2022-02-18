open Chessinternals
open Piecesinternals

let generteattackMaps (b: Board.t) : unit =
  Attack.attackMapForColor b (b.whiteAttackMap) Color.White; Attack.attackMapForColor b (b.blackAttackMap) Color.Black;;

let legalMovesForColor (b: Board.t) (t: Color.t) : Move.t list =
  let rec aux (i: int) (acc: Move.t list) : Move.t list =
    if i = 64 then acc else begin
      match b.repr.(i) with
      | Piece.Pawn(c) when c = t -> aux (i+1) (Pawn.pawnLegalMoves b i acc)
      | Piece.Knight(c) when c = t  -> aux (i+1) (Knight.knightLegalMoves b i acc)
      | Piece.Bishop(c) when c = t -> aux (i+1) (Bishop.bishopLegalMoves b i acc)
      | Piece.Rook(c) when c = t -> aux (i+1) (Rook.rookLegalMoves b i acc)
      | Piece.Queen(c) when c = t -> aux (i+1) (Queen.queenLegalMoves b i acc)
      | Piece.King(c) when c = t -> aux (i+1) (King.kingLegalMoves b i acc) 
      | _ -> (generteattackMaps b; aux (i+1) acc)
    end
  in aux 0 [];;

  let legalMoves (b: Board.t) : Move.t list = legalMovesForColor b (b.turnToPlay);;