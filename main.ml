open Chessinternals
open Piecesinternals

let testb = {
  Board.turnToPlay = White;
  Board.canWhiteCastleQ = true;
  Board.canBlackCastleQ = true;
  Board.canWhiteCastleK = true;
  Board.canBlackCastleK = true;
  Board.draw50HalfMoves = 0;
  Board.enPassantPawn = -1;
  Board.numberOfMoves = 1;
  Board.repr = Array.make 64 Piece.EmptySquare
};;

(* Board.setFENstring testb "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";; *)
Board.setFENstring testb "2bqkbn1/2pppp2/n7/r1p1P1p1/p2N2B1/5Q2/PPPPKPP1/RNB2r2 w - - 0 2";;

print_int (Board.eval testb);;
print_newline ();;
Board.print_Board testb;;

let print_int_list (l: int list) : unit =
  let rec aux (l: int list) : unit =
  match l with
  | [] -> print_string "]\n"
  | h1::h2::t -> Printf.printf "%d; " h1; aux (h2::t)
  | h1::[] -> Printf.printf "%d]\n" h1
  in print_char '['; aux l;;

let print_int_array (l: int array) : unit =
  let rec aux (l: int array) (i: int) : unit =
  match i with
  | 63 -> Printf.printf "%d]\n" l.(i)
  | _ -> Printf.printf "%d; " l.(i); aux l (i+1)
  in print_char '['; aux l 0;;

let a = 21;;
let attackmap = Array.make 64 0;;
Queen.queenAttackMap testb a attackmap ;;
print_int_array attackmap;;

Board.print_move_list testb (Queen.queenLegalMoves testb a);;