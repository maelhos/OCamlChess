open Chessinternals

let pawnAttackMap (b: Board.t) (fs: int) (a: int array) : unit = 
  let attack (off: int) (co: Color.t) : unit = 
    if (b.repr.(off+fs) = EmptySquare || Piece.getColor (b.repr.(off+fs)) != co) then
    a.(fs+off) <- a.(fs+off)+ 1 else ()
  in begin
    let cps = Piece.getColor (b.repr.(fs)) in 
    if cps = Color.White then begin
      match Move.col_of_pos fs with
      | 0 -> attack 9 cps
      | 7 -> attack 7 cps
      | _ -> (attack 7 cps; attack 9 cps)
    end else begin
      match Move.col_of_pos fs with
      | 0 -> attack (-7) cps
      | 7 -> attack (-9) cps
      | _ -> (attack (-7) cps; attack (-9) cps)
    end
  end;;

let pawnLegalMoves (b: Board.t) (fs: int) (lst: Move.t list) : Move.t list = 
  let pawnSimpleMove (acc: Move.t list)  (tgt: int): Move.t list = 
    if b.repr.(fs+tgt) = EmptySquare then Move(fs,fs+tgt)::acc else acc
  in let pawnOn2ndRank (acc: Move.t list) (tgt: int): Move.t list = 
    if b.repr.(fs+tgt) = EmptySquare then begin
      if b.repr.(fs+2*tgt) = EmptySquare then 
        (b.enPassantPawn <- fs+tgt; Move(fs,fs+tgt)::Move(fs,fs+2*tgt)::acc) (* DONT FORGET TO REMOVE ENPASSANT FLAG AFTER A NORMAL MOVE OCCURES *)
      else
        Move(fs,fs+tgt)::acc
      end
    else
      acc
  in let pawnCapture (acc: Move.t list) (co: Color.t) (tgt: int): Move.t list = (* tgt = 9 -> white right capture; tgt = 7 -> white left capture; tgt = -9 -> black left capture; tgt = -7 -> black right capture *)
    if b.repr.(fs+tgt) != EmptySquare && Piece.getColor (b.repr.(fs+tgt)) != co then Move(fs,fs+tgt)::acc else acc

  in let pawnSimplePromotion (acc: Move.t list) (co: Color.t) (tgt: int) : Move.t list = 
    if b.repr.(fs+tgt) = EmptySquare then Promote(fs+tgt,Piece.Knight(co))::
      Promote(fs+tgt,Piece.Queen(co))::Promote(fs+tgt,Piece.Bishop(co))::Promote(fs+tgt,Piece.Rook(co))::acc else acc

  in let pawnCapturePromotion (acc: Move.t list) (co: Color.t) (tgt: int) : Move.t list = 
    if b.repr.(fs+tgt) != EmptySquare && Piece.getColor (b.repr.(fs+tgt)) != co then Promote(fs+tgt, Piece.Knight(co))::
      Promote(fs+tgt,Piece.Queen(co))::Promote(fs+tgt,Piece.Bishop(co))::Promote(fs+tgt,Piece.Rook(co))::acc else acc
  in begin
    let cps = Piece.getColor (b.repr.(fs)) in 
  if cps = Color.White then begin (* en passant ... *)
    if 0=1 then 
      []
    else begin
      if Move.line_of_pos fs = 1 then 
        pawnOn2ndRank lst 8
      else begin
        if Move.line_of_pos fs = 6 then begin
          if Move.col_of_pos fs = 0 then 
            pawnSimplePromotion (pawnCapturePromotion lst cps 9) cps 8
          else begin
            if Move.col_of_pos fs = 7 then 
              pawnSimplePromotion (pawnCapturePromotion lst cps 7) cps 8
            else 
              pawnSimplePromotion (pawnCapturePromotion (pawnCapturePromotion lst cps 9) cps 7) cps 8
            end
          end
        else begin
          if Move.col_of_pos fs = 0 then 
            pawnSimpleMove (pawnCapture lst cps 9)  8
          else begin
            if Move.col_of_pos fs = 7 then 
              pawnSimpleMove (pawnCapture lst cps 7) 8
            else 
              pawnSimpleMove (pawnCapture (pawnCapture lst cps 9) cps 7) 8
            end
          end
      end
    end
  end
  else begin
    if Move.line_of_pos fs = 7 then 
      pawnOn2ndRank lst (-8)
    else begin
      if Move.line_of_pos fs = 1 then begin
        if Move.col_of_pos fs = 0 then 
          pawnSimplePromotion (pawnCapturePromotion lst cps (-7)) cps (-8)
        else begin
          if Move.col_of_pos fs = 7 then 
            pawnSimplePromotion (pawnCapturePromotion lst cps (-9)) cps (-8)
          else 
            pawnSimplePromotion (pawnCapturePromotion (pawnCapturePromotion lst cps (-9)) cps (-7)) cps (-8)
          end
        end
      else begin
        if Move.col_of_pos fs = 0 then 
          pawnSimpleMove (pawnCapture lst cps (-7)) (-8)
        else begin
          if Move.col_of_pos fs = 7 then 
            pawnSimpleMove (pawnCapture lst cps (-9)) (-8)
          else 
            pawnSimpleMove (pawnCapture (pawnCapture lst cps (-9)) cps (-7)) (-8)
          end
        end
    end
  end
end;;