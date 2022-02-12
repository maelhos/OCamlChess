open Chessinternals

let genDiagMove (side: int) : (int list) array =
  let rec long (off: int) (i: int) (accOfMoves: int list) : int list = 
    if (0 <= (i+off) && (i+off) <= 63 && (Move.distancesq i (i+off) < 3)) then begin
     long off (i+off) ((i+off)::accOfMoves)
     end
    else accOfMoves
  in let rec aux (i: int) (a: (int list) array ) : (int list) array =
  if i = 63 then begin
    a.(i) <- List.rev (long side i []); a
    end
  else begin
    a.(i) <- List.rev (long side i []); aux (i+1) a 
    end
  in aux 0 (Array.make 64 []);;

let slideTopRight = genDiagMove 9;;
let slideTopLeft = genDiagMove 7;;
let slideBottomRight = genDiagMove (-7);;
let slideBottomLeft = genDiagMove (-9);;

let bishopAttackMap (b: Board.t) (fs: int) (a: int array) : unit = 
  let rec long (map: int list) (co: Color.t) : unit = 
    match map with
    | [] -> ()
    | h::t -> begin
    match b.repr.(h) with
    | EmptySquare -> (a.(h) <- a.(h)+1; long t co)
    | a when Piece.getColor a = co -> ()
    | _ -> a.(h) <- a.(h)+1
    end
  in begin
    let cps = Piece.getColor (b.repr.(fs)) in 
    long slideTopRight.(fs) cps; long slideTopLeft.(fs) cps; long slideBottomRight.(fs) cps; long slideBottomLeft.(fs) cps
  end;;

let bishopLegalMoves (b: Board.t) (fs: int) : Move.t list = 
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
  long slideTopRight.(fs) cps (long slideTopLeft.(fs) cps (long slideBottomRight.(fs) cps (long slideBottomLeft.(fs) cps [])))
  end;;