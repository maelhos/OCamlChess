open Chessinternals

let genKnightMove () : (int list) array =
  let off = [10; 17; 15; -6; -10; -17; -15; 6] in
  let rec genn (offlist: int list ) (i: int) (accOfMoves: int list) : int list = 
    match offlist with
    | [] -> accOfMoves
    | h::t -> begin let trgt = i+h in if 0 <= trgt && trgt <= 63 && Move.distancesq i (i+h) < 6 then 
      genn t i (trgt::accOfMoves)
    else genn t i accOfMoves end 
  in let rec aux (i: int) (a: (int list) array ) : (int list) array =
  if i = 63 then begin
    a.(i) <- genn off i []; a
    end
  else begin
    a.(i) <- genn off i []; aux (i+1) a 
    end
  in aux 0 (Array.make 64 []);;
let knightMap = genKnightMove ();;

let knightAttackMap (b: Board.t) (fs: int) (a: int array) : unit = 
  let rec long (map: int list) (co: Color.t) : unit = 
    match map with
    | [] -> ()
    | h::t -> begin
    match b.repr.(h) with
    | EmptySquare -> (a.(h) <- a.(h)+1; long t co)
    | a when Piece.getColor a = co -> long t co
    | _ -> a.(h) <- a.(h)+1
    end
  in begin
    let cps = Piece.getColor (b.repr.(fs)) in 
    long knightMap.(fs) cps
  end;;

let knightLegalMoves (b: Board.t) (fs: int) : Move.t list = 
  let rec long (map: int list) (co: Color.t) : Move.t list = 
    match map with
    | [] -> []
    | h::t -> begin
      match b.repr.(h) with
      | EmptySquare -> Move(fs, h)::(long t co)
      | a when Piece.getColor a = co -> long t co
      | _ -> Move(fs, h)::(long t co)
    end
  in begin
    let cps = Piece.getColor (b.repr.(fs)) in 
    long knightMap.(fs) cps
  end;;