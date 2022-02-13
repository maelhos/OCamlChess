open Chessinternals

let genSlideMove (side: int) : (int list) array =
  let rec long (off: int) (i: int) (accOfMoves: int list) : int list = 
    if (0 <= (i+off) && (i+off) <= 63 && (not (abs off = 1) || Move.line_of_pos i = Move.line_of_pos (i+off))) then begin
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
  
let slideRight = genSlideMove 1;;
let slideLeft = genSlideMove (-1);;
let slideUp = genSlideMove 8;;
let slideDown = genSlideMove (-8);;

let rookAttackMap (b: Board.t) (fs: int) (a: int array) : unit = 
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
    long slideRight.(fs) cps; long slideLeft.(fs) cps; long slideUp.(fs) cps; long slideDown.(fs) cps
  end;;

let rookLegalMoves (b: Board.t) (fs: int) (lst: Move.t list): Move.t list = 
  let rec long (map: int list) (co: Color.t) (acc: Move.t list): Move.t list = 
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
    long slideRight.(fs) cps (long slideLeft.(fs) cps (long slideUp.(fs) cps (long slideDown.(fs) cps lst)))
  end;;