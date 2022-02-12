type t = {
  mutable turnToPlay : Color.t;

  mutable canWhiteCastleQ : bool;
  mutable canBlackCastleQ : bool;

  mutable canWhiteCastleK : bool;
  mutable canBlackCastleK : bool;

  mutable enPassantPawn : int;      

  mutable draw50HalfMoves : int;

  mutable numberOfMoves : int;
  repr : Piece.t array
};;

let eval (b: t) : int =
  let rec aux (b: t) (i: int) (acc: int) : int =
    if i = 64 then acc else aux b (i+1) (acc+(Piece.valueC b.repr.(i)))
  in aux b 0 0;;


let string_of_char (c: char) : string = String.make 1 c;;


let rec lex_int (s: string) (i: int) : (int*int) =
  let rec aux (s: string) (i: int): string =
  if String.length s > i && s.[i] >= '0' && s.[i] <= '9' then 
    (string_of_char s.[i]) ^ (aux s (i+1))
  else ""
  in let a = aux s i in (int_of_string a, i + String.length a);;


let print_Board (b: t) : unit =
  let rec aux (r: int) : unit=
    match r with
    | 0 -> print_char (Piece.char_of_piece (b.repr.((r/8)*8 + 7 - (r mod 8))))
    | _ -> begin print_char (Piece.char_of_piece (b.repr.((r/8)*8 + 7 - (r mod 8)))); 
      if r mod 8 = 0 then print_newline ();
      aux (r-1);
      end
  in aux 63; print_newline ();; 


let setFENstring (b: t) (s: string) : unit =
  let rec setPieces (b: t) (s: string) (index: int) (file: int) (rank: int) : int = 
    match s.[index] with 
    | ' ' -> index+1
    | '/' -> setPieces b s (index+1) 0 (rank-1)
    | a when '0' <= a && a <= '9' -> setPieces b s (index+1) (file+(int_of_char a - 0x30)) rank
    | a -> begin b.repr.(Move.pos_of_cord file rank) <- Piece.piece_of_char a;
      setPieces b s (index+1) (file+1) rank
      end
  in let setTurn (b: t) (s: string) (index: int) : int =
    match s.[index] with
    | 'w' -> b.turnToPlay <- Color.White ; index+2
    | 'b' -> b.turnToPlay <- Color.Black ; index+2
    | _ -> failwith "FEN string is wrong (TurnToPlay)..."
  in let rec setCastleRights (b: t) (s: string) (index: int) : int =
    match s.[index] with
    | '-' -> index+2
    | ' ' -> index+1
    | 'K' -> b.canWhiteCastleK <- true; setCastleRights b s (index+1)
    | 'Q' -> b.canWhiteCastleQ <- true; setCastleRights b s (index+1)
    | 'k' -> b.canBlackCastleK <- true; setCastleRights b s (index+1)
    | 'q' -> b.canBlackCastleK <- true; setCastleRights b s (index+1)
    | _ -> failwith "FEN string is wrong (castling rights)..."
  in let setenPassant (b: t) (s: string) (index: int) : int =
    match s.[index] with 
    | '-' -> index+2
    | a -> b.enPassantPawn <- Move.pos_of_cc s.[index] s.[index+1]; index+3
  in let setHalfMoves (b: t) (s: string) (index: int) : int =
    let a, c = lex_int s index in (b.draw50HalfMoves <- a; c+1)
  in let setNumberOfMobes (b: t) (s: string) (index: int) : unit =
    let a, _ = lex_int s index in b.numberOfMoves <- a
  in setNumberOfMobes b s (setHalfMoves b s (setenPassant b s (setCastleRights b s (setTurn b s (setPieces b s 0 0 7)))));;



let str_of_Move (m: Move.t) (b: t) : string =
  match m with 
  | CastleKS(_) -> "O-O"
  | CastleQS(_) -> "O-O-O"
  | Move(aa, bb) -> string_of_char (Piece.char_of_piece (b.repr.(aa))) 
  ^ string_of_char (char_of_int ((bb mod 8) + 0x61))
  ^ string_of_int ((bb / 8) + 1);;

let rec print_move_list (l: Move.t list) (b: t) : unit = 
  match l with
  | [] -> ()
  | h::t -> print_string ("- " ^ str_of_Move h b ^ "\n"); print_move_list t b;;

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

let knightAttackMap (b: t) (fs: int) (a: int array) : unit = 
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
    long knightMap.(fs) cps
  end;;


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


let bishopAttackMap (b: t) (fs: int) (a: int array) : unit = 
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

let rookAttackMap (b: t) (fs: int) (a: int array) : unit = 
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
