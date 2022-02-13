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


let lex_int (s: string) (i: int) : (int*int) =
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
    | _ -> b.enPassantPawn <- Move.pos_of_cc s.[index] s.[index+1]; index+3
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
  ^ string_of_int ((bb / 8) + 1)
  | Promote(a, b) -> string_of_char (char_of_int ((a mod 8) + 0x61))
  ^ string_of_int ((a / 8) + 1) ^ string_of_char (Piece.char_of_piece b);;

let print_move_list (bo: t) (l: Move.t list) : unit =
  let rec aux (l: Move.t list) : unit =
  match l with
  | [] -> print_string "]\n"
  | h1::h2::t -> begin match h1 with
    | Move.Move(a, b) -> begin let x, y = Move.cc_of_pos a in 
    let x1, y1 = Move.cc_of_pos b in 
    Printf.printf "%c%c%c -> %c%c; " (Piece.char_of_piece bo.repr.(a)) x y x1 y1  ; aux (h2::t) end
    | Move.CastleKS(a) -> (if Piece.getColor bo.repr.(a) = Color.Black then print_string "o-o; " else print_string "O-O; " ; aux (h2::t))
    | Move.CastleQS(a) -> (if Piece.getColor bo.repr.(a) = Color.Black then print_string "o-o-o; " else print_string "O-O-O; " ; aux (h2::t))
    | Promote(a, b) -> let x, y = Move.cc_of_pos a in Printf.printf "%c%c = %c; " x y (Piece.char_of_piece b)   ; aux (h2::t)
  end
  | h1::[] -> begin match h1 with
    | Move.Move(a, b) -> begin let x, y = Move.cc_of_pos a in 
    let x1, y1 = Move.cc_of_pos b in 
    Printf.printf "%c%c%c -> %c%c]\n" (Piece.char_of_piece bo.repr.(a)) x y x1 y1 end
    | CastleKS(a) -> if Piece.getColor bo.repr.(a) = Color.Black then print_string "o-o]\n" else print_string "O-O]\n"
    | CastleQS(a) -> if Piece.getColor bo.repr.(a) = Color.Black then print_string "o-o-o]\n" else print_string "O-O-O]\n"
    | Promote(a, b) -> let x, y = Move.cc_of_pos a in Printf.printf "%c%c = %c]\n" x y (Piece.char_of_piece b)
  end
  in print_char '['; aux l;;

