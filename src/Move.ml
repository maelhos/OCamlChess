type t = 
  | Move of int*int
  | CastleQS of int (* int is pos of the king *)
  | CastleKS of int;;

let pos_of_cord (f: int) (r: int) : int = f + 8*r;;
let cord_of_pos (p: int) : (int*int) = (p mod 8, p / 8);;

let col_of_pos (p: int) : int = fst (cord_of_pos p);;
let line_of_pos (p: int) : int = snd (cord_of_pos p);;

let distancesq (a: int) (b: int) = 
  let x1, y1 = cord_of_pos a in 
  let x2, y2 = cord_of_pos b in 
  (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2);;
  
let pos_of_cc (c: char) (v: char) : int = pos_of_cord (int_of_char c - 0x61) (int_of_char v - 0x31);;
let cc_of_pos (i: int) : char*char = (char_of_int(0x61 + col_of_pos i),char_of_int(0x31 +line_of_pos i));;
