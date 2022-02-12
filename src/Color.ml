type t = 
  | White
  | Black;;

let opposite (c1 :t) (c2: t) : bool = not (c1 = c2);;