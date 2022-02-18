type t = 
  | White
  | Black;;

let invert (c: t) : t = if c = White then Black else White;;
let opposite (c1 :t) (c2: t) : bool = not (c1 = c2);;