
open Graphics

(* échange les cases d'indices i et j dans t *)
let swap (t : 'a array) (i : int) (j : int) : unit =
  let x = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- x;;

(* permute aléatoirement les éléments de t *)
let melange (t : 'a array) : unit =
  let n = Array.length t in
  for i = 0 to n - 1 do
    (* i <= j < n *)
    let j = Random.int (n - i) + i in
    swap t i j
  done;;

(* Représentation d'un labyrinthe:

   Notre labyrinthe est une grille rectangulaire comportant des pièces, séparée
   par des murs. Ces pièces sont repérées par des coordonnées entières (i, j)
   avec i dans [0, p-1] et j dans [0, q-1] pour des valeurs p et q dépendant du
   labyrinthe. Chaque mur est repéré comme un couple de couple ((i1, j1), (i2,
   j2)) donnant les coordonnées (i1, j1) et (i2, j2) des pièces qu'il sépare.
   Le labyrinthe est représenté comme une matrice t telle que pour tout (i, j),
   t.(i).(j) est un tableau donnant les pièces voisines de (i, j) qui ne sont
   pas séparées de (i, j) par un mur. Ces pièces peuvent être données dans un
   ordre quelconque. *)

(* Création du labyrinthe:

   Pour créer un labyrinthe, on va utiliser un algorithme de type union-find
   pour trouver quelles cases sont accessibles à partir de quelles
   cases (https://fr.wikipedia.org/wiki/Union-Find).

   Nous étudierons ce problème en deuxième année, donc je n'en dis pas plus. *)

(* détermine le représentant canonique de (i, j) dans la structure d'union-find
   parent. *)
let find (i : int) (j : int) (parent : (int * int) array array) : int * int =
  let k = ref i and l = ref j in
  (* on cherche le représentant *)
  while parent.(!k).(!l) <> (!k, !l) do
    let (x, y) = parent.(!k).(!l) in
    k := x;
    l := y;
  done;
  (* on en profite pour compresser les chemins *)
  let repr = (!k, !l) in
  k := i;
  l := j;
  while parent.(!k).(!l) <> repr do
    let (x, y) = parent.(!k).(!l) in
    parent.(!k).(!l) <- repr;
    k := x;
    l := y;
  done;
  repr;;

(* fusionne les composantes de (i1, j1) et (i2, j2), que l'on suppose être des
   représentants canoniques dans la structure parent.
   rang est une matrice donnant un rang pour chacun des représentants canoniques
   *)
let union (i1 : int) (j1 : int) (i2 : int) (j2 : int)
      (parent : (int * int) array array) (rang : int array array) : unit =
  (* on fait pointer l'élément de plus petit rang vers celui de plus grand rang,
     avec mise à jour du rang en cas d'égalité *)
  let r1 = rang.(i1).(j1) and r2 = rang.(i2).(j2) in
  if r1 < r2 then parent.(i1).(j1) <- (i2, j2)
  else if r2 < r1 then parent.(i2).(j2) <- (i1, j1)
  else begin
      parent.(i1).(j1) <- (i2, j2);
      rang.(i2).(j2) <- rang.(i2).(j2) + 1
    end;;

(* construit un labyrinthe de taille p * q tel que toutes les cases sont reliées
   et qu'il n'y a qu'un seul chemin d'une case à une autre *)
let construit_labyrinthe (p : int) (q : int) : (int * int) list array array =
  (* initialement, les pièces sont toutes séparées *)
  let t = Array.make_matrix p q [] in
  (* on fait la liste des murs que l'on peut casser *)
  let murs = ref [] in
  for i = 0 to p - 1 do
    for j = 0 to q - 1 do
      if i < p - 1 then
        murs := ((i, j), (i+1, j)) :: !murs;
      if j < q - 1 then
        murs := ((i, j), (i, j+1)) :: !murs
    done
  done;
  (* on va supprimer des murs au hasard jusqu'à obtenir une unique composante
     connexe (cf le cours sur les graphes, plus tard) *)
  let murs = Array.of_list !murs in
  melange murs;
  (* matrice des parents *)
  let parent = Array.make_matrix p q (0, 0) in
  for i = 0 to p - 1 do
    for j = 0 to q - 1 do
      parent.(i).(j) <- (i, j)
    done
  done;
  (* matrice des rangs *)
  let rang = Array.make_matrix p q 0 in
  (* nombre initial de composantes connexes *)
  let n = ref (p * q) in
  (* suppression des murs *)
  let i = ref 0 in
  while !n > 1 do
    let ((i1, j1), (i2, j2)) = murs.(!i) in
    (* on récupère les représentants canoniques *)
    let (x1, y1) = find i1 j1 parent in
    let (x2, y2) = find i2 j2 parent in
    (* si les salles ne sont pas dans la même composante connexe, on les
       fusionne en brisant le mur *)
    if (x1, y1) <> (x2, y2) then begin
        union x1 y1 x2 y2 parent rang;
        t.(i1).(j1) <- (i2, j2) :: t.(i1).(j1);
        t.(i2).(j2) <- (i1, j1) :: t.(i2).(j2);
        decr n
      end;
    incr i
  done;
  t;;

(* trace un mur d'extrémités (x, y) et (x + dx) (y + dy)

   si ouvert vaut true, une porte est ouverte au milieu du mur *)
let mur (x : int) (y : int) (dx : int) (dy : int) (ouvert : bool) : unit =
  if ouvert then
    draw_segments [|(x, y, x + dx / 3, y + dy / 3);
      (x + 2 * dx / 3, y + 2 * dy / 3, x + dx, y + dy)|]
  else draw_segments [|(x, y, x + dx, y + dy)|];;

(* dessine le labyrinthe représenté par t *)
let dessine_labyrinthe (t : (int * int) list array array) : unit =
  open_graph "";
  let sx = size_x () and sy = size_y () in
  let p = Array.length t and q = Array.length t.(0) in
  let dx = sx / (p + 2) and dy = sy / (q + 2) in
  let directions = [(0, 1); (1, 0); (0, -1); (-1, 0)] in
  for i1 = 0 to p - 1 do
    for j1 = 0 to q - 1 do
      List.iter
        (fun (x, y) ->
          let i2 = i1 + x and j2 = j1 + y in
          let o = List.mem (i2, j2) t.(i1).(j1) || (i2, j2) = (-1, 0) ||
                    (i2, j2) = (p, q - 1) in
          mur (dx * (1 + i1 + max x 0)) (dy * (1 + j1 + max y 0)) (dx * abs y)
            (dy * abs x) o)
        directions
    done
  done;;

let trace_segment (p : int) (q : int) (i1 : int) (j1 : int) (i2 : int)
      (j2 : int) : unit =
  set_color red;
  let dx = size_x () / (p + 2) and dy = size_y () / (q + 2) in
  draw_segments [|(dx * (1 + i1) + dx / 2, dy * (1 + j1) + dy / 2,
                   dx * (1 + i2) + dx / 2, dy * (1 + j2) + dy / 2)|];;
let p = 10 and q = 20 in
let t = construit_labyrinthe p q in
dessine_labyrinthe t;;
