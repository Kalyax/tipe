type variable = string;;

type factor =
  | Indet of variable 
  | F of polynomial
and monomial = Mono of (factor * int) list 
and polynomial = Poly of (monomial * float) list;;

(*4.1x^2y + y + 2*)
let e2 = Poly [ Mono ([(Indet "x", 2); (Indet "y", 1)], 4.1) ; 
                Mono ([Indet "y" 1], 1.0) ; 
                Mono ([], 2.0)
              ];;

let nulla_test (systeme: polynomial list) =
  let n = List.length system in 
  let d = ref 1 in
  let K = 999 in (* borne sup à regarder *)
  while !d <= K do
    let cert = 0 in
    (* trouver moyen de mettre des inconnues en coef des monomes *)
    d := !d + 1
  done;; 

type graph = int list list;;

(* un literal par clause ajoute 3 sommets au graphe *)
(* chaque clause ajoute (nb_noeud - 1)*3 sommets (sans compter les sommets des littéraux) *)
let test (fnc: int list list) =
  
;;

test [[0;1];[1;2]];;
