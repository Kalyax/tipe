type variable = string;;

type factor =
  | Indet of variable 
  | F of polynomial
and monomial = Mono of float * ((factor * int) list)
and polynomial = Poly of monomial list;;

(*4x^2y + y + 2*)
let e2 = Poly [ Mono (4.0, [(Indet "x", 2); (Indet "y", 1)]) ; 
                Mono (1.0, [Indet "y" 1]) ; 
                Mono (2.0, [])
              ];;

(*e2^2*)
let e3 = Poly [
    Mono (1.0, [(e2,2)])
  ];;

let size poly = List.length poly;;

let somme p1 p2 =
  let t = Hashtbl.create (size p1 + size p2) in
  let iter_poly monome = 
    let indeters = snd monome in
    let coef = fst monome in
    let v = Hashtbl.find_opt t indeters in 
    match v in
    | None -> Hashtbl.add t indeters coef
    | Some c -> Hashtbl.replace t indeters (coef + c)
  in List.iter iter_poly p1; List.iter iter_poly p2;

;;  




(*let nulla_test (systeme: polynomial list) =
   let n = List.length system in 
   let d = ref 1 in
   let K = 999 in (* borne sup à regarder *)
   while !d <= K do
     let cert = 0 in
    (* trouver moyen de mettre des inconnues en coef des monomes *)
     d := !d + 1
   done;; 

 type graph = int list list;;*)

(* un literal par clause ajoute 3 sommets au graphe *)
(* chaque clause ajoute (nb_noeud - 1)*3 sommets (sans compter les sommets des littéraux) *)
let test (fnc: int list list) = 0
  
;;

test [[0;1];[1;2]];;
