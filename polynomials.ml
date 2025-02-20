type variable = string;;
type coef = float;;
type exposant = int;;

type factor =
  | Indet of variable 
  | F of polynomial
and power = factor * exposant
and monomial = coef * (power list) (* coef * [factor * exposant] *)
and polynomial = monomial list;;

let null_p () : polynomial = [0.0, []];;
let one_p () : polynomial = [1.0, []];;

let rec print_polynomial (p: polynomial): unit =
  let print_monomial m = 
    print_float (fst m);
    List.iter (fun indet -> 
      match (fst indet) with 
      | Indet c -> Printf.printf "%s^%d" (c) (snd indet)
      | F f -> print_string "("; print_polynomial f; Printf.printf ")^%d" (snd indet)
    ) (snd m)
  in List.iter (fun m  -> print_monomial m; print_string " + ") p;;

let count_monomial (p: polynomial) = List.length p;;

let sum_of_polynomial (p1: polynomial) (p2: polynomial) : polynomial =
  let n_p1 = count_monomial p1 in
  let n_p2 = count_monomial p2 in
  let tbl = Hashtbl.create @@ max n_p1 n_p2 in

  let action_monomial (m: monomial) =
    match Hashtbl.find_opt tbl (snd m) with
    | None -> Hashtbl.add tbl (snd m) (fst m)
    | Some mm -> Hashtbl.replace tbl (snd m) ((fst m) +. mm)
  in List.iter action_monomial p1; List.iter action_monomial p2;
  Hashtbl.fold (fun k v acc -> (v, k)::acc) tbl []
;;

let simplify_polynomial p = sum_of_polynomial p [];;


(*Prends deux monome (coef*power list), et renvoie leur produit*)
let product_of_monomial (m1: monomial) (m2: monomial) : monomial =
  let coef = (fst m1) *. (fst m2) in
  let n_m1 = List.length (snd m1) in
  let n_m2 = List.length (snd m2) in
  let tbl = Hashtbl.create @@ max n_m1 n_m2 in

  let count_indets indet =
    match Hashtbl.find_opt tbl (fst indet) with
    | None -> Hashtbl.add tbl (fst indet) (snd indet)
    | Some power -> Hashtbl.replace tbl (fst indet) (power + (snd indet))
  in List.iter count_indets (snd m1); List.iter count_indets (snd m2);
  let new_indets = Hashtbl.fold (fun k v acc -> (k, v)::acc) tbl [] in
  (coef, new_indets);;

let product_of_polynomial p1 p2 =
  let result = ref [] in
  List.iter (fun m1 -> 
    List.iter (fun m2 ->
      result := (product_of_monomial m1 m2)::(!result)
    ) p2
  ) p1;
  simplify_polynomial !result
;;

let rec power_polynomial p n : polynomial =
  if n = 1 then p
  else product_of_polynomial p (power_polynomial p (n-1))
;;

let develop_factors (p: polynomial) = 
  (*fait le produit de tous les facteurs dans un monome et renvoie un polynome*)
  let action_monomial (mono: monomial) : polynomial = 
    let coef = fst mono in
    (*power = factor * exposant *)
    let powers = snd mono in

    (*faire le produit des powers*)
    let rec produit (ps: power list): polynomial =
      match ps with
      | [] -> one_p ()
      | p::t -> (match p with 
                | (Indet x, e) -> product_of_polynomial [(1.0, [(Indet x,e)])] (produit t)
                | (F f, e) -> product_of_polynomial (power_polynomial f e) (produit t)
                )
    in product_of_polynomial ([(coef, [])]) (produit powers)
  in 
  let to_sum_poly = List.map action_monomial p in
  let head = List.hd to_sum_poly in
  let queue = List.tl to_sum_poly in
  List.fold_left sum_of_polynomial head queue
;;

let rec get_indeterminates (p: polynomial) =
  let tbl = Hashtbl.create (List.length p) in
  let add_indet i = match Hashtbl.find_opt tbl i with
  | None -> Hashtbl.add tbl i 0
  | Some _ -> ()
  in
  List.iter (fun m -> 
    List.iter (fun indet ->
      let indet2 = fst indet in
      match indet2 with
      | Indet s -> add_indet indet2
      | F pp -> begin 
        let indets = get_indeterminates pp in
        List.iter (fun i -> add_indet i) indets
      end
    ) (snd m)
  ) p;
  Hashtbl.fold (fun k v acc -> k::acc) tbl [];;

let rec merge_lists l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> if List.mem h l2 then merge_lists t l2 else merge_lists t (h::l2)

let get_system_indeterminates s =
  let indets = ref [] in
  List.iter (fun p -> indets := merge_lists !indets (get_indeterminates p)) s;
  !indets
;;


let increment_counter (d: int) (counter: int array) n =
  let is_over = if counter.(n-1) = d then false else true in
  let running = ref true in
  let i = ref 0 in
  while !running && !i < n do
    if counter.(!i) < d then begin
      counter.(!i) <- counter.(!i) + 1;
      running := false
    end
    else begin
      counter.(!i) <- 0;
      i := !i + 1;
    end
  done;
  is_over;
;;

let rec array_sum l = 
  let somme = ref 0 in
  Array.iter (fun i -> somme := !somme + i) l;
  !somme;;

let generate_polynomial d indets coef_count : polynomial = 
  let n = List.length indets in
  let counter = Array.make n 0 in
  let monomials = ref [] in
  (*let coef_count = ref 0 in*)
  while increment_counter d counter n do
    if array_sum counter <= d then begin 
      let i = ref (-1) in
      let powers = ref [(Indet ("c"^(string_of_int !coef_count)), 1)] in
      coef_count := !coef_count + 1;
      (*toutes les combinaisons d'indets*)
      List.iter (fun indet -> 
        i := !i + 1;
        if counter.(!i) <> 0 then powers := (indet, counter.(!i))::!powers
      ) indets;
      (*let powers = (Indet "c", 1)::(List.map (fun indet -> i := !i + 1; (indet, counter.(!i))) indets) in*)
      monomials := (1.0, !powers)::!monomials
    end 
  done;
  (*ajout monome constant*)
  monomials := (1.0, [(Indet ("c"^(string_of_int !coef_count)),1)])::!monomials;
  coef_count := !coef_count + 1;
  !monomials
;;

let evaluate (p: polynomial) indet (value: float) : polynomial =
  let rec eval_powers m = match m with
    | [] -> (1.0, [])
    | h::t -> begin
      let hindet = fst h in
      let hpower = float_of_int (snd h) in
      if hindet = indet then begin
        let (c,tt) = eval_powers t in 
        ((Float.pow value hpower)*.c, tt)
      end
      else let (c,tt) = (eval_powers t) in (c, h::tt)
    end
  in
  List.map (fun monos ->
    let coef = fst monos in
    let powers = snd monos in
    let (new_c, new_monos) = eval_powers powers in
    (coef*.new_c, new_monos)
  ) p;;

let rec get_degree (p: polynomial) : int =
  let max_d = ref (-1) in
  List.iter (fun mono -> 
    let powers = snd mono in
    let exposants = List.map (fun (factor, exposant) -> 
      match factor with
      | Indet _ -> exposant
      | F f -> (get_degree f)*exposant
    ) powers in
    let mono_deg = List.fold_left ( + ) 0 exposants in
    if mono_deg > !max_d then max_d := mono_deg
  ) p; 
  !max_d
;;

let factor_polynomial p =
  let tbl = Hashtbl.create (count_monomial p) in
  let rec find_c (m: power list) =
    match m with
    | [] -> (Indet "", [])
    | (f, exp)::ll -> (match f with
      | Indet c -> 
        (if c.[0] = 'c' then (Indet c, ll)
        else let (a,b) = find_c ll in (a, (f,exp)::b))
      | F f -> failwith "impossible")
  in 
  List.iter (fun m -> ) p
;;

(*4x^2y + y + 2*)
let e2 = [ (4.0, [(Indet "x", 2); (Indet "y", 1)]) ; 
           (1.0, [(Indet "y", 1)]) ; 
           (2.0, [])
         ];;

(*e2^2*)
let e3 = [
   (1.0, [(F e2,2)])
  ];;

let f1 = [
  (1.0, [(Indet "x", 2)]);
  (-1.0, [])
];;

let f2 = [
  (1.0, [(Indet "x", 1)]);
  (1.0, [(Indet "y", 1)])
];;

let f3 = [
  (1.0, [(Indet "x", 1)]);
  (1.0, [(Indet "z", 1)])
];;

let f4 = [
  (1.0, [(Indet "y", 1)]);
  (1.0, [(Indet "z", 1)])
];;


let print_array l = Array.iter (fun e -> Printf.printf "%d->" e) l; print_newline ();;

(*let p = generate_polynomial 3 [Indet "x"; Indet "y"];;
print_polynomial p;;
print_newline ();;
let p2 = evaluate p (Indet "x") 2.0;;
print_polynomial p2;;
print_newline ();;*)

(*print_polynomial e2;;
print_newline ();;
print_polynomial @@ product_of_polynomial e2 e3;;*)


let nulla (system: polynomial list) =
  let indeterminates = get_system_indeterminates system in
  let n = List.length indeterminates in
  let d = ref 1 in
  let k = int_of_float @@ float_of_int (max 3 (List.fold_left max (Int.min_int) (List.map get_degree system))) ** float_of_int n in (*Kollar*)
  while !d <= k do
    (*création des beta_i*)
    let coef_count = ref 0 in
    let betas = List.map (fun _ -> generate_polynomial !d indeterminates coef_count) system in
    List.iter (fun p -> print_polynomial p; print_newline ()) betas;

    let rec build_cert system betas =
      (*system et betas sont toujours de la même longueur*)
      match (system, betas) with
      | ([],[]) -> null_p ()
      | (f_i::t1, b_i::t2) -> sum_of_polynomial (product_of_polynomial b_i f_i) (build_cert t1 t2)
      | _ -> failwith "Impossible"
    in
    (*? Pas besoin de le simplifier ?*)
    let cert = develop_factors @@ simplify_polynomial (build_cert system betas) in

    
    print_polynomial cert;
    print_newline ();

    (*factoriser les polynomes pour en extraire un systeme linéaire*)
    


     d := !d + 1
   done;;

nulla [f1;f2;f3;f4];;


type graph = int list list;;

(* un literal par clause ajoute 3 sommets au graphe *)
(* chaque clause ajoute (nb_noeud - 1)*3 sommets (sans compter les sommets des littéraux) *)
let test (fnc: int list list) = 0
  
;;

test [[0;1];[1;2]];;
