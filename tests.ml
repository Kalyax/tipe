type variable = string;;

type factor =
  | Indet of variable 
  | F of polynomial
and power = factor * int
and monomial = float * (power list)
and polynomial = monomial list;;

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

(*let develop_factors (p: polynomial) = 
  let action_monomial m = 

  List.iter () p
;;*)

let get_indeterminates (p: polynomial) =
  let tbl = Hashtbl.create (List.length p) in
  List.iter (fun m -> 
    List.iter (fun indet ->
      let indet2 = fst indet in
      match Hashtbl.find_opt tbl indet2 with
      | None -> Hashtbl.add tbl indet2 0
      | Some i -> ()
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

let generate_polynomial d indets : polynomial = 
  let n = List.length indets in
  let counter = Array.make n 0 in
  let monomials = ref [] in
  let coef_count = ref 0 in
  while increment_counter d counter n do
    if array_sum counter <= d then begin 
      let i = ref (-1) in
      let powers = ref [(Indet ("c"^(string_of_int !coef_count)), 1)] in
      coef_count := !coef_count + 1;
      List.iter (fun indet -> 
        i := !i + 1;
        if counter.(!i) <> 0 then powers := (indet, counter.(!i))::!powers
      ) indets;
      (*let powers = (Indet "c", 1)::(List.map (fun indet -> i := !i + 1; (indet, counter.(!i))) indets) in*)
      monomials := (1.0, !powers)::!monomials
    end 
  done;
  !monomials
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

let print_array l = Array.iter (fun e -> Printf.printf "%d->" e) l; print_newline ();;

print_polynomial @@ generate_polynomial 3 [Indet "x"; Indet "y"];;
print_newline ();;

print_polynomial e2;;
print_newline ();;
print_polynomial @@ product_of_polynomial e2 e3;;

let nulla (system: polynomial list) =
   let n = List.length system in 
   let d = ref 1 in
   let k = 999 in (* borne sup à regarder *)
   while !d <= k do
    (*création des beta_i*)
    let indeterminates = get_system_indeterminates system in
    let betas = List.map (fun _ -> generate_polynomial !d indeterminates) system in

    let cert = 0 in
    (* trouver moyen de mettre des inconnues en coef des monomes *)
     d := !d + 1
   done;;


type graph = int list list;;

(* un literal par clause ajoute 3 sommets au graphe *)
(* chaque clause ajoute (nb_noeud - 1)*3 sommets (sans compter les sommets des littéraux) *)
let test (fnc: int list list) = 0
  
;;

test [[0;1];[1;2]];;
