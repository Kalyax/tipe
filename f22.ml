type f2 = int;;

let zero : f2 = 0;;
let one : f2 = 1;;

let norme x =
  let r = x mod 2 in
  if r < 0 then r + 2 else r

let ( +& ) (a: f2) (b: f2) : f2 = norme (a + b);;

let ( *& )  (a: f2) (b: f2) : f2 = norme (a * b);;


type variable = string;;
type exposant = int;;

type factor =
  | Indet of variable 
  | F of polynomial
and power = factor * exposant
and monomial = f2 * (power list) (* coef * [factor * exposant] *)
and polynomial = monomial list;;

let null_p () : polynomial = [zero, []];;
let one_p () : polynomial = [one, []];;

let rec print_polynomial p =
  match p with
  | [] -> ()
  | (coef, facteurs)::pp -> begin

    (*Affiche le coef du monome*)
    print_int @@ norme coef;

    (*Affiche chacun des facteur du monome*)
    List.iter (fun (facteur, exp) -> 
      (match facteur with
      | Indet c -> print_string c
      | F ff -> (print_string "("; print_polynomial ff; print_string ")"));
      Printf.printf "^%d" exp
    ) facteurs;

    if pp <> [] then begin
      print_string " + ";
      print_polynomial pp
    end
  end
;;

let count_monomial (p: polynomial) = List.length p;;

let sum_of_polynomial (p1: polynomial) (p2: polynomial) : polynomial =
  let n_p1 = count_monomial p1 in
  let n_p2 = count_monomial p2 in
  let tbl = Hashtbl.create @@ max n_p1 n_p2 in

  let action_monomial (m: monomial) =
    match Hashtbl.find_opt tbl (snd m) with
    | None -> Hashtbl.add tbl (snd m) (fst m)
    | Some mm -> (
      if (fst m) +& mm = zero then Hashtbl.remove tbl (snd m)
      else Hashtbl.replace tbl (snd m) one
    )
  in List.iter action_monomial p1; List.iter action_monomial p2;
  Hashtbl.fold (fun k v acc -> (v, k)::acc) tbl []
;;

let simplify_polynomial p = sum_of_polynomial p [];;

(*Prends deux monome (coef*power list), et renvoie leur produit*)
let product_of_monomial (m1: monomial) (m2: monomial) : monomial =
  let coef = (fst m1) *& (fst m2) in
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
      let mono = product_of_monomial m1 m2 in
      if fst mono <> zero then result := mono::(!result)
    ) p2
  ) p1;
  simplify_polynomial !result
;;

let rec power_polynomial p n : polynomial =
  if n = 1 then p
  else product_of_polynomial p (power_polynomial p (n-1))
;;

(*on obtient un polynome entièrement développé*)
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
                | (Indet x, e) -> product_of_polynomial [(one, [(Indet x,e)])] (produit t)
                | (F f, e) -> product_of_polynomial (power_polynomial f e) (produit t)
                )
    in product_of_polynomial ([(coef, [])]) (produit powers)
  in 
  let to_sum_poly = List.map action_monomial p in
  let head = List.hd to_sum_poly in
  let queue = List.tl to_sum_poly in
  List.fold_left sum_of_polynomial head queue
;;

(*donne la liste des indéterminées présente dans un polynome*)
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
  let monomials = ref [] in (*type polynomial*)
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
      monomials := (one, !powers)::!monomials
    end 
  done;
  (*ajout monome constant*)
  monomials := (one, [(Indet ("c"^(string_of_int !coef_count)),1)])::!monomials;
  coef_count := !coef_count + 1;
  !monomials
;;

(*let evaluate (p: polynomial) indet (value: float) : polynomial =
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
    (coef*&new_c, new_monos)
  ) p;;*)

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

(*à utiliser sur un polynome factorisé dans l'idéal*)
let remove_c_factor (mono: monomial) cc =
  let rec aux factors =
    match factors with
    | [] -> []
    | (factor, exp)::t -> 
      (match factor with
      | Indet c -> if String.get c 0 = 'c' then (cc := (Some (factor, exp)); t) else (factor, exp)::(aux t)
      | _ -> failwith "Erreur")
  in aux (snd mono);;

let factorized_form (p: polynomial) : polynomial list =
  let htbl : (power list, polynomial) Hashtbl.t = Hashtbl.create 999 in
  List.iter (
    fun monomial -> 
      let c = ref None in
      let clean_monomial = remove_c_factor monomial c in
      assert (!c != None);

      let factor = Option.get !c in
      match Hashtbl.find_opt htbl clean_monomial with
      | None -> Hashtbl.add htbl clean_monomial [(one,[factor])]
      | Some l -> Hashtbl.replace htbl clean_monomial ((one,[factor])::l)
  ) p;
  let one = Hashtbl.find_opt htbl [] in
  match one with
  | None -> failwith "Erreur ??????"
  | Some o -> (Hashtbl.remove htbl []; o::(List.of_seq (Hashtbl.to_seq_values htbl)))
;;

  (*a appliquer sur les polynome en c_...*)
let matrix_from_system (system: polynomial list) : int array array =
  let indeterminates = get_system_indeterminates system in
  let n = List.length indeterminates in
  let m = List.length system in
  let matrix = Array.make_matrix m n 0 in

  List.iteri (fun i p ->
    List.iter (fun (coef, powers) ->
      List.iter (fun (indet, exp) ->
        match indet with
        | Indet s -> 
          let j = int_of_string (String.sub s 1 (String.length s - 1)) in
          matrix.(i).(j) <- 1
        | _ -> ()
      ) powers
    ) p
  ) system;
  matrix
;;

let print_matrix (matrix: int array array) =
  Array.iter (fun row ->
    Array.iter (fun elem ->
      Printf.printf "%d-" elem
    ) row;
    print_newline ()
  ) matrix;
;;

let xor_row r1 r2 =
  Array.mapi (fun i x -> (x + r2.(i)) mod 2) r1

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let solve_mod2_general (a : int array array) (b : int array) : int array option =
  let m = Array.length a in
  let n = Array.length a.(0) in
  let aug = Array.init m (fun i -> Array.append (Array.copy a.(i)) [|b.(i)|]) in
  let pivot_cols = Array.make n (-1) in
  let row = ref 0 in

  for col = 0 to n - 1 do
    (* Find pivot *)
    let pivot = ref (-1) in
    for r = !row to m - 1 do
      if aug.(r).(col) = 1 then pivot := r
    done;
    if !pivot <> -1 then begin
      swap aug !row !pivot;
      pivot_cols.(col) <- !row;
      (* Eliminate below and above *)
      for r = 0 to m - 1 do
        if r <> !row && aug.(r).(col) = 1 then
          aug.(r) <- xor_row aug.(r) aug.(!row)
      done;
      incr row
    end
  done;

  (* Check for inconsistency *)
  let inconsistent = Array.exists (fun r ->
    let zero_row = Array.sub r 0 n in
    Array.for_all ((=) 0) zero_row && r.(n) = 1
  ) aug in

  if inconsistent then None
  else
    (* Back-substitute (construct particular solution) *)
    let x = Array.make n 0 in
    for col = 0 to n - 1 do
      let row_idx = pivot_cols.(col) in
      if row_idx <> -1 then
        x.(col) <- aug.(row_idx).(n)
    done;
    Some x
;;

(*4x^2y + y + 2*)
let e2 = [ (one, [(Indet "x", 2); (Indet "y", 1)]) ; 
           (one, [(Indet "y", 1)]) ; 
           (one, [])
         ];;

(*e2^2*)
let e3 = [
   (one, [(F e2,2)])
  ];;

let f1 = [
  (one, [(Indet "x", 2)]);
  (one, [])
];;

let f2 = [
  (one, [(Indet "x", 1)]);
  (one, [(Indet "y", 1)])
];;

let f3 = [
  (one, [(Indet "x", 1)]);
  (one, [(Indet "z", 1)])
];;

let f4 = [
  (one, [(Indet "y", 1)]);
  (one, [(Indet "z", 1)])
];;

let print_array l = Array.iter (fun e -> Printf.printf "%d->" e) l; print_newline ();;

(*let exp = ref 0;;
let p = generate_polynomial 3 [Indet "x"; Indet "y"] exp;;
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
  print_int k;
  print_newline ();
  while !d <= k do
    Printf.printf "%d-" !d;
    (*création des beta_i*)
    let coef_count = ref 0 in
    let betas = List.map (fun _ -> generate_polynomial !d indeterminates coef_count) system in
    (*List.iter (fun p -> print_polynomial p; print_newline ()) betas;*)

    let rec build_cert system betas =
      (*system et betas sont toujours de la même longueur*)
      match (system, betas) with
      | ([],[]) -> null_p ()
      | (f_i::t1, b_i::t2) -> sum_of_polynomial (product_of_polynomial b_i f_i) (build_cert t1 t2)
      | _ -> failwith "Impossible"
    in
    (*? Pas besoin de le simplifier ?*)
    let cert = develop_factors @@ simplify_polynomial (build_cert system betas) in

    let eqs = factorized_form cert in
    
    (*print_polynomial cert;
    print_newline ();
    print_newline ();
    print_polynomial @@ Option.get spe;
    print_newline ();
    print_newline ();

    List.iter (fun p -> 
      print_polynomial p;
      print_newline ()) eqs;*)

    (*factoriser les polynomes pour en extraire un systeme linéaire*)
    let matrix = matrix_from_system eqs in

    (*print_matrix matrix;*)
    let b = Array.make (Array.length matrix) 0 in
    b.(0) <- 1;


    let t = solve_mod2_general matrix b in

    match t with
    | Some x ->
        Printf.printf "Solution: [ %s ]\n"
          (String.concat " " (List.map string_of_int (Array.to_list x))); failwith "Système possède une solution"
    | None ->
        print_endline "Pas de solution";


    d := !d + 1
   done;;















type graph = int array array;;
(*matrice d'adjacence*)

let create_graph n = Array.make_matrix n n 0;;

let set_edge g i j =
  g.(i).(j) <- 1;
  g.(j).(i) <- 1
;;
let remove_edge g i j =
  g.(i).(j) <- 0;
  g.(j).(i) <- 0
;;
let is_edge g i j =
  g.(i).(j) = 1 ;;
;;

let graph_to_polynomial g =
  let s = Array.length g in
  let p = ref [] in
  p := [(one, [(Indet ("x1"), 3)]); (one, [])]::!p;
  for i = 0 to s - 1 do 
    for j = i to s - 1 do
      if is_edge g i j then begin
        p := ([
          (one, [(Indet ("x"^(string_of_int i)), 2)]);
          (one, [(Indet ("x"^(string_of_int i)), 1); (Indet ("x"^(string_of_int j)), 1)]);
          (one, [(Indet ("x"^(string_of_int j)), 2)]);
        ])::!p
      end
    done
  done;

  !p
;;

let g = create_graph 3;;
set_edge g 0 1;
set_edge g 0 2;
set_edge g 1 2;

let ps = graph_to_polynomial g in

nulla ps;;

(*let k = create_graph 4 in
set_edge k 0 1;
set_edge k 0 2;
set_edge k 0 3;
set_edge k 1 2;
set_edge k 1 3;
set_edge k 2 3;

let ks = graph_to_polynomial k in

nulla ks;;*)

(*let k = create_graph 12 in
set_edge k 0 1; 
set_edge k 1 2; 
set_edge k 2 7; 
set_edge k 2 9; 
set_edge k 0 3; 
set_edge k 3 10; 
set_edge k 3 8; 
set_edge k 0 11; 
set_edge k 0 5; 
set_edge k 1 4; 
set_edge k 1 6; 
set_edge k 4 5; 
set_edge k 5 6;
set_edge k 6 7; 
set_edge k 7 8; 
set_edge k 8 9; 
set_edge k 9 10; 
set_edge k 10 11; 
set_edge k 11 4; 
set_edge k 4 8; 
set_edge k 5 9; 
set_edge k 6 10; 
set_edge k 7 11; 

let ks = graph_to_polynomial k in
nulla ks;;*)