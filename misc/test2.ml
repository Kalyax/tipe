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


    let () =
    let a = [|
      [|1; 1; 1|];
      [|1; 1; 1|]
    |] in
    let b = [|1; 0|] in
    match solve_mod2_general a b with
    | Some x ->
        Printf.printf "Solution: [ %s ]\n"
          (String.concat " " (List.map string_of_int (Array.to_list x)))
    | None ->
        print_endline "No solution"