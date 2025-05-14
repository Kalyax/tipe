let xor_row r1 r2 =
  Array.mapi (fun i x -> (x + r2.(i)) mod 2) r1

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let solve_mod2 (a : int array array) (b : int array) : int array option =
  let n = Array.length a in
  let aug = Array.init n (fun i ->
    Array.append a.(i) [|b.(i)|]) in

  for col = 0 to n - 1 do
    (* Find pivot *)
    let pivot = ref (-1) in
    for row = col to n - 1 do
      if aug.(row).(col) = 1 then pivot := row
    done;

    if !pivot = -1 then
      () (* No pivot; might be no solution or infinite *)
    else begin
      swap aug col !pivot;
      for row = 0 to n - 1 do
        if row <> col && aug.(row).(col) = 1 then
          aug.(row) <- xor_row aug.(row) aug.(col)
      done
    end
  done;

  (* Extract solution *)
  let x = Array.make n 0 in
  for i = 0 to n - 1 do
    x.(i) <- aug.(i).(n)
  done;
  Some x


  let () =
  let a = [|
    [|1; 0; 0|];
    [|0; 1; 0|];
    [|0; 0; 1|]
  |] in
  let b = [|1; 0; 1|] in
  match solve_mod2 a b with
  | Some x ->
    Array.iter (Printf.printf "%d ") x;
    print_newline ()
  | None -> print_endline "No solution"