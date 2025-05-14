let print_matrix matrix =
  Array.iter (fun row ->
    Array.iter (fun elem ->
      Printf.printf "%d " elem
    ) row;
    print_newline ()
  ) matrix

(* Example usage *)
let () =
  let matrix = [| [|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|] |] in
  print_matrix matrix


let m = Array.make_matrix 3 3 0;;
print_matrix m;;