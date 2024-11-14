type variable = string;;

type factor =
  | Indet of variable 
and monomial = Mono of (factor * int) list 
and polynomial = Poly of (monomial * float) list;;

(*4.1x^2y + y + 2*)
let e2 = Poly [ Mono ([(Indet "x", 2); (Indet "y", 1)], 4.1) ; 
                Mono ([Indet "y" 1], 1.0) ; 
                Mono ([], 2.0)
              ];;
