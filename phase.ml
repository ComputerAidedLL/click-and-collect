open Sequent

(* Phase models: give necessary (not sufficient) condition for provability *)

type fact =
  | Fbot
  | Ftop
  | Fint of int

let rec fsum x y =
match x, y with
| Fint n, Fint m -> Fint (n + m)
| Fbot, _ -> Fbot
| _, Fbot -> Fbot
| Ftop, _ -> Ftop
| _, Ftop -> Ftop

let rec fsup x y =
match x, y with
| Ftop, _ -> Ftop
| _, Ftop -> Ftop
| Fint n, Fint m -> if n = m then Fint n else Ftop
| x, Fbot -> x
| Fbot, x -> x

let rec finf x y =
match x, y with
| Fbot, _ -> Fbot
| _, Fbot -> Fbot
| Fint n, Fint m -> if n = m then Fint n else Fbot
| x, Ftop -> x
| Ftop, x -> x

let fone = Fint 0
let fbot p = Fint p

let fdual p = function
  | Fbot -> Ftop
  | Ftop -> Fbot
  | Fint n -> Fint (p - n)

let fpar p x y = fdual p (fsum (fdual p x) (fdual p y))

let rec semantics p valuation = function
  | One -> fone
  | Bottom -> fbot p
  | Top -> Ftop
  | Zero -> Fbot
  | Litt x -> Fint (valuation x)
  | Dual x -> Fint (p - valuation x)
  | Tensor (f1, f2) -> fsum (semantics p valuation f1) (semantics p valuation f2)
  | Par (f1, f2) -> fpar p (semantics p valuation f1) (semantics p valuation f2)
  | With (f1, f2) -> finf (semantics p valuation f1) (semantics p valuation f2)
  | Plus (f1, f2) -> fsup (semantics p valuation f1) (semantics p valuation f2)
  | Ofcourse f -> finf (semantics p valuation f) fone
  | Whynot f -> fsup (semantics p valuation f) (fbot p)

let valid_semantics p v f =
  match semantics p v f with
  | Ftop -> true
  | Fint n -> n = 0
  | Fbot -> false

let delta_valuation reference atom =
  if atom = reference then 1 else 0

let zero_valuation atom = 0

let valid_sequent sequent =
  let variables = get_unique_variable_names sequent in
  let formula = sequent_to_formula sequent in
  valid_semantics 1 zero_valuation formula &&
  List.for_all (fun x -> valid_semantics 0 (delta_valuation x) formula) variables
