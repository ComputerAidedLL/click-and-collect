open Sequent

(* Clauses : naive check of classical validity
             could be replace by more efficient SAT checker *)

let rec formula_to_clauses = function
  | One -> []
  | Bottom -> [[]]
  | Top -> formula_to_clauses One
  | Zero -> formula_to_clauses Bottom
  | Litt x -> [[(x, true)]]
  | Dual x -> [[(x, false)]]
  | Tensor (f1, f2) ->
     let c1 = formula_to_clauses f1 in
     let c2 = formula_to_clauses f2 in
     c1 @ c2
  | Par (f1, f2) ->
     let c1 = formula_to_clauses f1 in
     let c2 = formula_to_clauses f2 in
     List.concat_map (fun c -> List.map (List.append c) c2) c1
  | With (f1, f2) -> formula_to_clauses (Tensor (f1, f2))
  | Plus (f1, f2) -> formula_to_clauses (Par (f1, f2))
  | Ofcourse f -> formula_to_clauses f
  | Whynot f -> formula_to_clauses f

let valid_clauses clauses =
  let rec valid_clause = function
    | [] -> false
    | (x, b) :: tail ->
       (List.exists (fun z -> z = (x, not b)) tail ||
        valid_clause tail) 
  in List.for_all valid_clause clauses

let provable_sequent_as_classical sequent =
  valid_clauses (formula_to_clauses (sequent_to_formula sequent))
