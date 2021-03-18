type formula =
  | True
  | False
  | Litt of string
  | Neg of formula
  | Conj of formula * formula
  | Disj of formula * formula
  | Impl of formula * formula;;

let rec formula_to_json =
  function
  | True -> `Assoc ([("type", `String "neutral") ; ("value", `String "true")])
  | False -> `Assoc ([("type", `String "neutral") ; ("value", `String "false")])
  | Litt x -> `Assoc ([("type", `String "litteral") ; ("value", `String x)])
  | Neg e -> `Assoc ([("type", `String "negation") ; ("value", formula_to_json e)])
  | Conj (e1, e2) -> `Assoc ([("type", `String "conjunction") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Disj (e1, e2) -> `Assoc ([("type", `String "disjunction") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Impl (e1, e2) -> `Assoc ([("type", `String "implication") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)]);;

let rec negative =
  function
  | True -> False
  | False -> True
  | Litt x -> Neg (Litt x)
  | Neg e -> e
  | Conj (e1, e2) -> Disj (negative e1, negative e2)
  | Disj (e1, e2) -> Conj (negative e1, negative e2)
  | Impl (e1, e2) -> Conj (e1, negative e2);;

let rec simplify =
  function
  | True -> True
  | False -> False
  | Litt x -> Litt x
  | Neg e -> negative (simplify e)
  | Conj (e1, e2) -> Conj (simplify e1, simplify e2)
  | Disj (e1, e2) -> Disj (simplify e1, simplify e2)
  | Impl (e1, e2) -> Impl (simplify e1, simplify e2);;

let get_monolatery_sequent (lk1, lk2) = ([], List.map simplify lk2 @ List.map negative (List.map simplify lk1));;

let sequent_to_json (lk1, lk2) = `Assoc [
        ("hyp", `List (List.map formula_to_json lk1));
        ("cons", `List (List.map formula_to_json lk2))
    ]
