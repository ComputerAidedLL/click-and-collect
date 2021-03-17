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

let negative e = Neg e;;

let get_monolatery_sequent (lk1, lk2) = ([], lk2 @ List.map negative lk1);;

let sequent_to_json (lk1, lk2) = `Assoc [
        ("hyp", `List (List.map formula_to_json lk1));
        ("cons", `List (List.map formula_to_json lk2))
    ]
