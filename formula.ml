type f =
  | Litt of string
  | Neg of f
  | Conj of f * f
  | Disj of f * f
  | Impl of f * f;;

let rec to_json =
  function
  | Litt x -> `Assoc ([("type", `String "litteral") ; ("value", `String x)])
  | Neg e -> `Assoc ([("type", `String "negation") ; ("value", to_json e)])
  | Conj (e1, e2) -> `Assoc ([("type", `String "conjunction") ; ("value1", to_json e1) ; ("value2", to_json e2)])
  | Disj (e1, e2) -> `Assoc ([("type", `String "disjunction") ; ("value1", to_json e1) ; ("value2", to_json e2)])
  | Impl (e1, e2) -> `Assoc ([("type", `String "implication") ; ("value1", to_json e1) ; ("value2", to_json e2)]);;