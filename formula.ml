type lkformula =
  | True
  | False
  | Litt of string
  | Neg of lkformula
  | Conj of lkformula * lkformula
  | Disj of lkformula * lkformula
  | Impl of lkformula * lkformula;;

type llformula =
  | One
  | Top
  | Bottom
  | Zero
  | Litt of string
  | Orth of llformula
  | Tensor of llformula * llformula
  | Par of llformula * llformula
  | With of llformula * llformula
  | Plus of llformula * llformula
  | Lollipop of llformula * llformula
  | Ofcourse of llformula
  | Whynot of llformula;;

type sequent =
  | Lksequent of lkformula list * lkformula list
  | Llsequent of llformula list * llformula list;;

let rec lkformula_to_json =
  function
  | True -> `Assoc ([("type", `String "neutral") ; ("value", `String "true")])
  | False -> `Assoc ([("type", `String "neutral") ; ("value", `String "false")])
  | Litt x -> `Assoc ([("type", `String "litteral") ; ("value", `String x)])
  | Neg e -> `Assoc ([("type", `String "negation") ; ("value", lkformula_to_json e)])
  | Conj (e1, e2) -> `Assoc ([("type", `String "conjunction") ; ("value1", lkformula_to_json e1) ; ("value2", lkformula_to_json e2)])
  | Disj (e1, e2) -> `Assoc ([("type", `String "disjunction") ; ("value1", lkformula_to_json e1) ; ("value2", lkformula_to_json e2)])
  | Impl (e1, e2) -> `Assoc ([("type", `String "implication") ; ("value1", lkformula_to_json e1) ; ("value2", lkformula_to_json e2)]);;

let rec llformula_to_json =
  function
  | One -> `Assoc ([("type", `String "neutral") ; ("value", `String "one")])
  | Top -> `Assoc ([("type", `String "neutral") ; ("value", `String "top")])
  | Bottom -> `Assoc ([("type", `String "neutral") ; ("value", `String "bottom")])
  | Zero -> `Assoc ([("type", `String "neutral") ; ("value", `String "zero")])
  | Litt x -> `Assoc ([("type", `String "litteral") ; ("value", `String x)])
  | Orth e -> `Assoc ([("type", `String "orthogonal") ; ("value", llformula_to_json e)])
  | Tensor (e1, e2) -> `Assoc ([("type", `String "tensor") ; ("value1", llformula_to_json e1) ; ("value2", llformula_to_json e2)])
  | Par (e1, e2) -> `Assoc ([("type", `String "par") ; ("value1", llformula_to_json e1) ; ("value2", llformula_to_json e2)])
  | With (e1, e2) -> `Assoc ([("type", `String "with") ; ("value1", llformula_to_json e1) ; ("value2", llformula_to_json e2)])
  | Plus (e1, e2) -> `Assoc ([("type", `String "plus") ; ("value1", llformula_to_json e1) ; ("value2", llformula_to_json e2)])
  | Lollipop (e1, e2) -> `Assoc ([("type", `String "lollipop") ; ("value1", llformula_to_json e1) ; ("value2", llformula_to_json e2)])
  | Ofcourse e -> `Assoc ([("type", `String "ofcourse") ; ("value", llformula_to_json e)])
  | Whynot e -> `Assoc ([("type", `String "whynot") ; ("value", llformula_to_json e)]);;

let sequent_to_json =
  function
  | Lksequent (lk1, lk2) -> `Assoc [
        ("hyp", `List (List.map lkformula_to_json lk1));
        ("cons", `List (List.map lkformula_to_json lk2))
    ]
  | Llsequent (ll1, ll2) -> `Assoc [
        ("hyp", `List (List.map llformula_to_json ll1));
        ("cons", `List (List.map llformula_to_json ll2))
    ]
