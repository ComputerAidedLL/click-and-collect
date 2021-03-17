type formula =
  | One
  | Top
  | Bottom
  | Zero
  | Litt of string
  | Orth of formula
  | Tensor of formula * formula
  | Par of formula * formula
  | With of formula * formula
  | Plus of formula * formula
  | Lollipop of formula * formula
  | Ofcourse of formula
  | Whynot of formula;;

let rec formula_to_json =
  function
  | One -> `Assoc ([("type", `String "neutral") ; ("value", `String "one")])
  | Top -> `Assoc ([("type", `String "neutral") ; ("value", `String "top")])
  | Bottom -> `Assoc ([("type", `String "neutral") ; ("value", `String "bottom")])
  | Zero -> `Assoc ([("type", `String "neutral") ; ("value", `String "zero")])
  | Litt x -> `Assoc ([("type", `String "litteral") ; ("value", `String x)])
  | Orth e -> `Assoc ([("type", `String "orthogonal") ; ("value", formula_to_json e)])
  | Tensor (e1, e2) -> `Assoc ([("type", `String "tensor") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Par (e1, e2) -> `Assoc ([("type", `String "par") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | With (e1, e2) -> `Assoc ([("type", `String "with") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Plus (e1, e2) -> `Assoc ([("type", `String "plus") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Lollipop (e1, e2) -> `Assoc ([("type", `String "lollipop") ; ("value1", formula_to_json e1) ; ("value2", formula_to_json e2)])
  | Ofcourse e -> `Assoc ([("type", `String "ofcourse") ; ("value", formula_to_json e)])
  | Whynot e -> `Assoc ([("type", `String "whynot") ; ("value", formula_to_json e)]);;

let negative e = Orth e;;

let get_monolatery_sequent (ll1, ll2) = ([], ll2 @ List.map negative ll1)

let sequent_to_json (ll1, ll2) = `Assoc [
        ("hyp", `List (List.map formula_to_json ll1));
        ("cons", `List (List.map formula_to_json ll2))
    ]