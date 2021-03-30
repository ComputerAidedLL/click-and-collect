(* DEFINITION *)

type formula =
  | One
  | Bottom
  | Top
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

type sequent = {hyp: formula list; cons: formula list};;

(* SEQUENT -> JSON *)

let rec formula_to_json =
  function
  | One -> `Assoc ([("type", `String "neutral") ; ("value", `String "one")])
  | Bottom -> `Assoc ([("type", `String "neutral") ; ("value", `String "bottom")])
  | Top -> `Assoc ([("type", `String "neutral") ; ("value", `String "top")])
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

let sequent_to_json sequent = `Assoc [
        ("hyp", `List (List.map formula_to_json sequent.hyp));
        ("cons", `List (List.map formula_to_json sequent.cons))
    ];;


(* JSON -> SEQUENT *)

exception Bad_sequent_json_exception of string;;

let required_field json key =
    let value =
        try Yojson.Basic.Util.member key json
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_sequent_json_exception ("a sequent and a formula (or sub-formula) must be a json object"))
    in
    if value = `Null
    then raise (Bad_sequent_json_exception ("required field '" ^ key ^ "' is missing"))
    else value

let get_json_string json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_sequent_json_exception ("field '" ^ key ^ "' must be a string"))

let get_json_list json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_list value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_sequent_json_exception ("field '" ^ key ^ "' must be a list"))

let rec json_to_formula json =
  let formula_type = get_json_string json "type" in
  match formula_type with
  "neutral" -> ( let neutral_value = get_json_string json "value" in
     match neutral_value with
       "one" -> One
       | "bottom" -> Bottom
       | "top" -> Top
       | "zero" -> Zero
       | _ -> raise (Bad_sequent_json_exception ("unknown neutral value " ^ neutral_value)) )
  | "litteral" -> Litt (get_json_string json "value")
  | "orthogonal" -> Orth (json_to_formula (required_field json "value"))
  | "tensor" -> Tensor ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "par" -> Par ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "with" -> With ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "plus" -> Plus ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "lollipop" -> Lollipop ( json_to_formula (required_field json "value1") , json_to_formula (required_field json "value2"))
  | "ofcourse" -> Ofcourse (json_to_formula (required_field json "value"))
  | "whynot" -> Whynot (json_to_formula (required_field json "value"))
  | _ -> raise (Bad_sequent_json_exception ("unknown formula type " ^ formula_type));;

let json_to_sequent sequent_as_json =
    let hyp_formulas = List.map json_to_formula (get_json_list sequent_as_json "hyp") in
    let cons_formulas = List.map json_to_formula (get_json_list sequent_as_json "cons") in
    {hyp=hyp_formulas; cons=cons_formulas}


(* OPERATIONS *)

let rec orthogonal =
    function
    | One -> Bottom
    | Bottom -> One
    | Top -> Zero
    | Zero -> Top
    | Litt x -> Orth (Litt x)
    | Orth e -> e
    | Tensor (e1, e2) -> Par (orthogonal e1, orthogonal e2)
    | Par (e1, e2) -> Tensor (orthogonal e1, orthogonal e2)
    | With (e1, e2) -> Plus (orthogonal e1, orthogonal e2)
    | Plus (e1, e2) -> With (orthogonal e1, orthogonal e2)
    | Lollipop (e1, e2) -> Tensor (e1, orthogonal e2)
    | Ofcourse e -> Whynot (orthogonal e)
    | Whynot e -> Ofcourse (orthogonal e);;

let rec simplify =
    function
    | One -> One
    | Bottom -> Bottom
    | Top -> Top
    | Zero -> Zero
    | Litt x -> Litt x
    | Orth e -> orthogonal (simplify e)
    | Tensor (e1, e2) -> Tensor (simplify e1, simplify e2)
    | Par (e1, e2) -> Par (simplify e1, simplify e2)
    | With (e1, e2) -> With (simplify e1, simplify e2)
    | Plus (e1, e2) -> Plus (simplify e1, simplify e2)
    | Lollipop (e1, e2) -> Par (orthogonal (simplify e1), simplify e2)
    | Ofcourse e -> Ofcourse (simplify e)
    | Whynot e -> Whynot (simplify e);;

let get_monolatery_sequent sequent =
    {hyp=[]; cons=List.map simplify sequent.cons @ List.map orthogonal (List.map simplify sequent.hyp)};;

let is_whynot = function
    | Whynot e -> true
    | _ -> false;;


(* APPLY RULE *)

exception Apply_rule_technical_exception of string;;
exception Apply_rule_logic_exception of string;;

let rec head_formula_tail_with_int formula_position = function
    | [] -> raise (Apply_rule_technical_exception "Argument formula_positions[0] is greater than the number of given formulas")
    | f :: formula_list -> if formula_position = 0
        then [], f, formula_list
        else let head, formula, tail = head_formula_tail_with_int (formula_position - 1) formula_list
        in f::head, formula, tail;;

let head_formula_tail = function
    | [] -> raise (Apply_rule_technical_exception "Argument formula_positions contains too few integer")
    | formula_position :: [] -> head_formula_tail_with_int formula_position
    | _ -> raise (Apply_rule_technical_exception "Argument formula_positions contains too many integers");;

let rec permute l = function
    | [] -> []
    | n :: tail -> (List.nth l n) :: (permute l tail);;

let is_valid_permutation l =
    let sorted_l = List.sort Int.compare l in
    let identity = List.init (List.length l) (fun n -> n) in
    sorted_l = identity;;

let apply_rule rule sequent formula_positions =
    (* Applying rule on sequent with non empty hypotheses is not implemented yet *)
    if sequent.hyp != [] then raise (Apply_rule_technical_exception ("This API can apply rule only on monolatery sequent for the moment"))

    else match rule with
    "axiom" -> (
        match sequent.cons with
        | e1 :: e2 :: [] -> if orthogonal e1 = e2 then []
            else raise (Apply_rule_logic_exception ("Can not apply 'axiom' rule: the two formulas are not orthogonal."))
        | _ -> raise (Apply_rule_logic_exception ("Can not apply 'axiom' rule: the sequent must contain exactly two formulas."))
    )
    | "one" -> (
        match sequent.cons with
        | One :: [] -> []
        | _ -> raise (Apply_rule_logic_exception ("Can not apply 'one' rule: the sequent must be reduced to the single formula '1'."))
    )
    | "bottom" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        | Bottom -> [{hyp=[]; cons=(head @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "top" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        | Top -> []
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "zero" -> raise (Apply_rule_logic_exception ("Can not apply 'zero' rule: there is no rule for introducing '0'."))
    | "tensor" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Tensor (e1, e2) -> [{hyp=[]; cons=(head @ [e1])}; {hyp=[]; cons=([e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "par" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Par (e1, e2) -> [{hyp=[]; cons=(head @ [e1; e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "with" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        With (e1, e2) -> [{hyp=[]; cons=(head @ [e1] @ tail)}; {hyp=[]; cons=(head @ [e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "plus_left" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Plus (e1, e2) -> [{hyp=[]; cons=(head @ [e1] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "plus_right" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Plus (e1, e2) -> [{hyp=[]; cons=(head @ [e2] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "promotion" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Ofcourse e -> if List.for_all is_whynot head && List.for_all is_whynot tail then [{hyp=[]; cons=(head @ [e] @ tail)}]
            else raise (Apply_rule_logic_exception ("Can not apply 'promotion' rule: the context must contain formulas starting by '?' only."))
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "dereliction" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Whynot e -> [{hyp=[]; cons=(head @ [e] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "weakening" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Whynot e -> [{hyp=[]; cons=(head @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "contraction" -> (
        let head, formula, tail = head_formula_tail formula_positions sequent.cons in
        match formula with
        Whynot e -> [{hyp=[]; cons=(head @ [Whynot e; Whynot e] @ tail)}]
        | _ -> raise (Apply_rule_technical_exception ("Cannot apply rule '" ^ rule ^ "' on this formula"))
    )
    | "exchange" -> (
        if List.length sequent.cons <> List.length formula_positions
        then raise (Apply_rule_technical_exception ("When applying exchange rule, formula_positions and sequent must have same size"))
        else if not (is_valid_permutation formula_positions)
        then raise (Apply_rule_technical_exception ("When applying exchange rule, formula_positions should be a permutation of the size of sequent formula list"))
        else [{hyp=[]; cons=permute sequent.cons formula_positions}]
    )
    | _ -> raise (Apply_rule_technical_exception ("Unknown rule '" ^ rule ^ "'"));;