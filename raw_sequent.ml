open Sequent

(* DEFINITION *)

type raw_formula =
  | One
  | Bottom
  | Top
  | Zero
  | Litt of string
  | Orth of raw_formula
  | Tensor of raw_formula * raw_formula
  | Par of raw_formula * raw_formula
  | With of raw_formula * raw_formula
  | Plus of raw_formula * raw_formula
  | Lollipop of raw_formula * raw_formula
  | Ofcourse of raw_formula
  | Whynot of raw_formula;;

type raw_sequent = {hyp: raw_formula list; cons: raw_formula list};;


(* RAW_SEQUENT -> SEQUENT *)

let rec to_formula raw_formula = match raw_formula with
    | One -> Sequent.One
    | Bottom -> Sequent.Bottom
    | Top -> Sequent.Top
    | Zero -> Sequent.Zero
    | Litt x -> Sequent.Litt x
    | Orth e -> orthogonal (to_formula e)
    | Tensor (e1, e2) -> Sequent.Tensor (to_formula e1, to_formula e2)
    | Par (e1, e2) -> Sequent.Par (to_formula e1, to_formula e2)
    | With (e1, e2) -> Sequent.With (to_formula e1, to_formula e2)
    | Plus (e1, e2) -> Sequent.Plus (to_formula e1, to_formula e2)
    | Lollipop (e1, e2) -> Sequent.Par (to_formula (Orth e1), to_formula e2)
    | Ofcourse e -> Sequent.Ofcourse (to_formula e)
    | Whynot e -> Sequent.Whynot (to_formula e);;

let to_sequent raw_sequent =
    List.map to_formula raw_sequent.cons @ List.map orthogonal (List.map to_formula raw_sequent.hyp);;


(* SEQUENT -> RAW_SEQUENT *)

let rec to_raw_formula =
    function
    | Sequent.One -> One
    | Sequent.Bottom -> Bottom
    | Sequent.Top -> Top
    | Sequent.Zero -> Zero
    | Sequent.Litt x -> Litt x
    | Sequent.Orth x -> Orth (Litt x)
    | Sequent.Tensor (e1, e2) -> Tensor (to_raw_formula e1, to_raw_formula e2)
    | Sequent.Par (e1, e2) -> Par (to_raw_formula e1, to_raw_formula e2)
    | Sequent.With (e1, e2) -> With (to_raw_formula e1, to_raw_formula e2)
    | Sequent.Plus (e1, e2) -> Plus (to_raw_formula e1, to_raw_formula e2)
    | Sequent.Ofcourse e -> Ofcourse (to_raw_formula e)
    | Sequent.Whynot e -> Whynot (to_raw_formula e);;

let to_raw_sequent sequent =
    {hyp=[]; cons=List.map to_raw_formula sequent};;

(* RAW_SEQUENT -> JSON *)

let rec raw_formula_to_json =
  function
  | One -> `Assoc ([("type", `String "neutral") ; ("value", `String "one")])
  | Bottom -> `Assoc ([("type", `String "neutral") ; ("value", `String "bottom")])
  | Top -> `Assoc ([("type", `String "neutral") ; ("value", `String "top")])
  | Zero -> `Assoc ([("type", `String "neutral") ; ("value", `String "zero")])
  | Litt x -> `Assoc ([("type", `String "litteral") ; ("value", `String x)])
  | Orth e -> `Assoc ([("type", `String "orthogonal") ; ("value", raw_formula_to_json e)])
  | Tensor (e1, e2) -> `Assoc ([("type", `String "tensor") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Par (e1, e2) -> `Assoc ([("type", `String "par") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | With (e1, e2) -> `Assoc ([("type", `String "with") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Plus (e1, e2) -> `Assoc ([("type", `String "plus") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Lollipop (e1, e2) -> `Assoc ([("type", `String "lollipop") ; ("value1", raw_formula_to_json e1) ; ("value2", raw_formula_to_json e2)])
  | Ofcourse e -> `Assoc ([("type", `String "ofcourse") ; ("value", raw_formula_to_json e)])
  | Whynot e -> `Assoc ([("type", `String "whynot") ; ("value", raw_formula_to_json e)]);;

let to_json sequent = `Assoc [
        ("hyp", `List (List.map raw_formula_to_json sequent.hyp));
        ("cons", `List (List.map raw_formula_to_json sequent.cons))
    ];;


(* JSON -> RAW_SEQUENT *)

exception Json_exception of string;;

let required_field json key =
    let value =
        try Yojson.Basic.Util.member key json
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("a sequent and a formula (or sub-formula) must be a json object"))
    in
    if value = `Null
    then raise (Json_exception ("required field '" ^ key ^ "' is missing"))
    else value

let get_json_string json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ key ^ "' must be a string"))

let get_json_list json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_list value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ key ^ "' must be a list"))

let rec json_to_raw_formula json =
  let formula_type = get_json_string json "type" in
  match formula_type with
  "neutral" -> ( let neutral_value = get_json_string json "value" in
     match neutral_value with
       "one" -> One
       | "bottom" -> Bottom
       | "top" -> Top
       | "zero" -> Zero
       | _ -> raise (Json_exception ("unknown neutral value " ^ neutral_value)) )
  | "litteral" -> Litt (get_json_string json "value")
  | "orthogonal" -> Orth (json_to_raw_formula (required_field json "value"))
  | "tensor" -> Tensor ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "par" -> Par ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "with" -> With ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "plus" -> Plus ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "lollipop" -> Lollipop ( json_to_raw_formula (required_field json "value1") , json_to_raw_formula (required_field json "value2"))
  | "ofcourse" -> Ofcourse (json_to_raw_formula (required_field json "value"))
  | "whynot" -> Whynot (json_to_raw_formula (required_field json "value"))
  | _ -> raise (Json_exception ("unknown formula type " ^ formula_type));;

let from_json sequent_as_json =
    let hyp_formulas = List.map json_to_raw_formula (get_json_list sequent_as_json "hyp") in
    let cons_formulas = List.map json_to_raw_formula (get_json_list sequent_as_json "cons") in
    {hyp=hyp_formulas; cons=cons_formulas}


(* SEQUENT <-> JSON *)

let sequent_from_json raw_sequent_as_json =
    to_sequent (from_json raw_sequent_as_json);;

let sequent_to_json sequent =
     to_json (to_raw_sequent sequent);;