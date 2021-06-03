open Sequent

type rule_request =
    | Axiom
    | One
    | Bottom of int
    | Top of int
    | Zero
    | Tensor of int
    | Par of int
    | With of int
    | Plus_left of int
    | Plus_right of int
    | Promotion of int
    | Dereliction of int
    | Weakening of int
    | Contraction of int
    | Exchange of int list * int list
    | Cut of formula * int
    | Unfold_litt of int
    | Unfold_dual of int;;


(* JSON -> RULE *)

exception Json_exception of string;;

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("rule_request must be a json object"))
    in
    if value = `Null
    then raise (Json_exception ("required field '" ^ k ^ "' is missing"))
    else value;;

let get_string d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ k ^ "' must be a string"));;

let get_int d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_int value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ k ^ "' must be an int"));;

let get_int_list d k =
    let value = get_key d k in
    try List.map Yojson.Basic.Util.to_int (Yojson.Basic.Util.to_list value)
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Json_exception ("field '" ^ k ^ "' must be a list of int"));;

let get_formula d k =
    let value = get_key d k in
    try Raw_sequent.formula_from_json value
    with Raw_sequent.Json_exception m -> raise (Json_exception ("field '" ^ k ^ "' must contain a valid formula: " ^ m));;

let from_json rule_request_as_json =
    let rule = get_string rule_request_as_json "rule" in
    match rule with
        | "axiom" -> Axiom
        | "one" -> One
        | "bottom" -> Bottom (get_int rule_request_as_json "formulaPosition")
        | "top" -> Top (get_int rule_request_as_json "formulaPosition")
        | "zero" -> Zero
        | "tensor" -> Tensor (get_int rule_request_as_json "formulaPosition")
        | "par" -> Par (get_int rule_request_as_json "formulaPosition")
        | "with" -> With (get_int rule_request_as_json "formulaPosition")
        | "plus_left" -> Plus_left (get_int rule_request_as_json "formulaPosition")
        | "plus_right" -> Plus_right (get_int rule_request_as_json "formulaPosition")
        | "promotion" -> Promotion (get_int rule_request_as_json "formulaPosition")
        | "dereliction" -> Dereliction (get_int rule_request_as_json "formulaPosition")
        | "weakening" -> Weakening (get_int rule_request_as_json "formulaPosition")
        | "contraction" -> Contraction (get_int rule_request_as_json "formulaPosition")
        | "exchange" -> Exchange (get_int_list rule_request_as_json "displayPermutation", get_int_list rule_request_as_json "permutation")
        | "cut" -> Cut (get_formula rule_request_as_json "formula", get_int rule_request_as_json "formulaPosition")
        | "unfold_litt" -> Unfold_litt (get_int rule_request_as_json "formulaPosition")
        | "unfold_dual" -> Unfold_dual (get_int rule_request_as_json "formulaPosition")
        | _ -> raise (Json_exception ("unknown rule '" ^ rule ^ "'"));;

(* RULE -> JSON *)

let to_json = function
    | Axiom -> `Assoc [("rule", `String "axiom")]
    | One -> `Assoc [("rule", `String "one")]
    | Bottom formula_position -> `Assoc [
        ("rule", `String "bottom");
        ("formulaPosition", `Int formula_position)]
    | Top formula_position -> `Assoc [
        ("rule", `String "top");
        ("formulaPosition", `Int formula_position)]
    | Zero -> `Assoc [("rule", `String "zero")]
    | Tensor formula_position -> `Assoc [
        ("rule", `String "tensor");
        ("formulaPosition", `Int formula_position)]
    | Par formula_position -> `Assoc [
        ("rule", `String "par");
        ("formulaPosition", `Int formula_position)]
    | With formula_position -> `Assoc [
        ("rule", `String "with");
        ("formulaPosition", `Int formula_position)]
    | Plus_left formula_position -> `Assoc [
        ("rule", `String "plus_left");
        ("formulaPosition", `Int formula_position)]
    | Plus_right formula_position -> `Assoc [
        ("rule", `String "plus_right");
        ("formulaPosition", `Int formula_position)]
    | Promotion formula_position -> `Assoc [
        ("rule", `String "promotion");
        ("formulaPosition", `Int formula_position)]
    | Dereliction formula_position -> `Assoc [
        ("rule", `String "dereliction");
        ("formulaPosition", `Int formula_position)]
    | Weakening formula_position -> `Assoc [
        ("rule", `String "weakening");
        ("formulaPosition", `Int formula_position)]
    | Contraction formula_position -> `Assoc [
        ("rule", `String "contraction");
        ("formulaPosition", `Int formula_position)]
    | Exchange (display_permutation, permutation) -> `Assoc [
        ("rule", `String "exchange");
        ("displayPermutation", `List (List.map (fun n -> `Int n) display_permutation));
        ("permutation", `List (List.map (fun n -> `Int n) permutation))]
    | Cut (formula, formula_position) -> `Assoc [
        ("rule", `String "cut");
        ("formula", Raw_sequent.formula_to_json formula);
        ("formulaPosition", `Int formula_position)]
    | Unfold_litt formula_position -> `Assoc [
        ("rule", `String "unfold_litt");
        ("formulaPosition", `Int formula_position)]
    | Unfold_dual formula_position -> `Assoc [
        ("rule", `String "unfold_dual");
        ("formulaPosition", `Int formula_position)]
    ;;

let to_string rule_request =
    Yojson.Basic.to_string (to_json rule_request);;