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
    | Exchange of int list;;


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
        | "exchange" -> Exchange (get_int_list rule_request_as_json "permutation")
        | _ -> raise (Json_exception ("unknown rule '" ^ rule ^ "'"));;
