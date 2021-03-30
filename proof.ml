(* PROOF *)
type proof = {
    sequent:Linear_logic.sequent;
    applied_rule: applied_rule option
}
and applied_rule = {
    rule:string;
    formula_position: int;
    premisses: proof list
};;

(* JSON -> PROOF *)
exception Bad_proof_json_exception of string;;

let optional_field json key =
    let value =
        try Yojson.Basic.Util.member key json
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_proof_json_exception ("a proof must be a json object"))
    in
    value

let required_field json key =
    let value = optional_field json key in
    if value = `Null
    then raise (Bad_proof_json_exception ("required field '" ^ key ^ "' is missing"))
    else value

let get_json_string json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_string value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_proof_json_exception ("field '" ^ key ^ "' must be a string"))

let get_json_int json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_int value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_proof_json_exception ("field '" ^ key ^ "' must be an int"))

let get_json_list json key =
    let value = required_field json key in
    try Yojson.Basic.Util.to_list value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_proof_json_exception ("field '" ^ key ^ "' must be a list"))

let rec json_to_proof json =
    let sequent_as_json = required_field json "sequentAsJson" in
    let sequent = Linear_logic.json_to_sequent sequent_as_json in
    let applied_ruled_as_json = optional_field json "appliedRule" in
    let applied_rule = json_to_applied_rule applied_ruled_as_json in
    {sequent=sequent; applied_rule=applied_rule}
and json_to_applied_rule json =
    match json with
    | `Null -> None
    | _ -> let rule = get_json_string json "rule" in
        let formula_position = get_json_int json "formulaPosition" in
        let premisses_as_json = get_json_list json "premisses" in
        let premisses = List.map json_to_proof premisses_as_json in
        Some {rule=rule; formula_position=formula_position; premisses=premisses};;

(* OPERATIONS *)
exception Invalid_proof_exception of string;;

let rec is_valid proof =
    match proof.applied_rule with
        | None -> true
        | Some applied_rule -> try
                let expected_sequent_list = Linear_logic.apply_rule applied_rule.rule proof.sequent applied_rule.formula_position in
                let get_sequent p = p.sequent in
                let given_sequent_list = List.map get_sequent applied_rule.premisses in
                if not (expected_sequent_list = given_sequent_list)
                then raise (Invalid_proof_exception ("Premisses do not match expected sequent list after applying rule " ^ applied_rule.rule))
                else List.for_all is_valid applied_rule.premisses
            with Linear_logic.Apply_rule_technical_exception m -> raise (Invalid_proof_exception ("Apply_rule_technical_exception: " ^ m))
            | Linear_logic.Apply_rule_logic_exception m -> raise (Invalid_proof_exception ("Apply_rule_logic_exception: " ^ m));;

let rec is_complete proof =
    match proof.applied_rule with
        | None -> false
        | Some applied_rule -> match applied_rule.premisses with
            | [] -> true
            | premisses -> List.for_all is_complete premisses;;