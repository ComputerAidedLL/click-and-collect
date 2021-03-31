(* PROOF *)
type proof = {
    sequent:Linear_logic.sequent;
    applied_rule: applied_rule option
}
and applied_rule = {
    rule:string;
    formula_positions: int list;
    premises: proof list
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

let get_json_int_list json key =
    let value = required_field json key in
    try List.map Yojson.Basic.Util.to_int (Yojson.Basic.Util.to_list value)
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_proof_json_exception ("field '" ^ key ^ "' must be a list of int"))

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
        let formula_positions = get_json_int_list json "formulaPositions" in
        let premises_as_json = get_json_list json "premises" in
        let premises = List.map json_to_proof premises_as_json in
        Some {rule=rule; formula_positions=formula_positions; premises=premises};;

(* OPERATIONS *)
exception Invalid_proof_exception of string;;

let rec is_valid proof =
    match proof.applied_rule with
        | None -> true
        | Some applied_rule -> try
                let expected_sequent_list = Linear_logic.apply_rule applied_rule.rule proof.sequent applied_rule.formula_positions in
                let get_sequent p = p.sequent in
                let given_sequent_list = List.map get_sequent applied_rule.premises in
                if expected_sequent_list <> given_sequent_list
                then raise (Invalid_proof_exception ("premises do not match expected sequent list after applying rule " ^ applied_rule.rule))
                else List.for_all is_valid applied_rule.premises
            with Linear_logic.Apply_rule_technical_exception m -> raise (Invalid_proof_exception ("Apply_rule_technical_exception: " ^ m))
            | Linear_logic.Apply_rule_logic_exception m -> raise (Invalid_proof_exception ("Apply_rule_logic_exception: " ^ m));;

let rec is_complete proof =
    match proof.applied_rule with
        | None -> false
        | Some applied_rule -> match applied_rule.premises with
            | [] -> true
            | premises -> List.for_all is_complete premises;;