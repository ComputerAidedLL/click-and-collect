exception Bad_request_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Bad_request_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let apply_rule_with_exceptions request_as_json auto_reverse_mode =
    let rule_request_as_json = get_key request_as_json "ruleRequest" in
    let rule_request = Rule_request.from_json rule_request_as_json in
    let sequent_as_json = get_key request_as_json "sequent" in
    let sequent = Raw_sequent.sequent_from_json sequent_as_json in
    let proof = Proof.from_sequent_and_rule_request sequent rule_request in
    let premises = Proof.get_premises proof in
    if auto_reverse_mode then List.map Proof.rec_apply_reversible_rule premises
    else premises

let apply_rule request_as_json auto_reverse_mode =
    try let premises = apply_rule_with_exceptions request_as_json auto_reverse_mode in
        let premises_as_json = List.map Proof.to_json premises in
        true, `Assoc [
            ("success", `Bool true);
            ("premises", `List premises_as_json)
        ]
    with
        | Bad_request_exception m -> false, `String ("Bad request: " ^ m)
        | Rule_request.Json_exception m -> false, `String ("Bad rule json: " ^ m)
        | Raw_sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m)
        | Proof.Rule_exception (is_pedagogic, m) -> if is_pedagogic
            then true, `Assoc [("success", `Bool false); ("errorMessage", `String m)]
            else false, `String m
