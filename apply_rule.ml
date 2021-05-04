exception Bad_request_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Bad_request_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let apply_rule_with_exceptions auto_weak request_as_json =
    let rule_request_as_json = get_key request_as_json "ruleRequest" in
    let rule_request = Rule_request.from_json rule_request_as_json in
    let sequent_as_json = get_key request_as_json "sequent" in
    let sequent = Raw_sequent.sequent_from_json sequent_as_json in
    Proof.from_sequent_and_rule_request auto_weak sequent rule_request

let apply_rule auto_weak request_as_json =
    try let proof = apply_rule_with_exceptions auto_weak request_as_json in
        let proof_as_json = Proof.to_json proof in
        true, `Assoc [
            ("success", `Bool true);
            ("proof", proof_as_json)
        ]
    with
        | Bad_request_exception m -> false, `String ("Bad request: " ^ m)
        | Rule_request.Json_exception m -> false, `String ("Bad rule json: " ^ m)
        | Raw_sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m)
        | Proof.Rule_exception (is_pedagogic, m) -> if is_pedagogic
            then true, `Assoc [("success", `Bool false); ("errorMessage", `String m)]
            else false, `String m
