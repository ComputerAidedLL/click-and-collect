let apply_rule_with_exceptions request_as_json =
    let rule_request_as_json = Request_utils.get_key request_as_json "ruleRequest" in
    let rule_request = Rule_request.from_json rule_request_as_json in
    let sequent_with_notations = Sequent_with_notations.from_json request_as_json in
    Proof.from_sequent_and_rule_request sequent_with_notations.sequent sequent_with_notations.notations rule_request

let apply_rule request_as_json =
    try let proof = apply_rule_with_exceptions request_as_json in
        let proof_as_json = Proof.to_json proof in
        true, `Assoc [
            ("success", `Bool true);
            ("proof", proof_as_json)
        ]
    with
        | Request_utils.Bad_request_exception m -> false, `String ("Bad request: " ^ m)
        | Rule_request.Json_exception m -> false, `String ("Bad rule json: " ^ m)
        | Sequent_with_notations.Json_exception m -> false, `String ("Bad sequent with notations: " ^ m)
        | Proof.Rule_exception (is_pedagogic, m) -> if is_pedagogic
            then true, `Assoc [("success", `Bool false); ("errorMessage", `String m)]
            else false, `String m
