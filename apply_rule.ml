exception Bad_request_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Bad_request_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let get_list d k =
    let value = get_key d k in
    try Yojson.Basic.Util.to_list value
    with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("argument '" ^ k ^ "' must be a list"))

let apply_rule_with_exceptions request_as_json =
    let rule_request_as_json = get_key request_as_json "ruleRequest" in
    let rule_request = Rule_request.from_json rule_request_as_json in
    let sequent_as_json = get_key request_as_json "sequent" in
    let sequent = Raw_sequent.sequent_from_json sequent_as_json in
    let proof = Proof.from_sequent_and_rule_request sequent rule_request in
    Proof.get_premises proof

let apply_rule request_as_json =
    try let proof_list = apply_rule_with_exceptions request_as_json in
        let sequent_list = List.map Proof.get_conclusion proof_list in
        true, `Assoc [
            ("success", `Bool true);
            ("sequentList", `List (List.map Raw_sequent.sequent_to_json sequent_list))
        ]
    with
        | Bad_request_exception m -> false, `String ("Bad request: " ^ m)
        | Rule_request.Json_exception m -> false, `String ("Bad rule json: " ^ m)
        | Raw_sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m)
        | Proof.Technical_exception m -> false, `String m
        | Proof.Pedagogic_exception m ->
            true, `Assoc [("success", `Bool false); ("errorMessage", `String m)]
