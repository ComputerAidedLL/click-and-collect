exception Bad_request_exception of string

let get_key d k =
    let value =
        try Yojson.Basic.Util.member k d
        with Yojson.Basic.Util.Type_error (_, _) -> raise (Bad_request_exception ("request body must be a json object"))
    in
    if value = `Null
    then raise (Bad_request_exception ("required argument '" ^ k ^ "' is missing"))
    else value

let check_sequent_provability sequent =
    (Clauses.provable_sequent_as_classical sequent) &&
    (Phase.valid_sequent sequent) &&
    (Slice.provable_sequent_as_slices sequent)

let is_sequent_provable_with_exceptions request_as_json =
    let sequent_as_json = get_key request_as_json "sequent" in
    let sequent = Raw_sequent.sequent_from_json sequent_as_json in
    let notations_as_json = get_key request_as_json "notations" in
    let notations = Notations.from_json notations_as_json in
    check_sequent_provability sequent || begin
        let cyclic_notations, sorted_acyclic_notations = Notations.split_cyclic_acyclic notations sequent in
        if List.length cyclic_notations > 0
        then true
        else if List.length sorted_acyclic_notations = 0
            then false
            else
                let sequent = Notations.replace_all_notations_in_sequent sequent sorted_acyclic_notations in
                check_sequent_provability sequent
    end;;

let is_sequent_provable request_as_json =
    try let is_provable = is_sequent_provable_with_exceptions request_as_json in
        true, `Assoc [("is_provable", `Bool is_provable)]
    with | Bad_request_exception m -> false, `String ("Bad request: " ^ m)
         | Raw_sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m)
         | Notations.Json_exception m -> false, `String ("Bad notations json: " ^ m);;
