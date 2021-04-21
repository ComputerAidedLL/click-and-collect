let is_sequent_provable_with_exceptions request_as_json =
    let sequent = Raw_sequent.sequent_from_json request_as_json in
    if not (Clauses.provable_sequent_as_classical sequent) then false
    else Phase.valid_sequent sequent;;

let is_sequent_provable request_as_json =
    try let is_provable = is_sequent_provable_with_exceptions request_as_json in
        true, `Assoc [("is_provable", `Bool is_provable)]
    with Raw_sequent.Json_exception m -> false, `String ("Bad sequent json: " ^ m);;
