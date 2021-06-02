let check_sequent_provability sequent =
    (Clauses.provable_sequent_as_classical sequent) &&
    (Phase.valid_sequent sequent) &&
    (Slice.provable_sequent_as_slices sequent)

let is_sequent_provable_with_exceptions request_as_json =
    let sequent_with_notations = Sequent_with_notations.from_json request_as_json in
    let cyclic_notations, sorted_acyclic_notations = Sequent_with_notations.split_cyclic_acyclic sequent_with_notations in
    if List.length cyclic_notations > 0
    then true
    else let sequent = Sequent_with_notations.replace_all_notations_in_sequent sequent_with_notations.sequent sorted_acyclic_notations in
        check_sequent_provability sequent;;

let is_sequent_provable request_as_json =
    try let is_provable = is_sequent_provable_with_exceptions request_as_json in
        true, `Assoc [("is_provable", `Bool is_provable)]
    with Sequent_with_notations.Json_exception m -> false, `String ("Bad sequent with notations: " ^ m);;
