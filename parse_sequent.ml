let ll_parse sequent_as_string =
    Ll_parser.main Ll_lexer.token (Lexing.from_string sequent_as_string);;

let safe_parse sequent_as_string =
    try let raw_sequent = ll_parse sequent_as_string in
        let sequent = Raw_sequent.to_sequent raw_sequent in
        let proof = Proof.Hypothesis_proof sequent in
        true, Proof.to_json proof
    with t -> if Printexc.to_string t = "Stdlib.Parsing.Parse_error"
            || Printexc.to_string t = "Failure(\"lexing: empty token\")" then
        false, `String "Syntax error: please read the syntax rules."
        else false, `String ("Technical error: " ^ Printexc.to_string t);;

let safe_parse_formula formula_as_string =
    try let raw_sequent = ll_parse formula_as_string in
        match raw_sequent with
        | {hyp=[]; cons=[e]} -> true, Raw_sequent.raw_formula_to_json e
        | _ -> false, `String "Input must contain exactly one formula."
    with t -> if Printexc.to_string t = "Stdlib.Parsing.Parse_error"
            || Printexc.to_string t = "Failure(\"lexing: empty token\")" then
        false, `String "Syntax error: please read the syntax rules."
        else false, `String ("Technical error: " ^ Printexc.to_string t);;

let safe_is_valid_litt litt =
    try let raw_sequent = ll_parse litt in
        match raw_sequent with
        | {hyp=[]; cons=[Litt s]} -> true
        | _ -> false
    with _ -> false;;
