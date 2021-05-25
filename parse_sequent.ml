let ll_parse sequent_as_string =
    let raw_sequent = Ll_parser.main Ll_lexer.token (Lexing.from_string sequent_as_string) in
    Raw_sequent.to_sequent raw_sequent;;

let safe_parse sequent_as_string =
    try let sequent = ll_parse sequent_as_string in
        let proof = Proof.Hypothesis_proof sequent in
        true, Proof.to_json proof
    with t -> if Printexc.to_string t = "Stdlib.Parsing.Parse_error"
            || Printexc.to_string t = "Failure(\"lexing: empty token\")" then
        false, `String "Syntax error: please read the syntax rules."
        else false, `String ("Technical error: " ^ Printexc.to_string t);;

let safe_parse_formula formula_as_string =
    try let sequent = ll_parse formula_as_string in
        match sequent with
        | [e] -> true, Raw_sequent.formula_to_json e
        | _ -> false, `String "Input must contain exactly one formula."
    with t -> if Printexc.to_string t = "Stdlib.Parsing.Parse_error"
            || Printexc.to_string t = "Failure(\"lexing: empty token\")" then
        false, `String "Syntax error: please read the syntax rules."
        else false, `String ("Technical error: " ^ Printexc.to_string t);;

let safe_is_valid_litt litt =
    try let sequent = ll_parse litt in
        match sequent with
        | [Litt s] -> true
        | _ -> false
    with _ -> false;;
