let ll_parse sequent_as_string =
    Ll_parser.main Ll_lexer.token (Lexing.from_string sequent_as_string);;

let except_syntax_error parse_method s =
    try parse_method s
    with t when Printexc.to_string t = "Parsing.Parse_error"
        || Printexc.to_string t = "(Failure \"lexing: empty token\")" ->
    `Assoc [("is_valid", `Bool false);("error_message", `String "Syntax error: please read the syntax rules.")]

let safe_parse sequent_as_string =
    except_syntax_error (fun s -> let raw_sequent = ll_parse s in
        let sequent = Raw_sequent.to_sequent raw_sequent in
        let proof = Proof.Hypothesis_proof sequent in
            `Assoc [("is_valid", `Bool true);("proof", Proof.to_json proof)]
    ) sequent_as_string;;

let safe_parse_formula formula_as_string =
    except_syntax_error (fun s -> let raw_sequent = ll_parse s in
        match raw_sequent with
            | {hyp=[]; cons=[e]} -> `Assoc [("is_valid", `Bool true); ("formula", Raw_sequent.raw_formula_to_json e)]
            | _ -> `Assoc [("is_valid", `Bool false); ("error_message", `String "Input must contain exactly one formula.")]
    ) formula_as_string;;

let safe_is_valid_litt litt =
    except_syntax_error (fun l -> let raw_sequent = ll_parse l in
        match raw_sequent with
            | {hyp=[]; cons=[Litt s]} -> `Assoc [("is_valid", `Bool true); ("value", `String s)]
            | _ -> `Assoc [("is_valid", `Bool false); ("error_message", `String "Input must contain exactly one litteral.")]
    ) litt;;
