let ll_parse sequent_as_string =
    let raw_sequent = Ll_parser.main Ll_lexer.token (Lexing.from_string sequent_as_string) in
    let sequent = Raw_sequent.to_sequent raw_sequent in
    let proof = Proof.Hypothesis_proof sequent in
    let proof_as_json = Proof.to_json proof in
    proof_as_json;;

let safe_parse sequent_as_string =
    try true, ll_parse sequent_as_string
    with t -> if Printexc.to_string t = "Stdlib.Parsing.Parse_error"
            || Printexc.to_string t = "Failure(\"lexing: empty token\")" then
        false, `String "Syntax error: please read the syntax rules."
        else false, `String ("Technical error: " ^ Printexc.to_string t);;
