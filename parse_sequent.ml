let ll_parse sequent_as_string =
    let sequent = Ll_parser.main Ll_lexer.token (Lexing.from_string sequent_as_string) in
    let monolatery_sequent = Linear_logic.get_monolatery_sequent sequent in
    let sequent_as_json = Linear_logic.sequent_to_json monolatery_sequent in
    sequent_as_json;;

let lk_parse sequent_as_string =
    let sequent = Lk_parser.main Lk_lexer.token (Lexing.from_string sequent_as_string) in
    let monolatery_sequent = Classical_logic.get_monolatery_sequent sequent in
    let sequent_as_json = Classical_logic.sequent_to_json monolatery_sequent in
    sequent_as_json;;

let safe_parse sequent_as_string =
    try true, ll_parse sequent_as_string
    with t -> try true, lk_parse sequent_as_string
    with t -> if Printexc.to_string t = "Stdlib.Parsing.Parse_error"
            || Printexc.to_string t = "Failure(\"lexing: empty token\")" then
        false, `String "Syntax error: please read the syntax rules"
        else false, `String ("Technical error: " ^ Printexc.to_string t);;
