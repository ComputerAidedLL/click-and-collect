open Yojson

let ll_parse proof_as_string =
    let sequent = Ll_parser.main Ll_lexer.token (Lexing.from_string proof_as_string) in
    let monolatery_sequent = Linear_logic.get_monolatery_sequent sequent in
    let sequent_as_json = Linear_logic.sequent_to_json monolatery_sequent in
    Yojson.to_string sequent_as_json;;

let lk_parse proof_as_string =
    let sequent = Lk_parser.main Lk_lexer.token (Lexing.from_string proof_as_string) in
    let monolatery_sequent = Classical_logic.get_monolatery_sequent sequent in
    let sequent_as_json = Classical_logic.sequent_to_json monolatery_sequent in
    Yojson.to_string sequent_as_json;;

let safe_parse proof_as_string =
    try true, ll_parse proof_as_string
    with Stdlib.Parsing.Parse_error ->
        try true, lk_parse proof_as_string
        with Stdlib.Parsing.Parse_error -> false, "Syntax error: please read the syntax rules"
    ;;
