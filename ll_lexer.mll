{
open Ll_parser        (* The type token is defined in ll_parser.mli *)
}
rule token = parse
    [' ' '\t']                       { token lexbuf }     (* skip blanks *)
    | ['\n']                         { EOL }
    | "|-" | "⊢"                     { THESIS }
    | ','                            { COMMA }
    | ['a'-'z' 'A'-'S' 'U'-'Z'] as l { LITT (String.make 1 l) }
    | '('                            { LPAREN }
    | ')'                            { RPAREN }
    | '^'                            { ORTH }
    | '*' | "⊗"                      { TENSOR }
    | '|'                            { PAR }
    | '&'                            { WITH }
    | '+' | "⊕"                      { PLUS }
    | '1'                            { ONE }
    | "bot" | "⊥" | '_'              { BOTTOM }
    | "top" | "⊤" | 'T'              { TOP }
    | '0'                            { ZERO }
    | "-o" | "⊸"                     { LOLLIPOP }
    | '!'                            { OFCOURSE }
    | '?'                            { WHYNOT }
    | eof                            { EOL }