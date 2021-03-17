{
open Parser        (* The type token is defined in parser.mli *)
}
rule token = parse
    [' ' '\t']               { token lexbuf }     (* skip blanks *)
    | ['\n']                 { EOL }
    | "|-" | "⊢"             { THESIS }
    | ','                    { COMMA }
    | ['a'-'z' 'A'-'Z'] as l { LITT (String.make 1 l) }
    | '('                    { LPAREN }
    | ')'                    { RPAREN }
    | '~' | "¬"              { NEG }
    | "/\\"                  { CONJ }
    | "\\/"                  { DISJ }
    | "true"                 { TRUE }
    | "false"                { FALSE }
    | "=>" | "->" | "→"      { IMPL }
    | '^'                    { ORTH }
    | '*' | "⊗"              { TENSOR }
    | '|'                    { PAR }
    | '&'                    { WITH }
    | '+' | "⊕"              { PLUS }
    | '1'                    { ONE }
    | "top"                  { TOP }
    | "bot"                  { BOTTOM }
    | '0'                    { ZERO }
    | "-o" | "⊸"             { LOLLIPOP }
    | '!'                    { OFCOURSE }
    | '?'                    { WHYNOT }
    | eof                    { EOL }