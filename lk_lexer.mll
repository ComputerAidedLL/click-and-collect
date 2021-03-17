{
open Lk_parser        (* The type token is defined in lk_parser.mli *)
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
    | eof                    { EOL }