{
open Parser        (* The type token is defined in parser.mli *)
}
rule token = parse
    [' ' '\t']             { token lexbuf }     (* skip blanks *)
  | ['\n' ]                { EOL }
  | ['a'-'z' 'A'-'Z'] as l { LITT (String.make 1 l) }
  | '~'                    { NEG }
  | "/\\"                  { CONJ }
  | "\\/"                  { DISJ }
  | "=>" | "->"            { IMPL }
  | '('                    { LPAREN }
  | ')'                    { RPAREN }
  | eof                    { EOL }