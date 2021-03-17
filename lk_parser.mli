type token =
  | THESIS
  | COMMA
  | LITT of (string)
  | LPAREN
  | RPAREN
  | EOL
  | NEG
  | CONJ
  | DISJ
  | TRUE
  | FALSE
  | IMPL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Classical_logic.formula list * Classical_logic.formula list
