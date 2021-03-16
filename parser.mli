type token =
  | LITT of (string)
  | NEG
  | CONJ
  | DISJ
  | IMPL
  | LPAREN
  | RPAREN
  | COMMA
  | THESIS
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Formula.f list * Formula.f list
