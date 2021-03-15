type token =
  | LITT of (string)
  | NEG
  | CONJ
  | DISJ
  | IMPL
  | LPAREN
  | RPAREN
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Formula.f
