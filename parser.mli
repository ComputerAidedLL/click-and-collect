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
  | ORTH
  | TENSOR
  | PAR
  | WITH
  | PLUS
  | ONE
  | TOP
  | BOTTOM
  | ZERO
  | LOLLIPOP
  | OFCOURSE
  | WHYNOT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Formula.sequent
