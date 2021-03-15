%{
open Formula
%}

%token <string> LITT
%token NEG
%token CONJ DISJ IMPL
%token LPAREN RPAREN
%token EOL
%left IMPL              /* lowest precedence */
%left CONJ DISJ         /* medium precedence */
%nonassoc NEG           /* highest precedence */
%start main             /* the entry point */
%type <Formula.f> main
%%
main:
    expr EOL                { $1 }
;
expr:
    LITT                    { Litt $1 }
  | LPAREN expr RPAREN      { $2 }
  | NEG expr				{ Neg ($2) }
  | expr CONJ expr          { Conj ($1, $3) }
  | expr DISJ expr          { Disj ($1, $3) }
  | expr IMPL expr          { Impl ($1, $3) }
;
