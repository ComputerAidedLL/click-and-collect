%{
open Formula
%}

%token <string> LITT
%token NEG
%token CONJ DISJ IMPL
%token LPAREN RPAREN
%token COMMA
%token THESIS
%token EOL
%left THESIS            /* lowest precedence */
%left COMMA             /* low precedence */
%left IMPL              /* medium precedence */
%left CONJ DISJ         /* high precedence */
%nonassoc NEG           /* highest precedence */
%start main             /* the entry point */
%type <Formula.f list * Formula.f list> main
%%
main:
    EOL                                { [], [] }
  | THESIS EOL                         { [], [] }
  | formulalist EOL                    { [], $1 }
  | THESIS formulalist EOL             { [], $2 }
  | formulalist THESIS formulalist EOL { $1, $3 }
;
formulalist:
    formula                     { [$1] }
  | formula COMMA formulalist   {  $1 :: $3 }
;
formula:
    LITT                    	{ Litt $1 }
  | LPAREN formula RPAREN      	{ $2 }
  | NEG formula					{ Neg ($2) }
  | formula CONJ formula        { Conj ($1, $3) }
  | formula DISJ formula        { Disj ($1, $3) }
  | formula IMPL formula        { Impl ($1, $3) }
;
