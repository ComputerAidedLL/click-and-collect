%{
open Classical_logic
%}

%token THESIS
%token COMMA
%token <string> LITT
%token LPAREN RPAREN
%token EOL

%token NEG
%token CONJ DISJ
%token TRUE FALSE
%token IMPL

%left THESIS       /* lowest precedence */
%left COMMA        /* low precedence */
%left IMPL         /* medium precedence */
%left CONJ DISJ    /* high precedence */
%nonassoc NEG      /* highest precedence */

%start main        /* the entry point */
%type <Classical_logic.formula list * Classical_logic.formula list> main
%%
main:
    EOL                                   { ([], []) }
  | THESIS EOL                            { ([], []) }
  | formulalist EOL                       { ([], $1) }
  | THESIS formulalist EOL                { ([], $2) }
  | formulalist THESIS formulalist EOL    { ($1, $3) }
;

formulalist:
    formula                      { [$1] }
  | formula COMMA formulalist    { $1 :: $3 }
;

formula:
    TRUE                        { True }
  | FALSE                       { False }
  | LITT                        { Litt $1 }
  | LPAREN formula RPAREN       { $2 }
  | NEG formula                 { Neg ($2) }
  | formula CONJ formula        { Conj ($1, $3) }
  | formula DISJ formula        { Disj ($1, $3) }
  | formula IMPL formula        { Impl ($1, $3) }
;
