%{
open Linear_logic
%}

%token THESIS
%token COMMA
%token <string> LITT
%token LPAREN RPAREN
%token EOL

%token ORTH
%token TENSOR PAR WITH PLUS
%token ONE BOTTOM TOP ZERO
%token LOLLIPOP
%token OFCOURSE WHYNOT

%left THESIS                     /* lowest precedence */
%left COMMA                      /* low precedence */
%left LOLLIPOP                   /* medium precedence */
%left TENSOR PAR WITH PLUS       /* high precedence */
%nonassoc ORTH OFCOURSE WHYNOT   /* highest precedence */

%start main                      /* the entry point */
%type <Linear_logic.formula list * Linear_logic.formula list> main
%%
main:
    EOL                                   { ([], []) }
  | THESIS EOL                            { ([], []) }
  | formulalist EOL                       { ([], $1) }
  | THESIS formulalist EOL                { ([], $2) }
  | formulalist THESIS formulalist EOL    { ($1, $3) }
;

formulalist:
    formula                     { [$1] }
  | formula COMMA formulalist   { $1 :: $3 }
;

formula:
    ONE                         { One }
  | BOTTOM                      { Bottom }
  | TOP                         { Top }
  | ZERO                        { Zero }
  | LITT                        { Litt $1 }
  | LPAREN formula RPAREN       { $2 }
  | formula ORTH                { Orth ($1) }
  | formula TENSOR formula      { Tensor ($1, $3) }
  | formula PAR formula         { Par ($1, $3) }
  | formula WITH formula        { With ($1, $3) }
  | formula PLUS formula        { Plus ($1, $3) }
  | formula LOLLIPOP formula    { Lollipop ($1, $3) }
  | OFCOURSE formula            { Ofcourse ($2) }
  | WHYNOT formula              { Whynot ($2) }
;
