%{
open Raw_sequent
%}

%token THESIS
%token COMMA
%token <string> LITT
%token LPAREN RPAREN
%token EOL

%token DUAL
%token TENSOR PAR WITH PLUS
%token ONE BOTTOM TOP ZERO
%token LOLLIPOP
%token OFCOURSE WHYNOT

%left THESIS                /* lowest precedence */
%left COMMA                 /* low precedence */
%right LOLLIPOP             /* medium precedence */
%left TENSOR PAR WITH PLUS  /* high precedence */
%nonassoc OFCOURSE WHYNOT   /* very high precedence */
%nonassoc DUAL              /* highest precedence */

%start main                 /* the entry point */
%type <Raw_sequent.raw_sequent> main
%%
main:
    EOL                                   { {hyp=[]; cons=[]} }
  | THESIS EOL                            { {hyp=[]; cons=[]} }
  | formulalist EOL                       { {hyp=[]; cons=$1} }
  | formulalist THESIS EOL                { {hyp=$1; cons=[]} }
  | THESIS formulalist EOL                { {hyp=[]; cons=$2} }
  | formulalist THESIS formulalist EOL    { {hyp=$1; cons=$3} }
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
  | formula DUAL                { Dual ($1) }
  | formula TENSOR formula      { Tensor ($1, $3) }
  | formula PAR formula         { Par ($1, $3) }
  | formula WITH formula        { With ($1, $3) }
  | formula PLUS formula        { Plus ($1, $3) }
  | formula LOLLIPOP formula    { Lollipop ($1, $3) }
  | OFCOURSE formula            { Ofcourse ($2) }
  | WHYNOT formula              { Whynot ($2) }
;
