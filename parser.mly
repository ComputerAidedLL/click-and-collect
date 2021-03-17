%{
open Formula
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

%token ORTH
%token TENSOR PAR WITH PLUS
%token ONE TOP BOTTOM ZERO
%token LOLLIPOP
%token OFCOURSE WHYNOT

%left THESIS							/* lowest precedence */
%left COMMA								/* low precedence */
%left IMPL LOLLIPOP						/* medium precedence */
%left CONJ DISJ TENSOR PAR WITH PLUS	/* high precedence */
%nonassoc NEG ORTH OFCOURSE WHYNOT		/* highest precedence */

%start main								/* the entry point */
%type <Formula.sequent> main
%%
main:
	EOL										{ Llsequent ([], []) }
  | THESIS EOL								{ Llsequent ([], []) }
  | lkformulalist EOL						{ Lksequent ([], $1) }
  | llformulalist EOL						{ Llsequent ([], $1) }
  | THESIS lkformulalist EOL				{ Lksequent ([], $2) }
  | THESIS llformulalist EOL				{ Llsequent ([], $2) }
  | lkformulalist THESIS lkformulalist EOL	{ Lksequent ($1, $3) }
  | llformulalist THESIS llformulalist EOL	{ Llsequent ($1, $3) }
;

lkformulalist:
	lkformula						{ [$1] }
  | lkformula COMMA lkformulalist	{  $1 :: $3 }
;
llformulalist:
	llformula						{ [$1] }
  | llformula COMMA llformulalist	{ $1 :: $3 }
;

lkformula:
	TRUE							{ True }
  | FALSE							{ False }
  | LITT							{ Litt $1 }
  | LPAREN lkformula RPAREN			{ $2 }
  | NEG lkformula					{ Neg ($2) }
  | lkformula CONJ lkformula		{ Conj ($1, $3) }
  | lkformula DISJ lkformula		{ Disj ($1, $3) }
  | lkformula IMPL lkformula		{ Impl ($1, $3) }
;
llformula:
	ONE								{ One }
  | TOP								{ Top }
  | BOTTOM							{ Bottom }
  | ZERO							{ Zero }
  | LITT							{ Litt $1 }
  | LPAREN llformula RPAREN			{ $2 }
  | llformula ORTH					{ Orth ($1) }
  | llformula TENSOR llformula		{ Tensor ($1, $3) }
  | llformula PAR llformula			{ Par ($1, $3) }
  | llformula WITH llformula		{ With ($1, $3) }
  | llformula PLUS llformula		{ Plus ($1, $3) }
  | llformula LOLLIPOP llformula	{ Lollipop ($1, $3) }
  | OFCOURSE llformula				{ Ofcourse ($2) }
  | WHYNOT llformula				{ Whynot ($2) }
;
