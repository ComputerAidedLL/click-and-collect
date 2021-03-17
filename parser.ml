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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Formula
# 32 "parser.ml"
let yytransl_const = [|
  257 (* THESIS *);
  258 (* COMMA *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* EOL *);
  263 (* NEG *);
  264 (* CONJ *);
  265 (* DISJ *);
  266 (* TRUE *);
  267 (* FALSE *);
  268 (* IMPL *);
  269 (* ORTH *);
  270 (* TENSOR *);
  271 (* PAR *);
  272 (* WITH *);
  273 (* PLUS *);
  274 (* ONE *);
  275 (* TOP *);
  276 (* BOTTOM *);
  277 (* ZERO *);
  278 (* LOLLIPOP *);
  279 (* OFCOURSE *);
  280 (* WHYNOT *);
    0|]

let yytransl_block = [|
  259 (* LITT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\002\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\002\000\003\000\003\000\004\000\004\000\
\001\000\003\000\001\000\003\000\001\000\001\000\001\000\003\000\
\002\000\003\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\013\000\
\014\000\021\000\022\000\023\000\024\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\015\000\000\000\017\000\025\000\000\000\033\000\034\000\
\000\000\003\000\000\000\004\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\005\000\
\006\000\016\000\026\000\000\000\000\000\010\000\018\000\019\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\007\000\
\008\000"

let yydgoto = "\002\000\
\016\000\017\000\018\000\019\000\020\000"

let yysindex = "\001\000\
\046\255\000\000\009\255\000\000\086\255\000\000\051\255\000\000\
\000\000\000\000\000\000\000\000\000\000\122\255\122\255\000\000\
\016\255\020\255\152\255\167\255\000\000\000\255\012\255\255\254\
\180\255\000\000\051\255\000\000\000\000\122\255\000\000\000\000\
\051\255\000\000\122\255\000\000\051\255\051\255\051\255\051\255\
\122\255\000\000\122\255\122\255\122\255\122\255\122\255\000\000\
\000\000\000\000\000\000\018\255\025\255\000\000\000\000\000\000\
\065\255\000\000\032\255\032\255\032\255\032\255\161\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\070\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\045\255\062\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\186\255\000\000\097\255\115\255\133\255\151\255\157\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\011\000\002\000\252\255\251\255"

let yytablesize = 202
let yytable = "\025\000\
\024\000\001\000\028\000\050\000\023\000\048\000\038\000\039\000\
\031\000\032\000\040\000\004\000\005\000\022\000\021\000\007\000\
\033\000\049\000\008\000\009\000\035\000\034\000\024\000\064\000\
\025\000\036\000\010\000\011\000\012\000\013\000\065\000\014\000\
\015\000\055\000\056\000\057\000\053\000\059\000\060\000\061\000\
\062\000\063\000\058\000\052\000\042\000\009\000\003\000\054\000\
\004\000\005\000\009\000\006\000\007\000\026\000\027\000\008\000\
\009\000\007\000\000\000\000\000\008\000\009\000\011\000\010\000\
\011\000\012\000\013\000\011\000\014\000\015\000\015\000\015\000\
\038\000\039\000\015\000\015\000\000\000\015\000\015\000\000\000\
\000\000\015\000\025\000\025\000\025\000\025\000\025\000\000\000\
\004\000\005\000\000\000\025\000\007\000\000\000\000\000\008\000\
\009\000\028\000\028\000\000\000\000\000\028\000\028\000\010\000\
\011\000\012\000\013\000\000\000\014\000\015\000\028\000\028\000\
\028\000\028\000\000\000\029\000\029\000\000\000\028\000\029\000\
\029\000\000\000\000\000\000\000\029\000\030\000\000\000\000\000\
\029\000\029\000\029\000\029\000\000\000\030\000\030\000\000\000\
\029\000\030\000\030\000\010\000\011\000\012\000\013\000\000\000\
\014\000\015\000\030\000\030\000\030\000\030\000\000\000\031\000\
\031\000\037\000\030\000\031\000\031\000\032\000\032\000\038\000\
\039\000\032\000\032\000\040\000\031\000\031\000\031\000\031\000\
\041\000\000\000\000\000\000\000\031\000\042\000\043\000\044\000\
\045\000\046\000\032\000\042\000\043\000\044\000\045\000\046\000\
\051\000\000\000\020\000\020\000\047\000\000\000\020\000\020\000\
\042\000\043\000\044\000\045\000\046\000\020\000\000\000\000\000\
\000\000\047\000"

let yycheck = "\005\000\
\005\000\001\000\007\000\005\001\003\000\006\001\008\001\009\001\
\014\000\015\000\012\001\003\001\004\001\003\000\006\001\007\001\
\001\001\006\001\010\001\011\001\001\001\006\001\027\000\006\001\
\030\000\006\001\018\001\019\001\020\001\021\001\006\001\023\001\
\024\001\038\000\039\000\040\000\035\000\043\000\044\000\045\000\
\046\000\047\000\041\000\033\000\013\001\001\001\001\001\037\000\
\003\001\004\001\006\001\006\001\007\001\003\001\004\001\010\001\
\011\001\007\001\255\255\255\255\010\001\011\001\001\001\018\001\
\019\001\020\001\021\001\006\001\023\001\024\001\001\001\002\001\
\008\001\009\001\005\001\006\001\255\255\008\001\009\001\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\003\001\004\001\255\255\022\001\007\001\255\255\255\255\010\001\
\011\001\001\001\002\001\255\255\255\255\005\001\006\001\018\001\
\019\001\020\001\021\001\255\255\023\001\024\001\014\001\015\001\
\016\001\017\001\255\255\001\001\002\001\255\255\022\001\005\001\
\006\001\255\255\255\255\255\255\003\001\004\001\255\255\255\255\
\014\001\015\001\016\001\017\001\255\255\001\001\002\001\255\255\
\022\001\005\001\006\001\018\001\019\001\020\001\021\001\255\255\
\023\001\024\001\014\001\015\001\016\001\017\001\255\255\001\001\
\002\001\002\001\022\001\005\001\006\001\001\001\002\001\008\001\
\009\001\005\001\006\001\012\001\014\001\015\001\016\001\017\001\
\002\001\255\255\255\255\255\255\022\001\013\001\014\001\015\001\
\016\001\017\001\022\001\013\001\014\001\015\001\016\001\017\001\
\005\001\255\255\001\001\002\001\022\001\255\255\005\001\006\001\
\013\001\014\001\015\001\016\001\017\001\012\001\255\255\255\255\
\255\255\022\001"

let yynames_const = "\
  THESIS\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  NEG\000\
  CONJ\000\
  DISJ\000\
  TRUE\000\
  FALSE\000\
  IMPL\000\
  ORTH\000\
  TENSOR\000\
  PAR\000\
  WITH\000\
  PLUS\000\
  ONE\000\
  TOP\000\
  BOTTOM\000\
  ZERO\000\
  LOLLIPOP\000\
  OFCOURSE\000\
  WHYNOT\000\
  "

let yynames_block = "\
  LITT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
              ( Llsequent ([], []) )
# 209 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
                      ( Llsequent ([], []) )
# 215 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lkformulalist) in
    Obj.repr(
# 34 "parser.mly"
                           ( Lksequent ([], _1) )
# 222 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'llformulalist) in
    Obj.repr(
# 35 "parser.mly"
                           ( Llsequent ([], _1) )
# 229 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lkformulalist) in
    Obj.repr(
# 36 "parser.mly"
                                ( Lksequent ([], _2) )
# 236 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'llformulalist) in
    Obj.repr(
# 37 "parser.mly"
                                ( Llsequent ([], _2) )
# 243 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lkformulalist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'lkformulalist) in
    Obj.repr(
# 38 "parser.mly"
                                           ( Lksequent (_1, _3) )
# 251 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'llformulalist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'llformulalist) in
    Obj.repr(
# 39 "parser.mly"
                                           ( Llsequent (_1, _3) )
# 259 "parser.ml"
               : Formula.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lkformula) in
    Obj.repr(
# 43 "parser.mly"
                ( [_1] )
# 266 "parser.ml"
               : 'lkformulalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lkformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lkformulalist) in
    Obj.repr(
# 44 "parser.mly"
                                  (  _1 :: _3 )
# 274 "parser.ml"
               : 'lkformulalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 47 "parser.mly"
                ( [_1] )
# 281 "parser.ml"
               : 'llformulalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'llformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'llformulalist) in
    Obj.repr(
# 48 "parser.mly"
                                  ( _1 :: _3 )
# 289 "parser.ml"
               : 'llformulalist))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
            ( True )
# 295 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                ( False )
# 301 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
               ( Litt _1 )
# 308 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lkformula) in
    Obj.repr(
# 55 "parser.mly"
                              ( _2 )
# 315 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lkformula) in
    Obj.repr(
# 56 "parser.mly"
                      ( Neg (_2) )
# 322 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lkformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lkformula) in
    Obj.repr(
# 57 "parser.mly"
                              ( Conj (_1, _3) )
# 330 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lkformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lkformula) in
    Obj.repr(
# 58 "parser.mly"
                              ( Disj (_1, _3) )
# 338 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lkformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lkformula) in
    Obj.repr(
# 59 "parser.mly"
                              ( Impl (_1, _3) )
# 346 "parser.ml"
               : 'lkformula))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
            ( One )
# 352 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
               ( Top )
# 358 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                 ( Bottom )
# 364 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
               ( Zero )
# 370 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
               ( Litt _1 )
# 377 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'llformula) in
    Obj.repr(
# 67 "parser.mly"
                              ( _2 )
# 384 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'llformula) in
    Obj.repr(
# 68 "parser.mly"
                       ( Orth (_1) )
# 391 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'llformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 69 "parser.mly"
                                ( Tensor (_1, _3) )
# 399 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'llformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 70 "parser.mly"
                              ( Par (_1, _3) )
# 407 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'llformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 71 "parser.mly"
                              ( With (_1, _3) )
# 415 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'llformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 72 "parser.mly"
                              ( Plus (_1, _3) )
# 423 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'llformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 73 "parser.mly"
                                 ( Lollipop (_1, _3) )
# 431 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 74 "parser.mly"
                          ( Ofcourse (_2) )
# 438 "parser.ml"
               : 'llformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'llformula) in
    Obj.repr(
# 75 "parser.mly"
                        ( Whynot (_2) )
# 445 "parser.ml"
               : 'llformula))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Formula.sequent)
