type token =
  | THESIS
  | COMMA
  | LITT of (string)
  | LPAREN
  | RPAREN
  | EOL
  | ORTH
  | TENSOR
  | PAR
  | WITH
  | PLUS
  | ONE
  | BOTTOM
  | TOP
  | ZERO
  | LOLLIPOP
  | OFCOURSE
  | WHYNOT

open Parsing;;
let _ = parse_error;;
# 2 "ll_parser.mly"
open Sequent
# 26 "ll_parser.ml"
let yytransl_const = [|
  257 (* THESIS *);
  258 (* COMMA *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* EOL *);
  263 (* ORTH *);
  264 (* TENSOR *);
  265 (* PAR *);
  266 (* WITH *);
  267 (* PLUS *);
  268 (* ONE *);
  269 (* BOTTOM *);
  270 (* TOP *);
  271 (* ZERO *);
  272 (* LOLLIPOP *);
  273 (* OFCOURSE *);
  274 (* WHYNOT *);
    0|]

let yytransl_block = [|
  259 (* LITT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\003\000\003\000\004\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\003\000\002\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\013\000\000\000\001\000\009\000\010\000\
\011\000\012\000\000\000\000\000\023\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\003\000\000\000\015\000\
\000\000\000\000\000\000\000\000\000\000\005\000\014\000\004\000\
\000\000\008\000\000\000\000\000\000\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000"

let yysindex = "\002\000\
\026\255\000\000\254\254\000\000\129\255\000\000\000\000\000\000\
\000\000\000\000\129\255\129\255\000\000\008\255\143\255\000\000\
\255\254\153\255\010\255\010\255\042\255\000\000\129\255\000\000\
\129\255\129\255\129\255\129\255\129\255\000\000\000\000\000\000\
\012\255\000\000\010\255\010\255\010\255\010\255\163\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\255\000\000\
\000\000\000\000\060\255\072\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\084\255\096\255\108\255\120\255\133\255\000\000"

let yygindex = "\000\000\
\000\000\005\000\251\255"

let yytablesize = 179
let yytable = "\018\000\
\004\000\005\000\001\000\016\000\030\000\019\000\020\000\017\000\
\021\000\007\000\008\000\009\000\010\000\022\000\011\000\012\000\
\024\000\040\000\000\000\035\000\036\000\037\000\038\000\039\000\
\000\000\033\000\003\000\034\000\004\000\005\000\007\000\006\000\
\000\000\000\000\000\000\007\000\000\000\007\000\008\000\009\000\
\010\000\000\000\011\000\012\000\004\000\005\000\000\000\032\000\
\000\000\000\000\000\000\000\000\000\000\007\000\008\000\009\000\
\010\000\000\000\011\000\012\000\021\000\021\000\000\000\000\000\
\021\000\021\000\000\000\021\000\021\000\021\000\021\000\000\000\
\022\000\022\000\000\000\021\000\022\000\022\000\000\000\022\000\
\022\000\022\000\022\000\000\000\016\000\016\000\000\000\022\000\
\016\000\016\000\000\000\016\000\016\000\016\000\016\000\000\000\
\017\000\017\000\000\000\016\000\017\000\017\000\000\000\017\000\
\017\000\017\000\017\000\000\000\018\000\018\000\000\000\017\000\
\018\000\018\000\000\000\018\000\018\000\018\000\018\000\000\000\
\019\000\019\000\000\000\018\000\019\000\019\000\000\000\019\000\
\019\000\019\000\019\000\004\000\005\000\020\000\020\000\019\000\
\000\000\020\000\020\000\000\000\007\000\008\000\009\000\010\000\
\023\000\011\000\012\000\000\000\000\000\024\000\025\000\026\000\
\027\000\028\000\000\000\000\000\000\000\031\000\029\000\024\000\
\025\000\026\000\027\000\028\000\000\000\000\000\000\000\000\000\
\029\000\024\000\025\000\026\000\027\000\028\000\000\000\000\000\
\000\000\000\000\029\000"

let yycheck = "\005\000\
\003\001\004\001\001\000\006\001\006\001\011\000\012\000\003\000\
\001\001\012\001\013\001\014\001\015\001\006\001\017\001\018\001\
\007\001\006\001\255\255\025\000\026\000\027\000\028\000\029\000\
\255\255\021\000\001\001\023\000\003\001\004\001\001\001\006\001\
\255\255\255\255\255\255\006\001\255\255\012\001\013\001\014\001\
\015\001\255\255\017\001\018\001\003\001\004\001\255\255\006\001\
\255\255\255\255\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\255\255\017\001\018\001\001\001\002\001\255\255\255\255\
\005\001\006\001\255\255\008\001\009\001\010\001\011\001\255\255\
\001\001\002\001\255\255\016\001\005\001\006\001\255\255\008\001\
\009\001\010\001\011\001\255\255\001\001\002\001\255\255\016\001\
\005\001\006\001\255\255\008\001\009\001\010\001\011\001\255\255\
\001\001\002\001\255\255\016\001\005\001\006\001\255\255\008\001\
\009\001\010\001\011\001\255\255\001\001\002\001\255\255\016\001\
\005\001\006\001\255\255\008\001\009\001\010\001\011\001\255\255\
\001\001\002\001\255\255\016\001\005\001\006\001\255\255\008\001\
\009\001\010\001\011\001\003\001\004\001\001\001\002\001\016\001\
\255\255\005\001\006\001\255\255\012\001\013\001\014\001\015\001\
\002\001\017\001\018\001\255\255\255\255\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\005\001\016\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\255\255\255\255\
\016\001\007\001\008\001\009\001\010\001\011\001\255\255\255\255\
\255\255\255\255\016\001"

let yynames_const = "\
  THESIS\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  ORTH\000\
  TENSOR\000\
  PAR\000\
  WITH\000\
  PLUS\000\
  ONE\000\
  BOTTOM\000\
  TOP\000\
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
# 28 "ll_parser.mly"
                                          ( {hyp=[]; cons=[]} )
# 169 "ll_parser.ml"
               : Sequent.sequent))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "ll_parser.mly"
                                          ( {hyp=[]; cons=[]} )
# 175 "ll_parser.ml"
               : Sequent.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 30 "ll_parser.mly"
                                          ( {hyp=[]; cons=_1} )
# 182 "ll_parser.ml"
               : Sequent.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formulalist) in
    Obj.repr(
# 31 "ll_parser.mly"
                                          ( {hyp=_1; cons=[]} )
# 189 "ll_parser.ml"
               : Sequent.sequent))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 32 "ll_parser.mly"
                                          ( {hyp=[]; cons=_2} )
# 196 "ll_parser.ml"
               : Sequent.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formulalist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 33 "ll_parser.mly"
                                          ( {hyp=_1; cons=_3} )
# 204 "ll_parser.ml"
               : Sequent.sequent))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 37 "ll_parser.mly"
                                ( [_1] )
# 211 "ll_parser.ml"
               : 'formulalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formulalist) in
    Obj.repr(
# 38 "ll_parser.mly"
                                ( _1 :: _3 )
# 219 "ll_parser.ml"
               : 'formulalist))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "ll_parser.mly"
                                ( One )
# 225 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "ll_parser.mly"
                                ( Bottom )
# 231 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "ll_parser.mly"
                                ( Top )
# 237 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "ll_parser.mly"
                                ( Zero )
# 243 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "ll_parser.mly"
                                ( Litt _1 )
# 250 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 47 "ll_parser.mly"
                                ( _2 )
# 257 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 48 "ll_parser.mly"
                                ( Orth (_1) )
# 264 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 49 "ll_parser.mly"
                                ( Tensor (_1, _3) )
# 272 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 50 "ll_parser.mly"
                                ( Par (_1, _3) )
# 280 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 51 "ll_parser.mly"
                                ( With (_1, _3) )
# 288 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 52 "ll_parser.mly"
                                ( Plus (_1, _3) )
# 296 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 53 "ll_parser.mly"
                                ( Lollipop (_1, _3) )
# 304 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 54 "ll_parser.mly"
                                ( Ofcourse (_2) )
# 311 "ll_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 55 "ll_parser.mly"
                                ( Whynot (_2) )
# 318 "ll_parser.ml"
               : 'formula))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Sequent.sequent)
