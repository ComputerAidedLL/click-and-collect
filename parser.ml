type token =
  | LITT of (string)
  | NEG
  | CONJ
  | DISJ
  | IMPL
  | LPAREN
  | RPAREN
  | COMMA
  | THESIS
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Formula
# 18 "parser.ml"
let yytransl_const = [|
  258 (* NEG *);
  259 (* CONJ *);
  260 (* DISJ *);
  261 (* IMPL *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* COMMA *);
  265 (* THESIS *);
  266 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* LITT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\003\000\004\000\001\000\003\000\001\000\
\003\000\002\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\001\000\014\000\
\000\000\000\000\010\000\000\000\002\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\009\000\004\000\000\000\011\000\
\012\000\000\000\007\000\005\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000"

let yysindex = "\015\000\
\017\255\000\000\000\000\032\255\032\255\019\255\000\000\000\000\
\255\254\027\255\000\000\036\255\000\000\012\255\032\255\000\000\
\032\255\032\255\032\255\032\255\000\000\000\000\014\255\000\000\
\000\000\007\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\253\254\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\252\255"

let yytablesize = 45
let yytable = "\011\000\
\012\000\013\000\014\000\013\000\013\000\013\000\013\000\015\000\
\016\000\017\000\018\000\023\000\024\000\025\000\026\000\001\000\
\027\000\003\000\004\000\003\000\004\000\022\000\005\000\028\000\
\005\000\006\000\007\000\000\000\013\000\017\000\018\000\019\000\
\003\000\004\000\020\000\000\000\000\000\005\000\017\000\018\000\
\019\000\000\000\021\000\006\000\006\000"

let yycheck = "\004\000\
\005\000\005\001\006\000\007\001\008\001\009\001\010\001\009\001\
\010\001\003\001\004\001\015\000\017\000\018\000\019\000\001\000\
\020\000\001\001\002\001\001\001\002\001\010\001\006\001\010\001\
\006\001\009\001\010\001\255\255\010\001\003\001\004\001\005\001\
\001\001\002\001\008\001\255\255\255\255\006\001\003\001\004\001\
\005\001\255\255\007\001\009\001\010\001"

let yynames_const = "\
  NEG\000\
  CONJ\000\
  DISJ\000\
  IMPL\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  THESIS\000\
  EOL\000\
  "

let yynames_block = "\
  LITT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "parser.mly"
                                       ( [], [] )
# 106 "parser.ml"
               : Formula.f list * Formula.f list))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "parser.mly"
                                       ( [], [] )
# 112 "parser.ml"
               : Formula.f list * Formula.f list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 23 "parser.mly"
                                       ( [], _1 )
# 119 "parser.ml"
               : Formula.f list * Formula.f list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 24 "parser.mly"
                                       ( [], _2 )
# 126 "parser.ml"
               : Formula.f list * Formula.f list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formulalist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 25 "parser.mly"
                                       ( _1, _3 )
# 134 "parser.ml"
               : Formula.f list * Formula.f list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 28 "parser.mly"
                                ( [_1] )
# 141 "parser.ml"
               : 'formulalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formulalist) in
    Obj.repr(
# 29 "parser.mly"
                                (  _1 :: _3 )
# 149 "parser.ml"
               : 'formulalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 32 "parser.mly"
                             ( Litt _1 )
# 156 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 33 "parser.mly"
                                ( _2 )
# 163 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 34 "parser.mly"
                    ( Neg (_2) )
# 170 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 35 "parser.mly"
                                ( Conj (_1, _3) )
# 178 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 36 "parser.mly"
                                ( Disj (_1, _3) )
# 186 "parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 37 "parser.mly"
                                ( Impl (_1, _3) )
# 194 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Formula.f list * Formula.f list)
