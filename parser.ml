type token =
  | LITT of (string)
  | NEG
  | CONJ
  | DISJ
  | IMPL
  | LPAREN
  | RPAREN
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Formula
# 16 "parser.ml"
let yytransl_const = [|
  258 (* NEG *);
  259 (* CONJ *);
  260 (* DISJ *);
  261 (* IMPL *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* LITT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\002\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\000\000\008\000\000\000\004\000\
\000\000\000\000\000\000\000\000\001\000\003\000\005\000\006\000\
\000\000"

let yydgoto = "\002\000\
\006\000\007\000"

let yysindex = "\002\000\
\011\255\000\000\000\000\011\255\011\255\000\000\006\255\000\000\
\015\255\011\255\011\255\011\255\000\000\000\000\000\000\000\000\
\012\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\253\254"

let yygindex = "\000\000\
\000\000\252\255"

let yytablesize = 22
let yytable = "\008\000\
\009\000\007\000\001\000\007\000\007\000\015\000\016\000\017\000\
\010\000\011\000\012\000\003\000\004\000\013\000\010\000\011\000\
\005\000\010\000\011\000\012\000\000\000\014\000"

let yycheck = "\004\000\
\005\000\005\001\001\000\007\001\008\001\010\000\011\000\012\000\
\003\001\004\001\005\001\001\001\002\001\008\001\003\001\004\001\
\006\001\003\001\004\001\005\001\255\255\007\001"

let yynames_const = "\
  NEG\000\
  CONJ\000\
  DISJ\000\
  IMPL\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  LITT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 17 "parser.mly"
                            ( _1 )
# 90 "parser.ml"
               : Formula.f))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 20 "parser.mly"
                            ( Litt _1 )
# 97 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 21 "parser.mly"
                            ( _2 )
# 104 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 22 "parser.mly"
                ( Neg (_2) )
# 111 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                            ( Conj (_1, _3) )
# 119 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                            ( Disj (_1, _3) )
# 127 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                            ( Impl (_1, _3) )
# 135 "parser.ml"
               : 'expr))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Formula.f)
