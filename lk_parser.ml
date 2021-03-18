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

open Parsing;;
let _ = parse_error;;
# 2 "lk_parser.mly"
open Classical_logic
# 20 "lk_parser.ml"
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
    0|]

let yytransl_block = [|
  259 (* LITT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\003\000\004\000\001\000\003\000\001\000\
\001\000\001\000\003\000\002\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\010\000\000\000\001\000\000\000\008\000\
\009\000\016\000\000\000\000\000\002\000\000\000\000\000\012\000\
\000\000\003\000\000\000\000\000\000\000\000\000\004\000\011\000\
\000\000\007\000\013\000\014\000\000\000\005\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000"

let yysindex = "\004\000\
\020\255\000\000\033\255\000\000\038\255\000\000\038\255\000\000\
\000\000\000\000\045\255\026\255\000\000\251\254\254\254\000\000\
\038\255\000\000\038\255\038\255\038\255\038\255\000\000\000\000\
\005\255\000\000\000\000\000\000\024\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\255\000\000"

let yygindex = "\000\000\
\000\000\001\000\251\255"

let yytablesize = 52
let yytable = "\015\000\
\023\000\016\000\024\000\014\000\001\000\020\000\021\000\015\000\
\015\000\022\000\030\000\015\000\015\000\000\000\027\000\028\000\
\029\000\025\000\015\000\026\000\003\000\000\000\004\000\005\000\
\000\000\006\000\007\000\019\000\000\000\008\000\009\000\020\000\
\021\000\020\000\021\000\004\000\005\000\022\000\013\000\007\000\
\004\000\005\000\008\000\009\000\007\000\017\000\006\000\008\000\
\009\000\000\000\018\000\006\000"

let yycheck = "\005\000\
\006\001\007\000\005\001\003\000\001\000\008\001\009\001\001\001\
\002\001\012\001\006\001\005\001\006\001\255\255\020\000\021\000\
\022\000\017\000\012\001\019\000\001\001\255\255\003\001\004\001\
\255\255\006\001\007\001\002\001\255\255\010\001\011\001\008\001\
\009\001\008\001\009\001\003\001\004\001\012\001\006\001\007\001\
\003\001\004\001\010\001\011\001\007\001\001\001\001\001\010\001\
\011\001\255\255\006\001\006\001"

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
  "

let yynames_block = "\
  LITT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "lk_parser.mly"
                                          ( ([], []) )
# 114 "lk_parser.ml"
               : Classical_logic.formula list * Classical_logic.formula list))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "lk_parser.mly"
                                          ( ([], []) )
# 120 "lk_parser.ml"
               : Classical_logic.formula list * Classical_logic.formula list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 28 "lk_parser.mly"
                                          ( ([], _1) )
# 127 "lk_parser.ml"
               : Classical_logic.formula list * Classical_logic.formula list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 29 "lk_parser.mly"
                                          ( ([], _2) )
# 134 "lk_parser.ml"
               : Classical_logic.formula list * Classical_logic.formula list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formulalist) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formulalist) in
    Obj.repr(
# 30 "lk_parser.mly"
                                          ( (_1, _3) )
# 142 "lk_parser.ml"
               : Classical_logic.formula list * Classical_logic.formula list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 34 "lk_parser.mly"
                                 ( [_1] )
# 149 "lk_parser.ml"
               : 'formulalist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formulalist) in
    Obj.repr(
# 35 "lk_parser.mly"
                                 ( _1 :: _3 )
# 157 "lk_parser.ml"
               : 'formulalist))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "lk_parser.mly"
                                ( True )
# 163 "lk_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "lk_parser.mly"
                                ( False )
# 169 "lk_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "lk_parser.mly"
                                ( Litt _1 )
# 176 "lk_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 42 "lk_parser.mly"
                                ( _2 )
# 183 "lk_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 43 "lk_parser.mly"
                                ( Neg (_2) )
# 190 "lk_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 44 "lk_parser.mly"
                                ( Conj (_1, _3) )
# 198 "lk_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 45 "lk_parser.mly"
                                ( Disj (_1, _3) )
# 206 "lk_parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 46 "lk_parser.mly"
                                ( Impl (_1, _3) )
# 214 "lk_parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Classical_logic.formula list * Classical_logic.formula list)
