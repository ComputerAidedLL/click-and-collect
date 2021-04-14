# 1 "ll_lexer.mll"
 
open Ll_parser        (* The type token is defined in ll_parser.mli *)

# 6 "ll_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\235\255\000\000\000\000\162\000\238\255\239\255\012\000\
    \241\255\246\000\074\001\243\255\158\001\244\255\245\255\246\255\
    \248\255\249\255\250\255\251\255\252\255\005\000\001\000\254\255\
    \255\255\253\255\000\000\132\001\240\255\242\255\247\255\002\000\
    \163\000\253\001\032\002\127\002\017\000\027\000\028\000\012\000\
    \009\000\211\002\039\003\013\000\164\000\018\000\029\000\031\000\
    \014\000\019\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\018\000\255\255\255\255\255\255\
    \255\255\013\000\018\000\255\255\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\008\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \018\000\018\000\018\000\012\000\255\255\018\000\018\000\255\255\
    \255\255\018\000\019\000\255\255\019\000\255\255\019\000\019\000\
    \255\255\255\255";
  Lexing.lex_default =
   "\255\255\000\000\255\255\255\255\255\255\000\000\000\000\255\255\
    \000\000\255\255\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\023\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\006\000\000\000\000\000\000\000\000\000\015\000\000\000\
    \019\000\018\000\016\000\014\000\020\000\007\000\025\000\000\000\
    \008\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\009\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\000\000\000\000\000\000\017\000\011\000\
    \000\000\004\000\012\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\010\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\028\000\022\000\000\000\000\000\000\000\
    \042\000\042\000\036\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\026\000\030\000\040\000\045\000\049\000\027\000\
    \000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\037\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\037\000\046\000\046\000\000\000\000\000\000\000\
    \000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \000\000\000\000\000\000\038\000\047\000\000\000\000\000\000\000\
    \000\000\032\000\032\000\044\000\000\000\000\000\003\000\002\000\
    \000\000\000\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\021\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\039\000\031\000\048\000\
    \001\000\043\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\032\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\031\000\031\000\043\000\000\000\
    \000\000\000\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\041\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\032\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \031\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\034\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\014\000\000\000\016\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\032\000\000\000\025\000\000\000\
    \029\000\011\000\000\000\000\000\031\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
    \000\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\035\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\032\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\000\000\000\000\000\000\000\000\000\000\031\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\032\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\031\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\009\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\044\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\043\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\022\000\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\000\000\255\255\255\255\255\255\
    \002\000\002\000\031\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\021\000\026\000\039\000\043\000\048\000\021\000\
    \255\255\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\040\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\036\000\045\000\049\000\255\255\255\255\255\255\
    \255\255\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \255\255\255\255\255\255\036\000\045\000\255\255\255\255\255\255\
    \255\255\004\000\032\000\044\000\255\255\255\255\000\000\000\000\
    \255\255\255\255\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\037\000\038\000\046\000\
    \000\000\047\000\255\255\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\009\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\255\255\255\255\255\255\255\255\255\255\255\255\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\010\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\004\000\032\000\044\000\255\255\
    \255\255\255\255\255\255\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\012\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \009\000\255\255\255\255\255\255\255\255\255\255\255\255\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\255\255\255\255\255\255\255\255\255\255\255\255\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\027\000\255\255\027\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\033\000\255\255\027\000\255\255\
    \027\000\027\000\255\255\255\255\010\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\255\255\
    \255\255\255\255\255\255\255\255\027\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\034\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\255\255\255\255\255\255\255\255\255\255\
    \012\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\035\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\255\255\255\255\255\255\255\255\255\255\033\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\041\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\034\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\042\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\035\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\041\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\042\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 5 "ll_lexer.mll"
                        ( token lexbuf )
# 323 "ll_lexer.ml"

  | 1 ->
# 6 "ll_lexer.mll"
                        ( EOL )
# 328 "ll_lexer.ml"

  | 2 ->
# 7 "ll_lexer.mll"
                          ( THESIS )
# 333 "ll_lexer.ml"

  | 3 ->
# 8 "ll_lexer.mll"
                        ( COMMA )
# 338 "ll_lexer.ml"

  | 4 ->
# 9 "ll_lexer.mll"
                        ( LPAREN )
# 343 "ll_lexer.ml"

  | 5 ->
# 10 "ll_lexer.mll"
                        ( RPAREN )
# 348 "ll_lexer.ml"

  | 6 ->
# 11 "ll_lexer.mll"
                        ( DUAL )
# 353 "ll_lexer.ml"

  | 7 ->
# 12 "ll_lexer.mll"
                          ( TENSOR )
# 358 "ll_lexer.ml"

  | 8 ->
# 13 "ll_lexer.mll"
                          ( PAR )
# 363 "ll_lexer.ml"

  | 9 ->
# 14 "ll_lexer.mll"
                        ( WITH )
# 368 "ll_lexer.ml"

  | 10 ->
# 15 "ll_lexer.mll"
                          ( PLUS )
# 373 "ll_lexer.ml"

  | 11 ->
# 16 "ll_lexer.mll"
                        ( ONE )
# 378 "ll_lexer.ml"

  | 12 ->
# 17 "ll_lexer.mll"
                          ( BOTTOM )
# 383 "ll_lexer.ml"

  | 13 ->
# 18 "ll_lexer.mll"
                          ( TOP )
# 388 "ll_lexer.ml"

  | 14 ->
# 19 "ll_lexer.mll"
                        ( ZERO )
# 393 "ll_lexer.ml"

  | 15 ->
# 20 "ll_lexer.mll"
                          ( LOLLIPOP )
# 398 "ll_lexer.ml"

  | 16 ->
# 21 "ll_lexer.mll"
                        ( OFCOURSE )
# 403 "ll_lexer.ml"

  | 17 ->
# 22 "ll_lexer.mll"
                        ( WHYNOT )
# 408 "ll_lexer.ml"

  | 18 ->
let
# 23 "ll_lexer.mll"
                                                        l
# 414 "ll_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 23 "ll_lexer.mll"
                                                           ( LITT l )
# 418 "ll_lexer.ml"

  | 19 ->
let
# 24 "ll_lexer.mll"
                                                                                                                                                                                                                                                                                       l
# 424 "ll_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 24 "ll_lexer.mll"
                                                                                                                                                                                                                                                                                          ( LITT l )
# 428 "ll_lexer.ml"

  | 20 ->
# 25 "ll_lexer.mll"
                        ( EOL )
# 433 "ll_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

