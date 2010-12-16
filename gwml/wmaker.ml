# 1 "wmaker.mll"
 
  (***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
  
  (* read the menu associated with WindowMaker ... 
  For now, we don't treat the #include directive, which should clearly be 
  handled by a generic preprocessor.
*)
  open Stddeco
open Gwml
open Options
open Themes
open Stdconfig
open Genlex

let wmaker_file = define_option ["wmaker_file"] "" filename_option "menu" 
let wmaker_path = define_option ["wmaker_path"] "" path_option []
  
type menu_tokens =
  STRING of string
| MENU | END
| EXEC of string
| LEGAL_PANEL
| INFO_PANEL
| WORKSPACE_MENU
| EXIT
| RESTART of string
| REFRESH
| EOF
| ARRANGE_ICONS
| SHUTDOWN
| SHOW_ALL
| HIDE_OTHERS
| SAVE_SESSION
| CLEAR_SESSION
| OPEN_MENU of string list
| WS_BACK of string

let maxstr = 200
let strbuf = String.create maxstr
let strlen = ref 0
  
  
# 54 "wmaker.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\231\255\232\255\000\000\002\000\001\000\002\000\002\000\
    \001\000\008\000\002\000\003\000\010\000\020\000\251\255\252\255\
    \253\255\254\255\000\000\255\255\012\000\006\000\250\255\023\000\
    \025\000\249\255\010\000\028\000\248\255\242\255\011\000\030\000\
    \013\000\017\000\014\000\034\000\037\000\247\255\037\000\027\000\
    \020\000\030\000\239\255\015\000\001\000\035\000\036\000\238\255\
    \044\000\019\000\033\000\048\000\035\000\036\000\047\000\042\000\
    \044\000\236\255\053\000\042\000\041\000\061\000\045\000\044\000\
    \246\255\060\000\047\000\059\000\241\255\062\000\054\000\039\000\
    \055\000\072\000\060\000\070\000\064\000\245\255\070\000\077\000\
    \067\000\049\000\065\000\081\000\069\000\079\000\073\000\244\255\
    \055\000\069\000\077\000\070\000\074\000\090\000\089\000\088\000\
    \063\000\082\000\091\000\083\000\077\000\243\255\097\000\099\000\
    \098\000\091\000\233\255\085\000\103\000\091\000\099\000\102\000\
    \077\000\100\000\107\000\096\000\098\000\094\000\240\255\110\000\
    \110\000\085\000\102\000\098\000\111\000\115\000\103\000\103\000\
    \237\255\118\000\123\000\107\000\095\000\108\000\123\000\110\000\
    \111\000\122\000\117\000\119\000\235\255\129\000\121\000\105\000\
    \124\000\133\000\125\000\119\000\234\255\004\000\253\255\254\255\
    \255\255\001\000\254\255\255\255\002\000\003\000\254\255\001\000\
    \255\255\006\000\253\255\254\255\255\255\007\000\253\255\254\255\
    \255\255\017\000\251\255\252\255\253\255\254\255\255\255\009\000\
    \253\255\254\255\255\255\205\000\244\255\238\000\003\001\247\255\
    \248\255\249\255\250\255\251\255\252\255\253\255\254\255\255\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\255\255\255\255\
    \255\255\255\255\024\000\255\255\255\255\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\003\000\255\255\002\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\010\000\009\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\000\000\255\255\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\150\000\000\000\000\000\
    \000\000\154\000\000\000\000\000\157\000\157\000\000\000\255\255\
    \000\000\162\000\000\000\000\000\000\000\166\000\000\000\000\000\
    \000\000\170\000\000\000\000\000\000\000\000\000\000\000\176\000\
    \000\000\000\000\000\000\181\000\000\000\181\000\181\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\017\000\017\000\155\000\000\000\152\000\151\000\000\000\
    \000\000\000\000\178\000\177\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\173\000\174\000\000\000\000\000\000\000\000\000\
    \017\000\000\000\016\000\014\000\152\000\000\000\000\000\015\000\
    \164\000\178\000\019\000\000\000\159\000\255\255\168\000\018\000\
    \160\000\173\000\000\000\171\000\000\000\000\000\000\000\000\000\
    \172\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\045\000\004\000\030\000\012\000\078\000\058\000\
    \005\000\009\000\119\000\031\000\008\000\013\000\129\000\003\000\
    \141\000\089\000\010\000\011\000\107\000\088\000\069\000\007\000\
    \024\000\020\000\021\000\022\000\027\000\025\000\029\000\028\000\
    \026\000\048\000\023\000\034\000\043\000\038\000\035\000\036\000\
    \037\000\039\000\040\000\041\000\042\000\032\000\044\000\046\000\
    \047\000\049\000\050\000\033\000\051\000\052\000\053\000\054\000\
    \055\000\056\000\057\000\059\000\065\000\061\000\062\000\063\000\
    \064\000\066\000\067\000\068\000\070\000\071\000\072\000\073\000\
    \060\000\074\000\075\000\076\000\077\000\079\000\080\000\081\000\
    \082\000\083\000\084\000\085\000\086\000\087\000\102\000\090\000\
    \091\000\092\000\093\000\094\000\095\000\096\000\097\000\098\000\
    \099\000\100\000\101\000\103\000\104\000\105\000\106\000\108\000\
    \109\000\110\000\111\000\112\000\113\000\114\000\115\000\116\000\
    \117\000\118\000\120\000\121\000\122\000\123\000\124\000\125\000\
    \126\000\127\000\128\000\130\000\131\000\132\000\133\000\134\000\
    \135\000\136\000\137\000\138\000\139\000\140\000\142\000\143\000\
    \144\000\145\000\146\000\147\000\148\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\191\000\191\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\191\000\000\000\190\000\
    \000\000\000\000\000\000\000\000\189\000\188\000\187\000\255\255\
    \255\255\184\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\155\000\158\000\255\255\151\000\000\000\163\000\167\000\
    \183\000\177\000\182\000\000\000\255\255\255\255\255\255\000\000\
    \255\255\174\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\255\255\255\255\255\255\255\255\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \186\000\000\000\185\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\180\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\153\000\255\255\149\000\149\000\255\255\
    \255\255\255\255\175\000\175\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\169\000\169\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\000\000\149\000\255\255\255\255\000\000\
    \161\000\175\000\018\000\255\255\156\000\157\000\165\000\000\000\
    \159\000\169\000\255\255\169\000\255\255\255\255\255\255\255\255\
    \169\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\044\000\000\000\011\000\000\000\008\000\010\000\
    \000\000\000\000\005\000\011\000\000\000\000\000\004\000\000\000\
    \003\000\007\000\000\000\000\000\006\000\007\000\009\000\000\000\
    \012\000\013\000\020\000\021\000\023\000\024\000\026\000\027\000\
    \023\000\030\000\012\000\031\000\032\000\033\000\034\000\035\000\
    \036\000\038\000\039\000\040\000\041\000\031\000\043\000\045\000\
    \046\000\048\000\049\000\031\000\050\000\051\000\052\000\053\000\
    \054\000\055\000\056\000\058\000\059\000\060\000\061\000\062\000\
    \063\000\065\000\066\000\067\000\069\000\070\000\071\000\072\000\
    \058\000\073\000\074\000\075\000\076\000\078\000\079\000\080\000\
    \081\000\082\000\083\000\084\000\085\000\086\000\088\000\089\000\
    \090\000\091\000\092\000\093\000\094\000\095\000\096\000\097\000\
    \098\000\099\000\100\000\102\000\103\000\104\000\105\000\107\000\
    \108\000\109\000\110\000\111\000\112\000\113\000\114\000\115\000\
    \116\000\117\000\119\000\120\000\121\000\122\000\123\000\124\000\
    \125\000\126\000\127\000\129\000\130\000\131\000\132\000\133\000\
    \134\000\135\000\136\000\137\000\138\000\139\000\141\000\142\000\
    \143\000\144\000\145\000\146\000\147\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\179\000\179\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\179\000\255\255\179\000\
    \255\255\255\255\255\255\255\255\179\000\179\000\179\000\181\000\
    \181\000\179\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\153\000\156\000\157\000\149\000\255\255\161\000\165\000\
    \179\000\175\000\179\000\255\255\182\000\182\000\181\000\255\255\
    \181\000\169\000\255\255\255\255\255\255\181\000\181\000\181\000\
    \255\255\255\255\181\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\182\000\255\255\182\000\255\255\255\255\
    \255\255\181\000\182\000\182\000\182\000\255\255\255\255\182\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\182\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \179\000\255\255\179\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\181\000\255\255\181\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\182\000\255\255\
    \182\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\179\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\181\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\182\000";
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

let rec menu lexbuf =
  __ocaml_lex_menu_rec lexbuf 0
and __ocaml_lex_menu_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 56 "wmaker.mll"
         ( comment lexbuf )
# 287 "wmaker.ml"

  | 1 ->
# 57 "wmaker.mll"
                      ( menu lexbuf )
# 292 "wmaker.ml"

  | 2 ->
# 58 "wmaker.mll"
        ( strlen := 0;  STRING (string lexbuf) )
# 297 "wmaker.ml"

  | 3 ->
# 59 "wmaker.mll"
         ( strlen := 0; STRING (string2 lexbuf) )
# 302 "wmaker.ml"

  | 4 ->
# 60 "wmaker.mll"
        ( strlen := 0; ignore (end_of_line lexbuf); menu lexbuf )
# 307 "wmaker.ml"

  | 5 ->
# 61 "wmaker.mll"
           ( MENU )
# 312 "wmaker.ml"

  | 6 ->
# 62 "wmaker.mll"
          ( END )
# 317 "wmaker.ml"

  | 7 ->
# 63 "wmaker.mll"
           ( strlen := 0; EXEC (end_of_line lexbuf)  )
# 322 "wmaker.ml"

  | 8 ->
# 64 "wmaker.mll"
             ( strlen := 0; EXEC (end_of_line lexbuf)  )
# 327 "wmaker.ml"

  | 9 ->
# 65 "wmaker.mll"
              ( strlen := 0; RESTART (end_of_line lexbuf)  )
# 332 "wmaker.ml"

  | 10 ->
# 66 "wmaker.mll"
                 ( INFO_PANEL )
# 337 "wmaker.ml"

  | 11 ->
# 67 "wmaker.mll"
                  ( LEGAL_PANEL )
# 342 "wmaker.ml"

  | 12 ->
# 68 "wmaker.mll"
                     ( WORKSPACE_MENU )
# 347 "wmaker.ml"

  | 13 ->
# 69 "wmaker.mll"
           ( EXIT )
# 352 "wmaker.ml"

  | 14 ->
# 70 "wmaker.mll"
              ( REFRESH )
# 357 "wmaker.ml"

  | 15 ->
# 71 "wmaker.mll"
                    ( ARRANGE_ICONS )
# 362 "wmaker.ml"

  | 16 ->
# 72 "wmaker.mll"
               ( SHUTDOWN )
# 367 "wmaker.ml"

  | 17 ->
# 73 "wmaker.mll"
               ( SHOW_ALL )
# 372 "wmaker.ml"

  | 18 ->
# 74 "wmaker.mll"
                  ( HIDE_OTHERS )
# 377 "wmaker.ml"

  | 19 ->
# 75 "wmaker.mll"
                   ( SAVE_SESSION )
# 382 "wmaker.ml"

  | 20 ->
# 76 "wmaker.mll"
                    ( CLEAR_SESSION )
# 387 "wmaker.ml"

  | 21 ->
# 77 "wmaker.mll"
                 ( strlen := 0; OPEN_MENU (begin_args lexbuf []) )
# 392 "wmaker.ml"

  | 22 ->
# 78 "wmaker.mll"
               ( WS_BACK (end_of_line lexbuf) )
# 397 "wmaker.ml"

  | 23 ->
# 79 "wmaker.mll"
        ( EOF )
# 402 "wmaker.ml"

  | 24 ->
# 80 "wmaker.mll"
      ( failwith "Syntax error in WindowMaker menu file" )
# 407 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_menu_rec lexbuf __ocaml_lex_state

and end_of_line lexbuf =
  __ocaml_lex_end_of_line_rec lexbuf 149
and __ocaml_lex_end_of_line_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 84 "wmaker.mll"
               ( end_of_line lexbuf )
# 418 "wmaker.ml"

  | 1 ->
# 85 "wmaker.mll"
                 ( "" )
# 423 "wmaker.ml"

  | 2 ->
# 86 "wmaker.mll"
      ( 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "end_of_line too long in WindowMaker menu file";
      in_end_of_line lexbuf )
# 433 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_end_of_line_rec lexbuf __ocaml_lex_state

and in_end_of_line lexbuf =
  __ocaml_lex_in_end_of_line_rec lexbuf 153
and __ocaml_lex_in_end_of_line_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 94 "wmaker.mll"
                ( String.sub strbuf 0 !strlen )
# 444 "wmaker.ml"

  | 1 ->
# 95 "wmaker.mll"
              ( 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "end_of_line too long in WindowMaker menu file";
      in_end_of_line lexbuf )
# 454 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_in_end_of_line_rec lexbuf __ocaml_lex_state

and comment lexbuf =
  __ocaml_lex_comment_rec lexbuf 156
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 103 "wmaker.mll"
         ( menu lexbuf )
# 465 "wmaker.ml"

  | 1 ->
# 104 "wmaker.mll"
        ( failwith "EOF in comment in WindowMaker menu file" )
# 470 "wmaker.ml"

  | 2 ->
# 105 "wmaker.mll"
        ( comment lexbuf )
# 475 "wmaker.ml"

  | 3 ->
# 106 "wmaker.mll"
             (  comment lexbuf )
# 480 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and string lexbuf =
  __ocaml_lex_string_rec lexbuf 161
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 109 "wmaker.mll"
        ( String.sub strbuf 0 !strlen )
# 491 "wmaker.ml"

  | 1 ->
# 110 "wmaker.mll"
        ( failwith "EOF in string in WindowMaker menu file" )
# 496 "wmaker.ml"

  | 2 ->
# 111 "wmaker.mll"
              ( 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "string too long in WindowMaker menu file";
      string lexbuf )
# 506 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and string2 lexbuf =
  __ocaml_lex_string2_rec lexbuf 165
and __ocaml_lex_string2_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 119 "wmaker.mll"
         ( String.sub strbuf 0 !strlen )
# 517 "wmaker.ml"

  | 1 ->
# 120 "wmaker.mll"
        ( failwith "EOF in string in WindowMaker menu file" )
# 522 "wmaker.ml"

  | 2 ->
# 121 "wmaker.mll"
               ( 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "string too long in WindowMaker menu file";
      string2 lexbuf )
# 532 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string2_rec lexbuf __ocaml_lex_state

and begin_args lexbuf =
  __ocaml_lex_begin_args_rec lexbuf 169
and __ocaml_lex_begin_args_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 129 "wmaker.mll"
              ( fun args -> List.rev args )
# 543 "wmaker.ml"

  | 1 ->
# 130 "wmaker.mll"
               ( begin_args lexbuf )
# 548 "wmaker.ml"

  | 2 ->
# 131 "wmaker.mll"
         ( fun args -> 
        strlen := 0; begin_args lexbuf ((string2 lexbuf)::args) )
# 554 "wmaker.ml"

  | 3 ->
# 133 "wmaker.mll"
        ( fun args -> 
        strlen := 0; begin_args lexbuf ((string lexbuf)::args) )
# 560 "wmaker.ml"

  | 4 ->
# 135 "wmaker.mll"
                                ( 
      strlen := 1;
      strbuf.[0] <- Lexing.lexeme_char lexbuf 0; 
      end_args lexbuf )
# 568 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_begin_args_rec lexbuf __ocaml_lex_state

and end_args lexbuf =
  __ocaml_lex_end_args_rec lexbuf 175
and __ocaml_lex_end_args_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 141 "wmaker.mll"
               ( fun args -> 
        begin_args lexbuf ((String.sub strbuf 0 !strlen) :: args) )
# 580 "wmaker.ml"

  | 1 ->
# 143 "wmaker.mll"
               ( fun args -> 
        List.rev ((String.sub strbuf 0 !strlen) :: args) )
# 586 "wmaker.ml"

  | 2 ->
# 145 "wmaker.mll"
                         ( 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "arg too long in WindowMaker menu file";
      end_args lexbuf )
# 596 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_end_args_rec lexbuf __ocaml_lex_state

and wmaker lexbuf =
  __ocaml_lex_wmaker_rec lexbuf 179
and __ocaml_lex_wmaker_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 153 "wmaker.mll"
                      ( wmaker lexbuf )
# 607 "wmaker.ml"

  | 1 ->
# 154 "wmaker.mll"
        ( strlen := 0; Some (String (string lexbuf)) )
# 612 "wmaker.ml"

  | 2 ->
# 155 "wmaker.mll"
         ( strlen := 0; Some (String (string2 lexbuf)) )
# 617 "wmaker.ml"

  | 3 ->
# 156 "wmaker.mll"
        ( Some (Kwd "(") )
# 622 "wmaker.ml"

  | 4 ->
# 157 "wmaker.mll"
        ( Some (Kwd ")") )
# 627 "wmaker.ml"

  | 5 ->
# 158 "wmaker.mll"
        ( Some (Kwd "{") )
# 632 "wmaker.ml"

  | 6 ->
# 159 "wmaker.mll"
        ( Some (Kwd "}") )
# 637 "wmaker.ml"

  | 7 ->
# 160 "wmaker.mll"
        ( Some (Kwd ",") )
# 642 "wmaker.ml"

  | 8 ->
# 161 "wmaker.mll"
        ( Some (Kwd ";") )
# 647 "wmaker.ml"

  | 9 ->
# 162 "wmaker.mll"
        ( Some (Kwd "=") )
# 652 "wmaker.ml"

  | 10 ->
# 163 "wmaker.mll"
                                                        ( 
      Some (Ident(Lexing.lexeme lexbuf)) )
# 658 "wmaker.ml"

  | 11 ->
# 165 "wmaker.mll"
        ( None )
# 663 "wmaker.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_wmaker_rec lexbuf __ocaml_lex_state

;;

# 167 "wmaker.mll"
 
(* Sorry, default is french :) *)
  
let lang = ref (try Sys.getenv "LANG" with _ -> "fr")
  
let _ = 
  if !!smart_install then begin
      add_to_path_check wmaker_path "/usr/share/WindowMaker";
      add_to_path_check wmaker_path "/usr/local/share/WindowMaker";
      add_to_path_check wmaker_path 
        (Filename.concat Utils.homedir "GNUstep/Library/WindowMaker");
    end
  
let find_menu name =
  try Utils.find_in_path !!wmaker_path (name ^ "." ^ !lang)
  with _ -> try Utils.find_in_path !!wmaker_path name with _ -> name

type menu = (string * action) list
and action = 
  Menu of menu
| Fun of (Gwml.wob -> unit)
| AMenu of (unit -> Stdconfig.menu)

let load_menu = ref (fun filename () -> [])

let fork_exec cmd = 
  let pid = Concur.Thread.fork () in
  if pid = 0 then begin
      Printf.printf "Commande:";
      for i = 0 to Array.length cmd - 1 do
        Printf.printf "%s " cmd.(i)
      done;
      print_newline ();
      Unix.execvp cmd.(0) cmd
    end
  
let cmd_on_file cmd file w =
  match cmd with
    None -> Stdconfig.commandw file w
  | Some cmd ->
      let cmd = Array.of_list (cmd @ [file]) in
      fork_exec cmd

type value = 
  Value of string
| List of value list

let install_theme_fwd = ref (fun name -> ())
  
let rec make_menu noext dirs list =
  match list with
    [menu_file] -> 
      !load_menu menu_file 
  | "|" :: _ -> Printf.printf "PIPE Menu"; print_newline (); 
      (fun _ -> [])
  | "-noext" :: tail -> make_menu true dirs tail
  | "WITH" :: "setstyle" :: _ ->
      end_make_menu (fun file w -> 

          try
            !install_theme_fwd file
          with e -> 
              Printf.printf "%s" (Utils.printexn e);
              print_newline ();
              raise e
      ) noext dirs
  | "WITH" :: "wmsetbg" :: "-u" :: tail ->
      end_make_menu (cmd_on_file (Some ("wmsetbg" :: tail))) noext dirs
  | "WITH" :: tail -> end_make_menu (cmd_on_file (Some tail)) noext dirs
  | "THEMES_DIR" :: tail -> 
      let theme_path = List.map (fun s ->
            Filename.concat s "Themes"
        ) !!wmaker_path in
      make_menu noext (theme_path @ dirs) tail
  | "BACKGROUNDS_DIR" :: tail -> 
      make_menu noext ("/usr/share/WindowMaker/Backgrounds" :: 
        (Filename.concat Utils.homedir "GNUstep/Library/WindowMaker/Backgrounds") ::
        dirs) tail
  | dir :: tail -> make_menu noext (dir :: dirs) tail
  | [] -> end_make_menu (cmd_on_file None) noext dirs

and end_make_menu cmd_on_file noext dirs = fun _ -> 
    let files = 
      List.flatten (List.map (fun dir -> 
            List.map (fun file ->
                file, Filename.concat dir file
            ) (Utils.list_dir_normal dir)) dirs) in
    let files = if noext then
        List.map (fun (name, file) -> 
            ((try Filename.chop_extension name with _ -> name), file)
        ) files else files in
    List.map (fun (name, file) ->
        if Utils.is_directory file && 
        (* this is REALLY bad. we should add an arg to check this ... *)
          (let len = String.length file in
            len < 8 || (String.sub file (len-7) 7 <> ".themed")
          ) then
          name, [submenu_item], ActiveMenu 
            (fun w -> end_make_menu cmd_on_file noext [file] ())
        else
          name, [], Function (cmd_on_file file)) files
    
let make_exec cmd = Stdconfig.commandw (cmd ^ " &")
let make_exit sw = Wob.exit_gwml ()
let make_restart cmd sw = 
  if cmd = "" then Wob.restart () else begin
      Wob.restart_cmd := [| cmd |]; Wob.restart ()
    end
    
let add_item name cmd items = (name,cmd)::items
(*  
let rec parse_top menus menu_name prev_items = parser
    [< 'STRING name; m = parse_action menus menu_name prev_items name >] -> 
    m
|   [< >] -> 
    (menu_name, Menu (List.rev prev_items))
  
and parse_action menus menu_name prev_items name = parser
|   [< 'END; s >] ->
    if name <> menu_name then failwith "Menus names clash";
    begin
      match menus with
        [] -> assert false
      | (menu_name, items) :: menus ->
          parse_top menus menu_name (add_item name (Menu 
              (List.rev prev_items)) items) s
    end
| [< 'MENU; s >] -> parse_top ((menu_name, prev_items) :: menus) name [] s
| [< 'EXEC cmd; s >] -> parse_top menus menu_name 
      (add_item name (Fun (make_exec cmd)) prev_items) s
| [< 'INFO_PANEL; s >] -> parse_top menus menu_name prev_items s
| [< 'LEGAL_PANEL; s >] -> parse_top menus menu_name prev_items s
| [< 'WORKSPACE_MENU; s >] -> parse_top menus menu_name prev_items s
| [< 'RESTART wm; s >] -> parse_top menus menu_name 
      (add_item name (Fun (make_restart wm)) prev_items) s
| [< 'EXIT; s >] -> parse_top menus menu_name 
      (add_item name (Fun make_exit) prev_items) s
| [< 'REFRESH; s >] -> parse_top menus menu_name prev_items s
| [< 'ARRANGE_ICONS; s >] -> parse_top menus menu_name prev_items s
| [< 'SHUTDOWN; s >] -> parse_top menus menu_name prev_items s
| [< 'SHOW_ALL; s >] -> parse_top menus menu_name prev_items s
| [< 'HIDE_OTHERS; s >] -> parse_top menus menu_name prev_items s
| [< 'SAVE_SESSION; s >] -> parse_top menus menu_name prev_items s
| [< 'CLEAR_SESSION; s >] -> parse_top menus menu_name prev_items s
| [< 'OPEN_MENU menu; s >] -> 
    parse_top menus menu_name 
      (add_item name (AMenu (make_menu false [] menu)) prev_items) s
| [< 'WS_BACK color; s >] -> parse_top menus menu_name prev_items s    

  
let parse_menu lexbuf =
  let stream = Stream.from (fun n -> 
        match menu lexbuf with
          EOF -> None
        | t ->  
            Some t) in
  parse_top [] "WMAKER_MENU" [] stream
        
let open_menu name =
  let filename = find_menu name in
  Cpp.path := !!wmaker_path;
  let lexbuf = Cpp.preprocess filename in
  let m = 
    try parse_menu lexbuf with e -> 
        let location = Cpp.abort () in
        Printf.printf "%s :" location;
        raise e in
  m
*)
let open_menu n = ()
let init () = ()

let rec make_menu menu () =
  List.map (fun (name, action) ->
      match action with
        Menu m -> name, [submenu_item], ActiveMenu (
            fun w -> make_menu m ())
      | Fun f -> name, [], Function f
      | AMenu f -> name, [submenu_item], 
          ActiveMenu (fun sw -> f ())
  ) menu
(*
let wmaker_menu name =
  try
    let m = open_menu name in
    match m with
    | _, Menu [ _, Menu menu] -> make_menu menu ()
    | _, Menu menu -> make_menu menu ()
    | _ -> []
  with e -> 
      Printf.printf "Wmaker: %s" (Utils.printexn e);
      print_newline ();
      []

let _ = load_menu := (fun filename unit -> wmaker_menu filename)
*)  
open Genlex
  
let wmaker_lexer = make_lexer [ "=" ; "(" ; ")"; "{"; "}"; ";"; "," ]
(*
let rec parse_module = parser
| [< 'Kwd "{"; v = parse_values; eof = parse_module >] -> 
    v :: eof
| [< >] -> []

and parse_values = parser
| [< 'Kwd "}" >] -> []
| [< id = parse_id; 'Kwd "="; v = parse_value; 'Kwd ";"; eov = parse_values >] ->
    (id,v) :: eov
    
and parse_value = parser
| [< 'Ident s >] -> Value s
| [< 'String s >] -> Value s
| [< 'Int i >] -> Value (string_of_int i)
| [< 'Float f >] -> Value (string_of_float f)
| [< 'Char c >] -> Value (let s = String.create 1 in s.[0] <- c; s) 
| [< 'Kwd "("; v = parse_list >] -> List v
    
and parse_id = parser
    [< 'Ident s >] -> s
|   [< 'String s >] -> s

and parse_list = parser
    [< 'Kwd ","; v = parse_list >] -> v
|   [< v = parse_value; t = parse_list >] -> v :: t
|   [< 'Kwd ")" >] -> []

let load_options  filename =
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let v = Stream.from (fun _ -> wmaker lexbuf) in
    try
      let m = parse_module v in
      close_in ic;
      m
    with e ->
        Printf.printf "At pos %d:" (Lexing.lexeme_start lexbuf);
        print_newline ();
        raise e
  with e -> close_in ic; raise e

let load_soundset name =
  let name = Filename.basename name in
  let path = List.map (fun f -> Filename.concat f "SoundSets") !!wmaker_path in
  let filename = Utils.find_in_path path name in
  let options = load_options filename in
  let soundpath = List.map (fun f -> Filename.concat f "Sounds") !!wmaker_path
  in
  List.iter (fun options ->
      List.iter (fun (name, value) ->
          try
            let value = match value with Value v -> v | _ -> raise Not_found in
            let option = match String.lowercase name with
                "appexit" -> exit_sound
              | "appstart" -> start_sound
              | "hide" -> hide_sound
              | "iconify" -> iconify_sound
              | "maximize" -> maximize_sound
              | "unmaximize" -> unmaximize_sound
              | "shade" -> shade_sound
              | "startup" -> startup_sound
              | "unhide" -> unhide_sound
              | "deiconify" -> deiconify_sound
              | "unshade" -> unshade_sound
              | _ -> raise Not_found
            in 
            let filename = Utils.find_in_path soundpath value in
            option =:= filename
          with _ -> ()
      ) options
  ) options
*)  
let find_image imagepath name =
  try
    Utils.find_in_path imagepath name
  with Not_found ->
  (* Can't always trust absolute paths *)
      Utils.find_in_path imagepath (Filename.basename name)
  
let set_back opt_bg opt_im imagepath value =
  match value with
    List [Value "spixmap"; Value name; _ ] ->
      let filename = find_image imagepath name in
      opt_im =:= ScaleImage filename
  | List [Value "tpixmap"; Value name; _ ] ->
      let filename = find_image imagepath name in
      opt_im =:= TileImage filename
  | List [Value "cpixmap"; Value name; _ ] ->
      let filename = find_image imagepath name in
      opt_im =:= TileImage filename
  | List [Value "mvgradient"; Value c1; Value _ ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "mhgradient"; Value c1; Value _ ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "mdgradient"; Value c1; Value _ ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "hgradient"; Value c1 ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "vgradient"; Value c1 ; Value c2 ] -> 
      let filename = Gradients.make_vgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "dgradient"; Value c1 ; Value c2 ] -> 
      let filename = Gradients.make_dgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | _ -> ()
(*
let install_theme name =
  Printf.printf "Install theme %s" name; print_newline ();
  Wob.reset_image_cache ();
  let path = List.map (fun f -> Filename.concat f "Themes") !!wmaker_path in
  let filename = Utils.find_in_path path name in
  let imagepath = List.map (fun s ->
        Filename.concat s "Pixmaps"
    ) !!wmaker_path in
  let imagepath = (List.map (fun s ->
          Filename.concat s "Backgrounds"
      ) !!wmaker_path) @ imagepath in
  let filename, imagepath = if Utils.is_directory filename then 
      Filename.concat filename "style", filename :: imagepath
    else filename, imagepath in
  let options = load_options filename in
  List.iter (fun options ->
      List.iter (fun (name,option) ->
          try
            match String.lowercase name, option with
              "workspaceback", back -> 
                set_back root_background root_image imagepath back
            | "ftitleback", back ->
                set_back active_background active_image imagepath back
            | "utitleback", back ->
                set_back title_background title_image imagepath back
            | "ftitlecolor", Value color ->
                active_foreground =:= color
            | "utitlecolor", Value color ->
                title_foreground =:= color
            | "menutitlecolor", Value color ->
                menu_title_foreground =:= color;
                iconMgr_title_foreground =:= color;
            | "menutextcolor", Value color ->
                menu_foreground =:= color;
                iconMgr_foreground =:= color;
            | "menutitleback", back ->
                set_back menu_title_background menu_title_image 
                  imagepath back;
                set_back iconMgr_title_background iconMgr_title_image 
                  imagepath back;
                
            | "menutextback", back ->
                set_back menu_background menu_image imagepath back;
                set_back iconMgr_background iconMgr_image imagepath back;
                
            | "windowtitlefont", Value font ->
                title_font =:= font;
                active_font =:= font;
            | "menutextfont", Value font ->
                menu_font =:= font;
                menu_hilite_font =:= font;
                iconMgr_font =:= font;
            | "menutitlefont", Value font ->
                menu_title_font =:= font;
                iconMgr_title_font =:= font;
            | "titlejustify", Value s ->
                title_justification =:= (match String.lowercase s with
                    "center" -> Center
                  | "left" -> Left
                  | "right" -> Right
                  | _ -> !!title_justification)
            | "highlighttextcolor", Value color ->
                menu_hilite_foreground =:= color;
                iconMgr_active_foreground =:= color;
            | "highlightcolor", Value color -> 
                menu_hilite_image =:= NoImage;
                menu_hilite_background =:= color;
                iconMgr_active_image =:= NoImage;
                iconMgr_active_background =:= color
            | _ -> ()
          with e ->
              Printf.printf "Error %s with option %s"
                (Utils.printexn e) name;
              print_newline ();
            (*
          IconTitleFont = "-*-helvetica-medium-r-normal-*-8-*-*-*-*-*-*-*";
          ClipTitleFont = "-*-helvetica-bold-r-normal-*-10-*-*-*-*-*-*-*";
          DisplayFont = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-*-*";
          HighlightColor = white;
          HighlightTextColor = black;
          ClipTitleColor = black;
          CClipTitleColor = black;
          FTitleColor = white;
          PTitleColor = white;
          UTitleColor = gray20;
          FTitleBack = (tpixmap, Vader_titlebar_F.tif, black);
          PTitleBack = (tpixmap, Vader_titlebar_UF.tif, black);
          UTitleBack = (tpixmap, Vader_titlebar_UF.tif, black);
          MenuTitleColor = white;
          MenuTextColor = white;
          MenuDisabledColor = gray60;
          MenuTitleBack = (hgradient, "rgb:30/32/3e", black);
          MenuTextBack = (hgradient, "rgb:50/5a/5e", "rgb:20/2a/2e");
          IconBack = (spixmap, Vader_Button.tif, black);
          IconTitleColor = black;
          IconTitleBack = black;
              *)      
      ) options
  ) options;
  Array.iter (fun s ->
      let sw = s#wob in
      (* reset menus *)
      (!menu_manager)#reset sw;      
      (* reset the clients *)
      List.iter (fun (c,w) ->
          let tw = w.w_top in
          let focus = Wob.getenv tw focus_var in
          update_focus tw.w_oo focus
      ) (list_clients sw)) !screens;
  load_soundset name
  
let _ = install_theme_fwd := install_theme

let available_themes () = 
  let themes_path = List.map (fun s ->
        Filename.concat s "Themes"
    ) !!wmaker_path in
  List.flatten (List.map (fun dir -> Utils.list_dir_normal dir) themes_path)

let menu () =
  List.map (fun name ->
      name, [], Function (fun w -> install_theme name)) (available_themes ())

  
let menu _ = end_make_menu (fun file w -> 
      try
        install_theme file
      with e -> 
          Printf.printf "%s" (Utils.printexn e);
          print_newline ();
          raise e
  ) true (List.map (fun f -> Filename.concat f "Themes") !!wmaker_path) ()
*) 
let install_theme lex = ()
let load_soundset string = ()
let wmaker_menu string = []
let menu m = []
  
# 1119 "wmaker.ml"
