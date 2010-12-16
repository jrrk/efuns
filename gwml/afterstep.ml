# 1 "afterstep.mll"
 
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

(* Load a theme from Afterstep *)
  open Wob  
  open Gwml
  open Options
  open Themes
  open Stdconfig
  open Stddeco
    
  type look_tokens = 
    STYLE of string list
  | FONT of string list
  | FORECOLOR of string list
  | BACKCOLOR of string list
  | MAXCOLORS of string list
  | INHERIT of string list
  | ENDSTYLE
  | BACKPIXMAP of string list
  | TEXTSTYLE of string list
  | BUTTONPIXMAP of string list
  | MARROWPIXMAP of string list
  | MENUPINON of string list
  | WINDOWFONT of string list
  | ICONFONT of string list
  | TITLEBUTTON of string list
  
  | DEFAULTSTYLE of string list
  | FWINDOWSTYLE of string list
  | UWINDOWSTYLE of string list
  | SWINDOWSTYLE of string list
  | MENUITEMSTYLE of string list
  | MENUTITLESTYLE of string list
  | MENUHILITESTYLE of string list
  | MENUSTIPPLESTYLE of string list  
    
  | EOF
    
let maxstr = 200
let strbuf = String.create maxstr
let strlen = ref 0


# 56 "afterstep.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\228\255\229\255\001\000\002\000\000\000\000\000\001\000\
    \001\000\000\000\000\000\003\000\006\000\255\255\001\000\001\000\
    \001\000\000\000\003\000\000\000\000\000\000\000\003\000\254\255\
    \000\000\001\000\001\000\002\000\004\000\004\000\250\255\008\000\
    \012\000\005\000\000\000\020\000\008\000\020\000\001\000\018\000\
    \244\255\014\000\060\000\018\000\030\000\031\000\021\000\033\000\
    \029\000\000\000\031\000\243\255\041\000\036\000\008\000\030\000\
    \026\000\040\000\048\000\233\255\034\000\043\000\051\000\009\000\
    \037\000\033\000\047\000\055\000\232\255\049\000\053\000\043\000\
    \059\000\010\000\045\000\041\000\055\000\063\000\231\255\060\000\
    \054\000\055\000\060\000\068\000\011\000\054\000\050\000\064\000\
    \072\000\230\255\069\000\065\000\075\000\061\000\253\255\002\000\
    \067\000\072\000\070\000\068\000\252\255\073\000\000\000\073\000\
    \066\000\012\000\070\000\066\000\080\000\088\000\236\255\074\000\
    \092\000\085\000\126\000\089\000\084\000\088\000\086\000\084\000\
    \251\255\079\000\091\000\104\000\090\000\247\255\087\000\093\000\
    \095\000\001\000\102\000\088\000\100\000\113\000\099\000\245\255\
    \101\000\109\000\113\000\101\000\111\000\101\000\249\255\108\000\
    \002\000\108\000\110\000\105\000\241\255\101\000\013\000\107\000\
    \103\000\117\000\125\000\248\255\111\000\108\000\113\000\147\000\
    \115\000\111\000\125\000\133\000\246\255\127\000\135\000\008\000\
    \120\000\122\000\123\000\129\000\131\000\159\000\131\000\147\000\
    \124\000\138\000\147\000\240\255\150\000\145\000\141\000\149\000\
    \239\255\143\000\154\000\144\000\139\000\005\000\148\000\150\000\
    \145\000\242\255\160\000\166\000\147\000\157\000\150\000\184\000\
    \152\000\148\000\162\000\170\000\237\255\167\000\163\000\174\000\
    \164\000\157\000\194\000\162\000\158\000\172\000\180\000\235\255\
    \177\000\173\000\184\000\174\000\167\000\204\000\172\000\168\000\
    \182\000\190\000\234\255\042\000\253\255\254\255\255\255\036\000\
    \037\000\255\255\029\001\251\255\252\255\034\001\254\255\255\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\255\255\000\000\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255\002\000\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\014\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\228\000\000\000\000\000\000\000\232\000\
    \232\000\000\000\237\000\000\000\000\000\237\000\000\000\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\013\000\013\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \013\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\233\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\010\000\025\000\005\000\096\000\011\000\016\000\
    \145\000\009\000\168\000\190\000\230\000\012\000\149\000\050\000\
    \035\000\130\000\000\000\003\000\007\000\004\000\019\000\006\000\
    \216\000\205\000\090\000\055\000\064\000\074\000\085\000\106\000\
    \151\000\112\000\039\000\136\000\103\000\194\000\157\000\017\000\
    \023\000\185\000\156\000\015\000\022\000\027\000\137\000\041\000\
    \026\000\028\000\091\000\031\000\020\000\111\000\029\000\030\000\
    \024\000\021\000\032\000\033\000\034\000\036\000\008\000\018\000\
    \037\000\038\000\040\000\042\000\044\000\046\000\079\000\069\000\
    \060\000\052\000\048\000\049\000\047\000\051\000\053\000\043\000\
    \045\000\054\000\056\000\057\000\058\000\059\000\061\000\062\000\
    \063\000\065\000\066\000\067\000\068\000\070\000\071\000\072\000\
    \073\000\075\000\076\000\077\000\078\000\080\000\081\000\082\000\
    \083\000\084\000\086\000\087\000\088\000\089\000\101\000\093\000\
    \095\000\094\000\097\000\092\000\098\000\099\000\100\000\102\000\
    \104\000\105\000\107\000\108\000\109\000\110\000\126\000\113\000\
    \114\000\116\000\121\000\117\000\118\000\119\000\120\000\122\000\
    \123\000\124\000\125\000\127\000\128\000\129\000\115\000\131\000\
    \132\000\133\000\134\000\135\000\143\000\138\000\139\000\140\000\
    \141\000\142\000\144\000\146\000\147\000\148\000\150\000\152\000\
    \153\000\154\000\155\000\165\000\158\000\159\000\160\000\161\000\
    \162\000\163\000\164\000\166\000\167\000\169\000\170\000\171\000\
    \172\000\173\000\174\000\175\000\180\000\177\000\178\000\176\000\
    \179\000\181\000\182\000\183\000\184\000\186\000\187\000\188\000\
    \002\000\255\255\189\000\191\000\192\000\193\000\195\000\196\000\
    \197\000\198\000\199\000\200\000\201\000\202\000\203\000\204\000\
    \206\000\207\000\208\000\209\000\210\000\211\000\212\000\213\000\
    \214\000\215\000\217\000\218\000\219\000\220\000\221\000\222\000\
    \223\000\224\000\225\000\226\000\233\000\255\255\239\000\236\000\
    \000\000\000\000\229\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\239\000\000\000\238\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\235\000\000\000\000\000\
    \000\000\000\000\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\014\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\231\000\232\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\024\000\000\000\095\000\000\000\012\000\
    \144\000\000\000\167\000\189\000\227\000\000\000\008\000\049\000\
    \034\000\129\000\255\255\000\000\000\000\000\000\018\000\000\000\
    \003\000\004\000\011\000\054\000\063\000\073\000\084\000\105\000\
    \150\000\010\000\038\000\009\000\102\000\005\000\007\000\012\000\
    \022\000\006\000\007\000\012\000\021\000\026\000\009\000\015\000\
    \025\000\027\000\011\000\016\000\019\000\010\000\028\000\029\000\
    \017\000\020\000\031\000\032\000\033\000\035\000\000\000\012\000\
    \036\000\037\000\039\000\041\000\042\000\042\000\043\000\044\000\
    \045\000\046\000\047\000\048\000\042\000\050\000\052\000\042\000\
    \042\000\053\000\055\000\056\000\057\000\058\000\060\000\061\000\
    \062\000\064\000\065\000\066\000\067\000\069\000\070\000\071\000\
    \072\000\074\000\075\000\076\000\077\000\079\000\080\000\081\000\
    \082\000\083\000\085\000\086\000\087\000\088\000\090\000\091\000\
    \092\000\093\000\096\000\091\000\097\000\098\000\099\000\101\000\
    \103\000\104\000\106\000\107\000\108\000\109\000\111\000\112\000\
    \113\000\114\000\115\000\116\000\117\000\118\000\119\000\121\000\
    \122\000\123\000\124\000\126\000\127\000\128\000\114\000\130\000\
    \131\000\132\000\133\000\134\000\136\000\137\000\138\000\139\000\
    \140\000\141\000\143\000\145\000\146\000\147\000\149\000\151\000\
    \152\000\153\000\154\000\156\000\157\000\158\000\159\000\160\000\
    \161\000\162\000\163\000\165\000\166\000\168\000\169\000\170\000\
    \171\000\172\000\173\000\174\000\175\000\176\000\177\000\174\000\
    \178\000\180\000\181\000\182\000\183\000\185\000\186\000\187\000\
    \000\000\014\000\188\000\190\000\191\000\192\000\194\000\195\000\
    \196\000\197\000\198\000\199\000\200\000\201\000\202\000\203\000\
    \205\000\206\000\207\000\208\000\209\000\210\000\211\000\212\000\
    \213\000\214\000\216\000\217\000\218\000\219\000\220\000\221\000\
    \222\000\223\000\224\000\225\000\231\000\232\000\234\000\234\000\
    \255\255\255\255\227\000\237\000\237\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\234\000\255\255\234\000\
    \255\255\255\255\237\000\255\255\237\000\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\234\000\255\255\255\255\
    \255\255\255\255\237\000";
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

let rec look lexbuf =
  __ocaml_lex_look_rec lexbuf 0
and __ocaml_lex_look_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 57 "afterstep.mll"
                       ( look lexbuf )
# 315 "afterstep.ml"

  | 1 ->
# 59 "afterstep.mll"
              ( STYLE (args lexbuf 1) )
# 320 "afterstep.ml"

  | 2 ->
# 60 "afterstep.mll"
                 ( FONT (args lexbuf 1) )
# 325 "afterstep.ml"

  | 3 ->
# 61 "afterstep.mll"
                 ( FORECOLOR (args lexbuf 1) )
# 330 "afterstep.ml"

  | 4 ->
# 62 "afterstep.mll"
                 ( BACKCOLOR (args lexbuf 1) )
# 335 "afterstep.ml"

  | 5 ->
# 63 "afterstep.mll"
                 ( MAXCOLORS (args lexbuf 1) )
# 340 "afterstep.ml"

  | 6 ->
# 64 "afterstep.mll"
                 ( INHERIT (args lexbuf 1) )
# 345 "afterstep.ml"

  | 7 ->
# 65 "afterstep.mll"
                 ( ENDSTYLE )
# 350 "afterstep.ml"

  | 8 ->
# 66 "afterstep.mll"
                 ( BACKPIXMAP (args lexbuf 2) )
# 355 "afterstep.ml"

  | 9 ->
# 67 "afterstep.mll"
                 ( TEXTSTYLE (args lexbuf 1) )
# 360 "afterstep.ml"

  | 10 ->
# 68 "afterstep.mll"
                   ( BUTTONPIXMAP (args lexbuf 1) )
# 365 "afterstep.ml"

  | 11 ->
# 69 "afterstep.mll"
                   ( MARROWPIXMAP (args lexbuf 1) )
# 370 "afterstep.ml"

  | 12 ->
# 70 "afterstep.mll"
                ( MENUPINON (args lexbuf 1) )
# 375 "afterstep.ml"

  | 13 ->
# 71 "afterstep.mll"
                 ( WINDOWFONT(args lexbuf 1) )
# 380 "afterstep.ml"

  | 14 ->
# 72 "afterstep.mll"
                 ( ICONFONT(args lexbuf 1) )
# 385 "afterstep.ml"

  | 15 ->
# 73 "afterstep.mll"
                        ( end_of_line lexbuf; look lexbuf )
# 390 "afterstep.ml"

  | 16 ->
# 74 "afterstep.mll"
                          ( end_of_line lexbuf; look lexbuf )
# 395 "afterstep.ml"

  | 17 ->
# 75 "afterstep.mll"
                  ( TITLEBUTTON (args lexbuf 3) )
# 400 "afterstep.ml"

  | 18 ->
# 78 "afterstep.mll"
                   ( DEFAULTSTYLE (args lexbuf 1) )
# 405 "afterstep.ml"

  | 19 ->
# 79 "afterstep.mll"
                   ( FWINDOWSTYLE (args lexbuf 1) )
# 410 "afterstep.ml"

  | 20 ->
# 80 "afterstep.mll"
                   ( UWINDOWSTYLE (args lexbuf 1) )
# 415 "afterstep.ml"

  | 21 ->
# 81 "afterstep.mll"
                   ( SWINDOWSTYLE (args lexbuf 1) )
# 420 "afterstep.ml"

  | 22 ->
# 82 "afterstep.mll"
                    ( MENUITEMSTYLE (args lexbuf 1) )
# 425 "afterstep.ml"

  | 23 ->
# 83 "afterstep.mll"
                     ( MENUTITLESTYLE (args lexbuf 1) )
# 430 "afterstep.ml"

  | 24 ->
# 84 "afterstep.mll"
                      ( MENUHILITESTYLE (args lexbuf 1) )
# 435 "afterstep.ml"

  | 25 ->
# 85 "afterstep.mll"
                       ( MENUSTIPPLESTYLE (args lexbuf 1) )
# 440 "afterstep.ml"

  | 26 ->
# 89 "afterstep.mll"
        ( EOF )
# 445 "afterstep.ml"

  | 27 ->
# 90 "afterstep.mll"
      ( end_of_line lexbuf; look lexbuf )
# 450 "afterstep.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_look_rec lexbuf __ocaml_lex_state

and string lexbuf =
  __ocaml_lex_string_rec lexbuf 227
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 93 "afterstep.mll"
        ( String.sub strbuf 0 !strlen )
# 461 "afterstep.ml"

  | 1 ->
# 94 "afterstep.mll"
        ( failwith "EOF in string in WindowMaker menu file" )
# 466 "afterstep.ml"

  | 2 ->
# 95 "afterstep.mll"
        ( 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "string too long in Afterstep look file";
      string lexbuf )
# 476 "afterstep.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and end_of_line lexbuf =
  __ocaml_lex_end_of_line_rec lexbuf 231
and __ocaml_lex_end_of_line_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 103 "afterstep.mll"
                 ( () )
# 487 "afterstep.ml"

  | 1 ->
# 104 "afterstep.mll"
               ( end_of_line lexbuf )
# 492 "afterstep.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_end_of_line_rec lexbuf __ocaml_lex_state

and args lexbuf =
  __ocaml_lex_args_rec lexbuf 234
and __ocaml_lex_args_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 107 "afterstep.mll"
               ( args lexbuf )
# 503 "afterstep.ml"

  | 1 ->
# 108 "afterstep.mll"
        ( 
      strlen := 0; let s = string lexbuf in 
      fun n -> if n = 1 then [s] else s :: (args lexbuf (n-1)) )
# 510 "afterstep.ml"

  | 2 ->
# 111 "afterstep.mll"
                            (
      let s = Lexing.lexeme lexbuf in 
      fun n -> if n = 1 then [s] else s :: (args lexbuf (n-1)) )
# 517 "afterstep.ml"

  | 3 ->
# 114 "afterstep.mll"
         ( fun n ->
        let t = Array.create n "" in
        Array.to_list t
    )
# 525 "afterstep.ml"

  | 4 ->
# 118 "afterstep.mll"
        ( failwith "eof while waiting for args" )
# 530 "afterstep.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_args_rec lexbuf __ocaml_lex_state

;;

# 120 "afterstep.mll"
 
type look = 
  Style of string * (look list)
| Font of string
| ForeColor of string
| BackColor of string
| MaxColors of int
| Inherit of string
| BackPixmap of int * string
| TextStyle of int
| ButtonPixmap of string
| MArrowPixmap of string
| MenuPinOn of string
| WindowFont of string
| IconFont of string
| TitleButton of int * string * string

| DefaultStyle of string
| FWindowStyle of string
| UWindowStyle of string 
| SWindowStyle of string 
| MenuItemStyle of string 
| MenuTitleStyle of string 
| MenuHiliteStyle of string 
| MenuStippleStyle of string 
  (*
let rec parse_all list = parser
    [< 'STYLE [style]; m = parse_all []; 'ENDSTYLE; s >] ->
    parse_all ((Style (style, m))::list) s
|   [< 'FONT [font]; s >] -> 
    parse_all ((Font font)::list) s
|   [< 'TEXTSTYLE [style]; s >] -> 
    parse_all ((TextStyle (int_of_string style))::list) s
|   [< 'FORECOLOR [color]; s >] -> parse_all ((ForeColor color)::list) s
|   [< 'BACKCOLOR [color]; s >] -> parse_all ((BackColor color)::list) s
|   [< 'MAXCOLORS [max]; s >] -> 
    parse_all ((MaxColors (int_of_string max))::list) s
|   [< 'INHERIT [style]; s >] -> parse_all ((Inherit style)::list) s
|   [< 'BACKPIXMAP [max;pixmap]; s >] -> 
    parse_all ((BackPixmap (int_of_string max,pixmap))::list) s
|   [< 'TEXTSTYLE [style]; s >] -> 
    parse_all ((TextStyle (int_of_string style))::list) s
|   [< 'BUTTONPIXMAP [pixmap]; s >] -> 
    parse_all ((ButtonPixmap pixmap)::list) s
|   [< 'MARROWPIXMAP [pixmap]; s >] -> 
    parse_all ((MArrowPixmap pixmap)::list) s
|   [< 'MENUPINON [pixmap]; s >] -> parse_all ((MenuPinOn pixmap)::list) s
|   [< 'WINDOWFONT [font]; s >] -> parse_all ((WindowFont font)::list) s
|   [< 'ICONFONT [font]; s >] -> parse_all ((IconFont font)::list) s
|   [< 'TITLEBUTTON [num;pix1;pix2]; s >] -> 
    parse_all ((TitleButton (int_of_string num, pix1,pix2))::list) s

|   [< 'DEFAULTSTYLE [style]; s >] -> parse_all ((DefaultStyle style)::list) s
|   [< 'FWINDOWSTYLE [style]; s >] -> parse_all ((FWindowStyle style)::list) s
|   [< 'UWINDOWSTYLE [style]; s >] -> parse_all ((UWindowStyle style)::list) s
|   [< 'SWINDOWSTYLE [style]; s >] -> parse_all ((SWindowStyle style)::list) s
|   [< 'MENUITEMSTYLE [style]; s >] -> parse_all ((MenuItemStyle style)::list) s
|   [< 'MENUTITLESTYLE [style]; s >] -> parse_all ((MenuTitleStyle style)::list) s
|   [< 'MENUHILITESTYLE [style]; s >] -> parse_all ((MenuHiliteStyle style)::list) s
|   [< 'MENUSTIPPLESTYLE [style]; s >] -> parse_all ((MenuStippleStyle style)::list) s
  
|   [< >] -> List.rev list
  

let parse_look lexbuf =
  let stream = Stream.from (fun n -> 
        match look lexbuf with
          EOF -> None
        | t ->   Some t) in
  parse_all [] stream

let open_look name =
  let dirname = 
    Utils.find_in_path !!Themes.asthemes_path name
  in
  let filename = Filename.concat dirname ("look." ^ (Filename.basename name))
  in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let m = try parse_look lexbuf with e -> 
        Printf.printf "At pos %d:" (Lexing.lexeme_start lexbuf);
        print_newline ();
        close_in ic;
        raise e in
  close_in ic;
  m
*)  

let open_look name = []
    
let install_theme name =
  let look = open_look name in
  let dirname = 
    Utils.find_in_path !!Themes.asthemes_path name
  in
  root_image =:= ScaleImage (Filename.concat dirname
  ("background." ^ (Filename.basename name)));
  let styles = Hashtbl.create 31 in
  let rec getstyle list =
    match list with
      [] -> []
    | option :: list ->
        match option with
          Inherit style ->
            getstyle ((Hashtbl.find styles style) @ list)
        | _ -> option :: (getstyle list)
  in
  List.iter (fun option ->
      match option with
        Style (name, list) -> Hashtbl.add styles name list
          
      | FWindowStyle style ->
          active_font =:= !!default_font;
          active_foreground =:= !!default_foreground;
          active_background =:= !!default_background;
          active_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> active_font =:= font
              | ForeColor color -> active_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  active_image =:= TileImage filename
              | BackColor color -> active_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | UWindowStyle style ->
          title_font =:= !!default_font;
          title_foreground =:= !!default_foreground;
          title_background =:= !!default_background;
          title_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> title_font =:= font
              | ForeColor color -> title_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  title_image =:= TileImage filename
              | BackColor color -> title_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | MenuItemStyle style ->
          menu_font =:= !!default_font;
          menu_foreground =:= !!default_foreground;
          menu_background =:= !!default_background;
          menu_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> 
                  menu_font =:= font;
                  iconMgr_font =:= font;
              | ForeColor color -> 
                  menu_foreground =:= color;
                  iconMgr_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  menu_image =:= TileImage filename;
                  iconMgr_image =:= TileImage filename
              | BackColor color -> 
                  menu_background =:= color;
                  iconMgr_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | MenuHiliteStyle style -> 
          menu_hilite_font =:= !!default_font;
          menu_hilite_foreground =:= !!default_background;
          menu_hilite_background =:= !!default_foreground;
          menu_hilite_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> 
                  menu_hilite_font =:= font;
                  iconMgr_active_font =:= font;
              | ForeColor color -> 
                  menu_hilite_foreground =:= color;
                  iconMgr_active_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  menu_hilite_image =:= TileImage filename;
                  iconMgr_active_image =:= TileImage filename
              | BackColor color -> 
                  menu_hilite_background =:= color;
                  iconMgr_active_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | MenuTitleStyle style ->
          menu_title_font =:= !!default_font;
          menu_title_foreground =:= !!default_foreground;
          menu_title_background =:= !!default_background;
          menu_title_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> 
                  menu_title_font =:= font;
                  iconMgr_title_font =:= font;
              | ForeColor color -> 
                  menu_title_foreground =:= color;
                  iconMgr_title_foreground =:= color;
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  menu_title_image =:= TileImage filename;
                  iconMgr_title_image =:= TileImage filename;
              | BackColor color -> 
                  menu_title_background =:= color;
                  iconMgr_title_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | _ -> ()) look; 
  Array.iter (fun s ->
      let sw = s#wob in
      (* reset menus *)
      (!menu_manager)#reset sw;      
      (* reset the clients *)
      List.iter (fun (c,w) ->
          let tw = w.w_top in
          let focus = Wob.getenv tw focus_var in
          update_focus tw.w_oo focus
      ) (list_clients sw)) !screens

let init () = ()
  
let available_themes () = 
  List.flatten (List.map (fun dir -> Utils.list_dir_normal dir) !!asthemes_path)

  
let menu _ =
  List.map (fun name ->
      name, [], Function (fun w -> install_theme name)) (available_themes ())
  
  

# 793 "afterstep.ml"
