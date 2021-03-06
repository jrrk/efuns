# 1 "cpp.mll"
 
(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
  
  let current_file = ref []
  let current_line = ref ""
  let current_pos = ref 0
  
  let abort () =
    let location = match !current_file with
        [] -> assert false
      | (_,filename,line,_)::_ -> Printf.sprintf " at %s:%d" filename !line
    in
    List.iter (fun (ic,_, _, _) -> close_in ic) !current_file;
    current_file := []; 
    current_line := "";
    location
  
  let failwith s = 
    let location = abort () in
    failwith (s^location)
  
  let path = ref []
  let maxstr = 100
  let strbuf = String.create maxstr
  let strlen = ref 0
  let add_char lexbuf =
    strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0;
    incr strlen;
    if !strlen = maxstr then failwith "String too long in cpp"


# 43 "cpp.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\252\255\253\255\001\000\002\000\003\000\004\000\006\000\
    \005\000\008\000\009\000\011\000\007\000\012\000\010\000\253\255\
    \254\255\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\000\000\000\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default = 
   "\003\000\000\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\015\000\000\000\
    \000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\011\000\002\000\002\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\011\000\017\000\012\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
    \017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
    \000\000\000\000\000\000\005\000\010\000\000\000\011\000\000\000\
    \000\000\006\000\008\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
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
    \001\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\016\000\255\255\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\003\000\004\000\005\000\006\000\008\000\
    \007\000\012\000\009\000\010\000\011\000\011\000\013\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\011\000\014\000\011\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\011\000\
    \014\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\006\000\
    \255\255\255\255\255\255\004\000\009\000\255\255\010\000\255\255\
    \255\255\005\000\007\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\255\255\255\255\255\255\255\255\
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
    \000\000\003\000\004\000\005\000\006\000\008\000\007\000\012\000\
    \009\000\010\000\014\000\011\000\013\000";
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

let rec next_line lexbuf =
  __ocaml_lex_next_line_rec lexbuf 0
and __ocaml_lex_next_line_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 43 "cpp.mll"
                               ( strlen:=0; include_file lexbuf )
# 148 "cpp.ml"

  | 1 ->
# 44 "cpp.mll"
                               ( strlen:=0; include_file lexbuf )
# 153 "cpp.ml"

  | 2 ->
# 45 "cpp.mll"
                    ( Lexing.lexeme lexbuf )
# 158 "cpp.ml"

  | 3 ->
# 46 "cpp.mll"
                    ( 
      match !current_file with
        [] -> assert false
      | [ic,_,_,_] -> close_in ic; ""
      | (ic,_,_,_) :: (((_,_,_,lexbuf) :: _) as file) ->
          close_in ic;
          current_file := file;
          next_line lexbuf
    )
# 171 "cpp.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_line_rec lexbuf __ocaml_lex_state

and include_file lexbuf =
  __ocaml_lex_include_file_rec lexbuf 14
and __ocaml_lex_include_file_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 57 "cpp.mll"
              ( 
      let filename = String.sub strbuf 0 !strlen in
      let filename = Utils.string_to_filename filename in
      let filename =
        try Utils.find_in_path !path filename with _ ->
            failwith (Printf.sprintf "included file %s not found" filename)
      in
      let ic = open_in filename in
      let lexbuf = Lexing.from_channel ic in
      current_file := (ic,filename,ref 0,lexbuf) :: !current_file;
      next_line lexbuf
    )
# 193 "cpp.ml"

  | 1 ->
# 69 "cpp.mll"
        ( failwith "EOF in #include directive" )
# 198 "cpp.ml"

  | 2 ->
# 70 "cpp.mll"
      ( add_char lexbuf; include_file lexbuf )
# 203 "cpp.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_include_file_rec lexbuf __ocaml_lex_state

;;

# 72 "cpp.mll"
 

let do_next_line buf maxlen =
  let len = String.length !current_line in
  if len - !current_pos = 0 then begin
      current_pos := 0;
      current_line := (match !current_file with
          [] -> assert false
        | (ic,_,line, lexbuf) :: _ -> incr line; next_line lexbuf);
    end;
  let len = String.length !current_line in
  let displen = len - !current_pos in
  if displen = 0 then 0 else
  let dolen = min maxlen displen in
  String.blit !current_line !current_pos buf 0 dolen;
  current_pos := !current_pos + dolen;
  dolen

let preprocess filename =
  let ic = open_in filename in
  current_file := [ic, filename, ref 0, Lexing.from_channel ic];
  Lexing.from_function do_next_line

# 233 "cpp.ml"
