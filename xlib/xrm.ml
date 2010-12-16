# 1 "xrm.mll"
 
(***********************************************************************)
(*                                                                     *)
(*                             Xlib                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projets Para/SOR, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*
  Resources management. Un peu trop complique pour l'instant.
  
  *)
  open Xtypes  
  open Printf
  
  type expr = 
    STAR
  | SEMI
  | POINT
  | EOF
  | NAME of string
  | ERROR


# 31 "xrm.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\250\255\251\255\005\000\253\255\006\000\255\255\007\000\
    \008\000\001\000\013\000\046\000\047\000\052\000\067\000\070\000\
    \075\000\003\000\004\000\076\000\251\255\104\000\253\255\254\255\
    \012\000\009\000\011\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\003\000\255\255\003\000\255\255\000\000\
    \003\000\255\255\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\255\255\001\000\255\255\255\255\003\000\255\255\255\255\
    \000\000\000\000\000\000";
  Lexing.lex_default = 
   "\003\000\000\000\000\000\003\000\000\000\003\000\000\000\003\000\
    \008\000\009\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \016\000\017\000\017\000\021\000\000\000\021\000\000\000\000\000\
    \255\255\025\000\025\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\007\000\006\000\006\000\007\000\018\000\018\000\255\255\
    \255\255\255\255\006\000\255\255\000\000\024\000\024\000\255\255\
    \024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\008\000\000\000\005\000\000\000\255\255\255\255\255\255\
    \009\000\000\000\004\000\000\000\024\000\255\255\001\000\255\255\
    \255\255\255\255\009\000\255\255\255\255\255\255\009\000\255\255\
    \255\255\255\255\001\000\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\009\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\255\255\018\000\020\000\000\000\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\000\000\026\000\255\255\026\000\
    \255\255\255\255\000\000\017\000\020\000\255\255\255\255\010\000\
    \255\255\255\255\255\255\000\000\255\255\017\000\022\000\000\000\
    \000\000\017\000\023\000\011\000\000\000\255\255\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\017\000\024\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\012\000\255\255\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\015\000\
    \000\000\014\000\000\000\016\000\000\000\000\000\000\000\000\000\
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
    \002\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\009\000\000\000\017\000\018\000\003\000\
    \005\000\007\000\008\000\025\000\255\255\024\000\024\000\010\000\
    \024\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\255\255\003\000\005\000\007\000\
    \008\000\255\255\000\000\255\255\024\000\010\000\000\000\003\000\
    \005\000\007\000\008\000\003\000\005\000\007\000\008\000\010\000\
    \011\000\012\000\000\000\010\000\255\255\255\255\013\000\003\000\
    \005\000\007\000\008\000\255\255\255\255\255\255\255\255\010\000\
    \255\255\255\255\255\255\255\255\255\255\014\000\011\000\012\000\
    \015\000\255\255\255\255\255\255\013\000\016\000\019\000\255\255\
    \011\000\012\000\255\255\255\255\011\000\012\000\013\000\255\255\
    \255\255\255\255\013\000\014\000\255\255\025\000\015\000\026\000\
    \011\000\012\000\255\255\016\000\019\000\014\000\013\000\005\000\
    \015\000\014\000\021\000\255\255\015\000\016\000\019\000\255\255\
    \255\255\016\000\019\000\010\000\255\255\014\000\255\255\255\255\
    \015\000\255\255\255\255\255\255\255\255\016\000\019\000\255\255\
    \021\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\011\000\021\000\255\255\255\255\255\255\021\000\255\255\
    \255\255\255\255\255\255\012\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\021\000\255\255\255\255\255\255\255\255\014\000\
    \255\255\013\000\255\255\015\000\255\255\255\255\255\255\255\255\
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
    \000\000\009\000\255\255\017\000\018\000\003\000\005\000\007\000\
    \008\000\025\000\255\255\026\000\255\255\010\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\011\000\012\000\
    \255\255\255\255\255\255\255\255\013\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\014\000\255\255\255\255\015\000\255\255\
    \255\255\255\255\255\255\016\000\019\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \021\000";
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

let rec first lexbuf =
  __ocaml_lex_first_rec lexbuf 0
and __ocaml_lex_first_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 34 "xrm.mll"
         ( first lexbuf )
# 163 "xrm.ml"

  | 1 ->
# 36 "xrm.mll"
      ( (* not implemented yet  *)
      first lexbuf )
# 169 "xrm.ml"

  | 2 ->
# 38 "xrm.mll"
         ( STAR )
# 174 "xrm.ml"

  | 3 ->
# 39 "xrm.mll"
                                ( NAME (Lexing.lexeme lexbuf))
# 179 "xrm.ml"

  | 4 ->
# 40 "xrm.mll"
        ( EOF )
# 184 "xrm.ml"

  | 5 ->
# 41 "xrm.mll"
       ( first lexbuf )
# 189 "xrm.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_first_rec lexbuf __ocaml_lex_state

and next lexbuf =
  __ocaml_lex_next_rec lexbuf 19
and __ocaml_lex_next_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 44 "xrm.mll"
                ( SEMI )
# 200 "xrm.ml"

  | 1 ->
# 45 "xrm.mll"
        ( POINT )
# 205 "xrm.ml"

  | 2 ->
# 46 "xrm.mll"
        ( STAR )
# 210 "xrm.ml"

  | 3 ->
# 47 "xrm.mll"
                                 ( NAME (Lexing.lexeme lexbuf))
# 215 "xrm.ml"

  | 4 ->
# 48 "xrm.mll"
      ( ERROR )
# 220 "xrm.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_rec lexbuf __ocaml_lex_state

and ressource lexbuf =
  __ocaml_lex_ressource_rec lexbuf 25
and __ocaml_lex_ressource_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 51 "xrm.mll"
                            ( Lexing.lexeme lexbuf )
# 231 "xrm.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_ressource_rec lexbuf __ocaml_lex_state

;;

# 53 "xrm.mll"
 

type 'a node = {
    name : int;
    mutable items : 'a item list;
  }

and 'a item =
  Name of 'a node
| EndingStar of 'a
| Ressource of 'a

type 'a t =
  {
    mutable tags : int; (* 0 = *, 1 = end *)
    names : (string, int) Hashtbl.t;
    inverse : (int,string) Hashtbl.t;
    table : (int, 'a node) Hashtbl.t;
  }

let create () = 
  let t =
    { tags = 3;
      names = Hashtbl.create 19;
      inverse = Hashtbl.create 19;
      table = Hashtbl.create 19
    }
  in
  Hashtbl.add t.names "*" 0;
  Hashtbl.add t.inverse 0 "*";
  Hashtbl.add t.inverse 1 ":";
  Hashtbl.add t.inverse 2 "?";
  t

let add_name t name =
  try
    Hashtbl.find t.names name
  with
    Not_found ->
      t.tags <- t.tags + 1;
      Hashtbl.add t.names name t.tags;
      Hashtbl.add t.inverse t.tags name;
      t.tags

let print t name = Hashtbl.find t.inverse name

let set t resname res = 
(*  Printf.printf "Set:";
  List.iter (fun n -> Printf.printf ".%s" n) resname;
  print_newline ();*)
  let resname = List.map (fun name -> add_name t name) (List.rev resname) in
  match resname with
    name :: tail when name <> 0  ->
      let n =
        try
          Hashtbl.find t.table name
        with
          Not_found ->
            let n = {
                name = name;
                items = []
              } in
            Hashtbl.add t.table name n;
            n
      in
      let rec iter list n =
        match list with
          [] -> n.items <- (Ressource res) :: n.items
        | [0] -> n.items <- (EndingStar res) :: n.items
        | name :: tail -> 
            match n.items with
              (Name node) :: _ when node.name = name ->
                iter tail node
            | _ -> 
                let nn = { name = name; items = [] } in
                n.items <- (Name nn) :: n.items;
                iter tail nn
      in
      iter tail n
  | _ -> failwith "Invalid Xrm entry"

let rec iterget list n =
  match list with
    [] -> (* all words have matched *)
      let rec iter list =
        match list with
          [] -> raise Not_found
        | (Ressource res) :: _ -> res
        | (EndingStar res) :: _ -> res
        | _ :: tail -> iter tail
      in iter n.items
  | name :: tail -> 
      let rec iter items =
        match items with
          [] -> raise Not_found
        | (EndingStar res) :: _ -> res
        | (Name node) :: left when node.name = name -> 
            begin
              try
                iterget tail node
              with
                Not_found -> iter left
            end
        | (Name ({ name = 0 } as node)) :: left -> 
            begin
              try
                let rec iter list =
                  try
                    iterget list node
                  with
                    Not_found ->
                      match list with
                        [] -> raise Not_found
                      | name :: tail -> iter tail
                in
                iter list
              with
                Not_found -> iter left
            end
        | _ :: left -> iter left
      in iter n.items



let get t resname = 
  let resname = List.rev resname in
  match resname with
    name :: tail ->
      let num = Hashtbl.find t.names name in
      let node = Hashtbl.find t.table num in
      let list = List.map (add_name t) tail in
      iterget list node
  | _ -> failwith "Invalid Xrm query"

let loader t lexbuf =
  let set t all res =
    try
      set t all res
    with
      Failure _ -> ()
  in
  
    let rec others list =
      let token = next lexbuf in
      match token with
        STAR -> others ("*"::list)
      | NAME name -> others (name :: list)
      | POINT -> others list
      | SEMI -> List.rev list, ressource lexbuf
      | _ -> [], ""
    in
    let rec iter () =
      let token = first lexbuf in
      match token with
      | STAR -> 
          let all,res = others ["*"] in
          set t all res;
          iter ()
      | POINT -> iter ()
      | NAME name -> 
          let all,res = others [name] in
          set t all res;
          iter ()
      | _ -> ()
    in
    iter ()

let load t filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  loader t lexbuf;
  close_in ic

let safe_load t name =
  try
    load t name
  with
    _ -> ()
  
  (* Not implemented yet *)
let save t filename = ()

let print t pr =
  Hashtbl.iter (fun key n ->
      let rec iter list n =
        List.iter (fun item ->
            match item with
              Name nn -> iter (n::list) nn
            | Ressource res -> 
                let rec iter list =
                  match list with
                    { name = 0 } :: (({ name = name } :: _) as tail) ->
                      Printf.printf "%s" (print t name);
                      iter tail
                  | _ :: (({ name = name } :: _) as tail ) ->
                      Printf.printf ".%s" (print t name);
                      iter tail
                  | _ -> 
                      Printf.printf " : %s\n" (pr res)
                in 
                let _ = print t n.name in
                iter (n :: list)
            | EndingStar res  -> 
                let rec iter list =
                  match list with
                    { name = 0 } :: (({ name = name } :: _) as tail) ->
                      Printf.printf "%s" (print t name);
                      iter tail
                  | _ :: (({ name = name } :: _) as tail ) ->
                      Printf.printf ".%s" (print t name);
                      iter tail
                  | _ -> 
                      Printf.printf "* : %s\n" (pr res)
                in 
                let _ = print t n.name in
                iter (n :: list)
        ) n.items
      in
      iter [] n
  ) t.table;
  print_newline ()

let safe_get t list default =
  try
    get t list
  with
    _ -> default

let rget t rev key =
  let res = List.rev (key :: (List.rev rev)) in
  get t res

let safe_rget t rev key v =
  let res = List.rev (key :: (List.rev rev)) in
  safe_get t res v

let xdefaults dpy app_classe =
  let res = create () in
  let _ =
    try
      let root = dpy.dpy_roots.(0).scr_root in
      let gp = Xlib.getWholeProperty dpy root XA.xa_resource_manager in
      loader res (Lexing.from_string gp.gp_value)
    with _ -> let file =
        (*
          try Sys.getenv "XUSERFILESEARCHPATH" with
          Not_found -> 
    *)
        Filename.concat (Sys.getenv "HOME") ".Xdefaults" 
  in
  safe_load res file
  in
  let _ =
    try
    let gwml_res = Sys.getenv "XENVIRONMENT" 
      (*
        let path = 
          try Utils.string_to_path (Sys.getenv "XFILESEARCHPATH") 
          with _ -> [] in
        let xenv = try Sys.getenv "XENVIRONMENT" with _ -> "" in
        let xroot = try Filename.concat  (Sys.getenv "X11ROOT")
            "lib/X11/app-defaults/" with _ -> "" in
        Utils.find_in_path (path@[
      xenv; xroot; "/usr/X11/lib/X11/app-defaults/"]) app_classe
    *)
      in
      safe_load res gwml_res
    with _ -> ()
  in
  res


# 510 "xrm.ml"
