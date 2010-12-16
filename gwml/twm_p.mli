type token =
  | EOF
  | LB
  | RB
  | LP
  | RP
  | MENUS
  | MENU
  | BUTTON
  | DEFAULT_FUNCTION
  | PLUS
  | MINUS
  | ALL
  | OR
  | CURSORS
  | PIXMAPS
  | ICONS
  | COLOR
  | SAVECOLOR
  | MONOCHROME
  | FUNCTION
  | ICONMGR_SHOW
  | ICONMGR
  | WINDOW_FUNCTION
  | ZOOM
  | ICONMGRS
  | ICONMGR_GEOMETRY
  | ICONMGR_NOSHOW
  | MAKE_TITLE
  | GRAYSCALE
  | ICONIFY_BY_UNMAPPING
  | DONT_ICONIFY_BY_UNMAPPING
  | NO_TITLE
  | AUTO_RAISE
  | NO_HILITE
  | ICON_REGION
  | META
  | SHIFT
  | LOCK
  | CONTROL
  | WINDOW
  | TITLE
  | ICON
  | ROOT
  | FRAME
  | COLON
  | EQUALS
  | SQUEEZE_TITLE
  | DONT_SQUEEZE_TITLE
  | START_ICONIFIED
  | NO_TITLE_HILITE
  | TITLE_HILITE
  | STICKY
  | MOVE
  | RESIZE
  | WAIT
  | SELECT
  | KILL
  | LEFT_TITLEBUTTON
  | RIGHT_TITLEBUTTON
  | NUMBER of (int)
  | KEYWORD of (Twm_t.keyword)
  | NKEYWORD of (Twm_t.nkeyword)
  | CKEYWORD of (Twm_t.clkeyword)
  | CLKEYWORD of (Twm_t.clkeyword)
  | FKEYWORD of (Twm_t.fkeyword)
  | FSKEYWORD of (Twm_t.fskeyword)
  | SKEYWORD of (Twm_t.skeyword)
  | DKEYWORD of (Twm_t.dkeyword)
  | JKEYWORD of (Twm_t.jkeyword)
  | WINDOW_RING
  | WARP_CURSOR
  | ERRORTOKEN
  | NO_STACKMODE
  | STRING of (string)

val twmrc :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Twm_t.twm list
