type token =
  | INT of (int)
  | NBR of (float)
  | CELLROW of (string)
  | LPAREN
  | RPAREN
  | EQUAL
  | SEMICOL
  | DOT
  | SUM
  | MULT
  | AVERAGE
  | MAX
  | SHOW
  | SHOWALL
  | SHOWVAL
  | SHOWALLVAL
  | SHOWERROR
  | SHOWALLERROR
  | EOF

val debut :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Command.comm list
