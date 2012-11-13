type token =
  | NUM of (float)
  | MULTIPLY
  | DIVIDE
  | PLUS
  | MINUS
  | NEWLINE
  | ECHO
  | END

val stmt :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.stmt
