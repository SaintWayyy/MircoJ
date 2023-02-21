type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | DOT
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | DOUBLE
  | VOID
  | STRING
  | BREAK
  | CONTINUE
  | INTLIST
  | BOOLLIST
  | DOUBLELIST
  | STRINGLIST
  | CLASS
  | INTERFACE
  | NEW
  | IMPLEMENTS
  | EXTENDS
  | IS
  | PUBLIC
  | PRIVATE
  | PROTECT
  | STATIC
  | THIS
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | DLIT of (string)
  | STRINGLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
