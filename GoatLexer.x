{
module Main where
}

%wrapper "basic"

$digit       = 0-9
@alpha       = [a-zA-Z]
@digits      = $digit+
@stringlit   = \" [^\"]* \"
@ident       = @alpha (@alpha | $digit | \_ | \')*
@comment     = \# [^\n]* \n
@float       = @digits\.@digits

rules :-

  $white+    ;    -- skip white space
  @comment   ;    -- skip comments
  @digits    { \s -> INT_CONST (read s :: Int) }
  @float     { \s -> FLOAT_CONST (read s :: Float) }
  true       { \s -> BOOL_CONST True }
  false      { \s -> BOOL_CONST False }
  bool       { \s -> BOOL }
  int        { \s -> INT }
  proc       { \s -> PROC }
  begin      { \s -> BEGIN }
  end        { \s -> END }
  read       { \s -> READ }
  write      { \s -> WRITE }
  
  do         { \s -> DO }
  else       { \s -> ELSE }
  fi         { \s -> FI }
  float      { \s -> FLOAT }
  if         { \s -> IF }
  od         { \s -> OD }
  ref        { \s -> REF }
  then       { \s -> THEN}
  val        { \s -> VAL }
  while      { \s -> WHILE}

  :=         { \s -> ASSIGN }
  \(         { \s -> LPAREN }
  \)         { \s -> RPAREN }
  \+         { \s -> PLUS }
  \-         { \s -> MINUS }
  \*         { \s -> MUL }
  \;         { \s -> SEMI }
  
  \|\|       { \s -> OR }
  \&\&       { \s -> AND }
  \!         { \s -> NEG }
  \=         { \s -> EQUAL}
  \!\=       { \s -> NOT_EQUAL }
  \<         { \s -> LESS }
  \<\=       { \s -> LESS_EQUAL }
  \>         { \s -> GREATER }
  \>\=       { \s -> GREATER_EQUAL }
  \/         { \s -> DIV }

  \,         { \s -> COMMA }
  \[         { \s -> LBRACKET }
  \]         { \s -> RBRACKET }
  
  @ident     { \s -> IDENT s }
  @stringlit { \s -> LIT s }

{
data Token
  = BOOL | INT | PROC | BEGIN | END | READ | WRITE | ASSIGN
  | INT_CONST Int | BOOL_CONST Bool | IDENT String | LIT String
  | LPAREN | RPAREN | PLUS | MINUS | MUL | SEMI | FLOAT_CONST Float
  | DO | ELSE | FI | FLOAT | IF | OD | REF | THEN | VAL | WHILE
  | OR | AND | NEG | EQUAL | NOT_EQUAL | LESS | LESS_EQUAL | GREATER
  | GREATER_EQUAL | DIV | COMMA | LBRACKET | RBRACKET
    deriving (Eq, Show)

main
  = do
      s <- getContents
      print (alexScanTokens s)
}