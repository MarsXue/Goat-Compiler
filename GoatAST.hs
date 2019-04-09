module GoatAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String

data Indicator
  = Val | Ref
  deriving(Show, Eq)

data BaseType
  = BoolType | IntType | FloatType
  deriving (Show, Eq)

data Shape
  = SShape Int
  | DShape Int Int
  deriving (Show, Eq)

data Idx
  = SIdx Expr
  | DIdx Expr Expr
  deriving (Show, Eq)

data VarType
  = SVarType BaseType 
  | DVarType BaseType Shape
  deriving (Show, Eq)

data LValue
  = SLVal Ident 
  | DLVal Ident Idx
  deriving (Show, Eq)

data Binop
  = Add 
  | Minus 
  | Mul 
  | Div 
  | Or 
  | And 
  | Equal 
  | NotEqual 
  | Less 
  | LessEqual 
  | Greater 
  | GreaterEqual
  deriving (Show, Eq)

data Unop
  = Neg | UMinus
  deriving (Show, Eq)

data Expr
  = SId Ident
  | DId Ident Idx
  | BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Binop Expr Expr
  | Unop Expr
  deriving (Show, Eq)

data Decl
  = Decl VarType Ident
  deriving (Show, Eq)

data Stmt
  = AStmt
  | CStmt
  deriving (Show, Eq)

data AStmt
  = Assign LValue Expr
  | Read LValue
  | Write Expr
  | Call Ident [Expr]
  deriving (Show, Eq)

data CStmt
  = If Expr [Stmt] 
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  deriving (Show, Eq)

data Parameter
  = Parameter Indicator BaseType Ident
  deriving (Show, Eq)

data Header
  = Header Ident [Parameter]
  deriving (Show, Eq)

data Procedure
  = Procedure Header [Decl] [Stmt]
  deriving (Show, Eq)

data GoatProgram
  = Program [Procedure]
  deriving (Show, Eq)