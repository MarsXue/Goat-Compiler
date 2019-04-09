module GoatAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String

data BaseType
  = BoolType | IntType | FloatType
  deriving (Show, Eq)

data LValue
  = LId Ident
  | LId Ident [Expr]
  | LId Ident [Expr, Expr]
  deriving (Show, Eq)

data Binop
  = Op_add | Op_minus | Op_mul | Op_div | Op_or | Op_and | Op_equal | 
  deriving (Show, Eq)

data Unop
  = Op_neg | Op_minus
  deriving (Show, Eq)

data Expr
  = Id Ident
  | Id Ident [Expr]
  | Id Ident [Expr, Expr]
  | BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | (Expr)
  | Add Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Or  Expr Expr
  | And Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | LessEqual Expr Expr
  | Greater Expr Expr
  | GreaterEqual Expr Expr
  -- Unary
  | UnaryNeg Expr
  | UnaryMinus Expr
  deriving (Show, Eq)

data Decl
  = Decl Ident BaseType
  deriving (Show, Eq)

data Stmt
  = Assign LValue Expr
  | Read LValue
  | Write Expr
  | Call Ident [Expr]
  | If Expr Then [Stmt] Fi
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  deriving (Show, Eq)

data GoatProgram
  = Program [Decl] [Stmt]
  deriving (Show, Eq)