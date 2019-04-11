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

data Index
  = SIndex Expr
  | DIndex Expr Expr
  deriving (Show, Eq)

data DeclVar
  = DBaseVar Ident
  | ShapeVar Ident Shape
  deriving (Show, Eq)

data StmtVar
  = SBaseVar Ident
  | IndexVar Ident Index
  deriving (Show, Eq)

data Expr
  = Id StmtVar
  | BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Add Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Or Expr Expr
  | And Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | LessEqual Expr Expr
  | Greater Expr Expr
  | GreaterEqual Expr Expr
  | Neg Expr
  | UMinus Expr
  deriving (Show, Eq)

data Decl
  = Decl BaseType DeclVar
  deriving (Show, Eq)

data Stmt
  = Assign StmtVar Expr
  | Read StmtVar
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  deriving (Show, Eq)

data Param
  = Param Indicator BaseType Ident
  deriving (Show, Eq)

data Proc
  = Proc Ident [Param] [Decl] [Stmt]
  deriving (Show, Eq)

data GoatProg
  = Prog [Proc]
  deriving (Show, Eq)
