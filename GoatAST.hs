---------------------------------------------------------
-- Programming Language Implementation COMP90045 Project1
-- Implemented by Shjie Liu, Wenqing Xue, Minjian Chen
---------------------------------------------------------

module GoatAST where

import Text.Parsec.Pos

-----------------------------------
-- Specification of an AST for Goat
-----------------------------------

-- Identifier: String
type Ident = String

-- Position: SourcePos (SourceName, Line, Column)
type Pos = SourcePos

-- Parameter indicator: value, reference
data Indicator
  = Val
  | Ref
  deriving(Show, Eq)

-- Base type: bool, int, float
data BaseType
  = BoolType
  | IntType
  | FloatType
  deriving (Show, Eq)

-- Shape (int) for array and matrix in declaration
data Shape
  = SArray Int          -- array  [n]
  | SMatrix Int Int     -- matrix [m,n]
  deriving (Show, Eq)

-- Shape (expr) for array and matrix in statement
data Index
  = IArray Expr         -- array  [n]
  | IMatrix Expr Expr   -- matrix [m,n]
  deriving (Show, Eq)

-- Declaration variable
data DeclVar
  -- Ident: identifier
  -- Shape: declaration shape <int>
  = DBaseVar Ident
  | ShapeVar Ident Shape
  deriving (Show, Eq)

-- Statement variable
data StmtVar
  -- Ident: identifier
  -- Index: statement shape <expr>
  = SBaseVar Ident
  | IndexVar Ident Index
  deriving (Show, Eq)

-- Expression
data Expr
  -- Identifier
  = Id Pos StmtVar
  -- Constant expression
  | BoolConst Pos Bool
  | IntConst Pos Int
  | FloatConst Pos Float
  -- Binary operation expression
  | Add Pos Expr Expr
  | Minus Pos Expr Expr
  | Mul Pos Expr Expr
  | Div Pos Expr Expr
  | Or Pos Expr Expr
  | And Pos Expr Expr
  | Equal Pos Expr Expr
  | NotEqual Pos Expr Expr
  | Less Pos Expr Expr
  | LessEqual Pos Expr Expr
  | Greater Pos Expr Expr
  | GreaterEqual Pos Expr Expr
  -- Unary operation expression
  | Neg Pos Expr
  | UMinus Pos Expr
  deriving (Show, Eq)

-- Declaration
data Decl
  -- Pos: position
  -- BaseType: base type
  -- DeclVar:  declaration variable
  = Decl Pos BaseType DeclVar
  deriving (Show, Eq)

-- Statement
data Stmt
  -- Pos: position
  -- StmtVar: statement variable
  -- Expr:    expression
  -- Ident:   identifier
  -- Stmt:    statement
  = Assign Pos StmtVar Expr   -- <stmtvar> := <expr>;
  | Read Pos StmtVar          -- read <stmtvar>;
  | Write Pos Expr            -- write <expr>;
  | SWrite Pos String         -- write <string>;
  | Call Pos Ident [Expr]     -- call <id> (<expr-list>);
  | If Pos Expr [Stmt] [Stmt] -- if <expr> then <stmt-list> else <stmt-list> fi
  | While Pos Expr [Stmt]     -- while <expr> do <stmt-list> od
  deriving (Show, Eq)

-- Parameter
data Param
  -- Pos: position
  -- Indicator: indicator
  -- BaseType:  base type
  -- Ident:     identifier
  = Param Pos Indicator BaseType Ident
  deriving (Show, Eq)

-- Procedure
data Proc
  -- Pos: position
  -- Ident:   identifier
  -- [Param]: list of parameters
  -- [Decl]:  list of declarations
  -- [Stmt]:  list of statements
  = Proc Pos Ident [Param] [Decl] [Stmt]
  deriving (Show, Eq)

-- Goar Program
data GoatProg
  -- [Proc]: list of procedures
  = Prog [Proc]
  deriving (Show, Eq)
