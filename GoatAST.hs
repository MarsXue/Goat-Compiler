module GoatAST where

-----------------------------------
-- Specification of an AST for Goat
-----------------------------------

-- Identifier: string
type Ident = String

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
  | SMatrix Int Int      -- matrix [m,n]
  deriving (Show, Eq)

-- Shape (expr) for array and matrix in statement
data Index
  = IArray Expr         -- array  [n]
  | IMatrix Expr Expr    -- matrix [m,n]
  deriving (Show, Eq)

-- Declaration variable
data DeclVar
  -- Ident: identifier
  -- Shape: declaration shape (int)
  = DBaseVar Ident
  | ShapeVar Ident Shape
  deriving (Show, Eq)

-- Statement variable
data StmtVar
  -- Ident: identifier
  -- Index: statement shape (expr)
  = SBaseVar Ident
  | IndexVar Ident Index
  deriving (Show, Eq)

-- Expression
data Expr
  -- Identifier
  = Id StmtVar
  -- Constant expression
  | BoolConst Bool
  | IntConst Int
  | FloatConst Float
  -- Binary operation expression
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
  -- Unary operation expression
  | Neg Expr
  | UMinus Expr
  deriving (Show, Eq)

-- Declaration
data Decl
  -- BaseType: base type
  -- DeclVar:  declaration variable
  = Decl BaseType DeclVar
  deriving (Show, Eq)

-- Statement
data Stmt
  -- StmtVar: statement variable
  -- Expr:    expression
  -- Ident:   identifier
  -- Stmt:    statement
  = Assign StmtVar Expr   -- <stmtvar> := <expr>;
  | Read StmtVar          -- read <stmtvar>;
  | Write Expr            -- write <expr>;
  | SWrite String         -- write <string>;
  | Call Ident [Expr]     -- call <id> (<expr-list>);
  | If Expr [Stmt] [Stmt] -- if <expr> then <stmt-list> else <stmt-list> fi
  | While Expr [Stmt]     -- while <expr> do <stmt-list> od
  deriving (Show, Eq)

-- Parameter
data Param
  -- Indicator: indicator
  -- BaseType:  base type
  -- Ident:     identifier
  = Param Indicator BaseType Ident
  deriving (Show, Eq)

-- Procedure
data Proc
  -- Ident:   identifier
  -- [Param]: list of parameters
  -- [Decl]:  list of declarations
  -- [Stmt]:  list of statements
  = Proc Ident [Param] [Decl] [Stmt]
  deriving (Show, Eq)

-- Goar Program
data GoatProg
  -- [Proc]: list of procedures
  = Prog [Proc]
  deriving (Show, Eq)
