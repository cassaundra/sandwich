module Language.Sandwich.AST where

import           Language.Sandwich.Type

type Module = [Declaration]

type Identifier = String

data Declaration
  = ConstantDecl Identifier Expression
  | FunctionDecl Identifier SignatureType Block
  deriving (Show, Eq)

data Block = Block [Statement] (Maybe Expression)
  deriving (Show, Eq)

data Statement
  = DeclarationStmt Declaration
  | ExpressionStmt Expression
  | AssignmentStmt [(AssignmentIdentifier, Expression)] -- should be non-empty
  | OperatorAssignmentStmt AssignmentIdentifier Expression
  | IfStmt IfStatement
  | WhileStmt ConditionalBlock
  | ReturnStmt (Maybe Expression)
  | BreakStmt
  | ContinueStmt
  deriving (Show, Eq)

type ConditionalBlock = (Expression, Block)

data IfStatement = IfStatement
  { ifBlocks  :: [ConditionalBlock], -- should be non-empty
    elseBlock :: Maybe ConditionalBlock
  }
  deriving (Show, Eq)

data AssignmentIdentifier
  = SimpleIdent Identifier
  | TypedIdent Identifier Type
  | IndexedIdent Identifier Int
  deriving (Show, Eq)

data Expression
  = TupleExpr [Expression]
  | ArrayExpr [Expression]
  | CallExpr Expression [Expression]
  | TernaryExpr Expression Expression Expression
  | IndexExpr Expression Int
  | OperationExpr Operation
  | VariableExpr Identifier
  | LiteralExpr Literal
  deriving (Show, Eq)

-- data Operation
--   = UnaryOp UnaryOperand Expression
--   | BinaryOp BinaryOperand Expression Expression
--   deriving (Show, Eq)

data Operation
  = RefOp Expression
  | NegOp Expression
  | MulOp Expression Expression
  | DivOp Expression Expression
  | AddOp Expression Expression
  | SubOp Expression Expression
  | BitAndOp Expression Expression
  | BitOrOp Expression Expression
  | BitXorOp Expression Expression
  | BitShlOp Expression Expression
  | BitShrOp Expression Expression
  deriving (Show, Eq)

-- data UnaryOperand = NegOp | RefOp
--   deriving (Show, Eq)

-- data BinaryOperand
--   = AddOp
--   | SubOp
--   | MulOp
--   | DivOp
--   | ModOp
--   | BitAndOp
--   | BitOrOp
--   | BitXorOp
--   | BitShlOp
--   | BitShrOp
--   deriving (Show, Eq)

data Literal
  = IntegerLit Int
  | FloatLit Float
  | BooleanLit Bool
  | StringLit String
  | Lambda LambdaSignature Block
  deriving (Show, Eq)

data LambdaSignature = LambdaSignature
  { lamSigArguments  :: [(Identifier, Maybe Type)],
    lamSigReturnType :: Maybe Type
  }
  deriving (Show, Eq)
