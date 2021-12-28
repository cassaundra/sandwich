module Language.Sandwich.AST where

import           Language.Sandwich.Type

type Module = [Declaration]

type Identifier = String

data Declaration
  = ConstantDecl Identifier Expression
  | FunctionDecl Identifier FunctionSignature Block
  deriving (Show, Eq)

data Block = Block [Statement] (Maybe Expression)
  deriving (Show, Eq)

data FunctionSignature =
  FunctionSignature
    { funSigArguments  :: [(Identifier, Maybe Type)]
    , funSigReturnType :: Type
    }
  deriving (Show, Eq)

data Statement
  = DeclarationStmt Declaration
  | AssignmentStmt [(AssignmentIdentifier, Expression)] -- should be non-empty
  | OperatorAssignmentStmt BinaryOperand AssignmentIdentifier Expression
  | ExpressionStmt Expression
  | IfStmt IfStatement
  | WhileStmt ConditionalBlock
  | ReturnStmt Expression
  | BreakStmt
  | ContinueStmt
  deriving (Show, Eq)

type ConditionalBlock = (Expression, Block)

data IfStatement =
  IfStatement
    { ifBlocks  :: [ConditionalBlock] -- should be non-empty
    , elseBlock :: Maybe ConditionalBlock
    }
  deriving (Show, Eq)

data AssignmentIdentifier
  = SimpleIdent Identifier
  | IndexedIdent Identifier Int
  deriving (Show, Eq)

data Expression
  = TupleExpr [Expression]
  | ArrayExpr [Expression]
  | CallExpr Expression [Expression]
  | TernaryExpr Expression Expression Expression
  | OperationExpr Operation
  | IndexExpr Expression Int
  | VariableExpr Identifier
  | LiteralExpr Literal
  deriving (Show, Eq)

data Operation
  = UnaryOp UnaryOperand Expression
  | BinaryOp BinaryOperand Expression Expression
  deriving (Show, Eq)

data UnaryOperand = NegateOp | ReferenceOp
  deriving (Show, Eq)

data BinaryOperand
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | ModOp
  | BitAndOp
  | BitOrOp
  | BitXorOp
  | BitShlOp
  | BitShrOp
  deriving (Show, Eq)

data Literal
  = IntegerLit Int
  | FloatLit Float
  | BooleanLit Bool
  | StringLit String
  | Lambda LambdaSignature Block
  deriving (Show, Eq)

data LambdaSignature =
  LambdaSignature
    { lamSigArguments  :: [(Identifier, Maybe Type)]
    , lamSigReturnType :: Maybe Type
    }
  deriving (Show, Eq)
