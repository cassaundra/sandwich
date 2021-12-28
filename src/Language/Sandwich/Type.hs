module Language.Sandwich.Type where

data Type
  = IntegerType
  | FloatType
  | BooleanType
  | StringType
  | TupleType [Type]
  | ArrayType Type
  | RefType Type
  | FunctionType SignatureType
  deriving (Show, Eq)

data SignatureType =
  SignatureType
    { sigArguments  :: [Type]
    , sigReturnType :: Type
    }
  deriving (Show, Eq)
