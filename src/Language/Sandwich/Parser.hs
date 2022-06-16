{-# LANGUAGE RecordWildCards #-}

module Language.Sandwich.Parser where

import           Control.Applicative            (many, optional, (<|>))
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Void                      (Void)
import           Language.Sandwich.AST
import           Language.Sandwich.Type
import           Text.Megaparsec                (Parsec, choice, label,
                                                 runParser)
import           Text.Megaparsec.Char           (alphaNumChar, char, letterChar,
                                                 space1)
import qualified Text.Megaparsec.Char.Lexer     as L

unimplemented = error "not implemented"

type Parser = Parsec Void String

pIdentifier :: Parser Identifier
pIdentifier =
  label "identifier" $
    lexeme $
      (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')

pFile :: Parser Module
pFile = many pDeclaration

pDeclaration :: Parser Declaration
pDeclaration = pConstantDecl <|> pFunctionDecl

pConstantDecl :: Parser Declaration
pConstantDecl = label "constant declaration" $ do
  symbol "const"
  ident <- pIdentifier
  symbol "="
  expr <- pExpression
  return $ ConstantDecl ident expr

pFunctionDecl :: Parser Declaration
pFunctionDecl =
  label "function declaration" $ do
    symbol "func"
    ident <- pIdentifier
    sigType <- pSigType
    block <- pBlock
    return $ FunctionDecl ident sigType block

pBlock :: Parser Block
pBlock = surround '{' '}' $ do
  body <- many pStatement
  returnExpr <- optional pExpression
  return $ Block body returnExpr

pStatement :: Parser Statement
pStatement =
  choice
    [ DeclarationStmt <$> pDeclaration,
      ExpressionStmt <$> pExpression,
      pAssignStmt,
      pOpAssignStmt,
      pIfStmt,
      pWhileStmt,
      pReturnStmt,
      pBreakStmt,
      pContinueStmt
    ]
    <* symbol ";"

pAssignStmt :: Parser Statement
pAssignStmt = unimplemented

pOpAssignStmt :: Parser Statement
pOpAssignStmt = unimplemented

pIfStmt :: Parser Statement
pIfStmt = unimplemented

pWhileStmt :: Parser Statement
pWhileStmt = unimplemented

pReturnStmt :: Parser Statement
pReturnStmt = ReturnStmt <$> (symbol "return" *> optional pExpression)

pBreakStmt :: Parser Statement
pBreakStmt = BreakStmt <$ symbol "break"

pContinueStmt :: Parser Statement
pContinueStmt = ContinueStmt <$ symbol "continue"

pExpression :: Parser Expression
pExpression =
  choice
    [ TupleExpr <$> surround '(' ')' (many pExpression),
      ArrayExpr <$> surround '[' ']' (many pExpression),
      pCallExpr,
      -- TODO ternary
      -- TODO index
      pOperation,
      VariableExpr <$> pIdentifier,
      LiteralExpr <$> pLiteral
    ]

pCallExpr :: Parser Expression
pCallExpr = do
  fun <- pExpression
  args <- many pExpression
  return $ CallExpr fun args

pOperation :: Parser Expression
pOperation = makeExprParser pExpression operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ prefix "&" RefOp,
      prefix "-" NegOp,
      Prefix (id <$ symbol "+")
    ],
    [ binary "*" MulOp,
      binary "/" DivOp
    ],
    [ binary "+" AddOp,
      binary "-" SubOp
    ],
    [ binary "&" BitAndOp,
      binary "|" BitOrOp,
      binary "^" BitXorOp
    ],
    [ binary "<<" BitShlOp,
      binary ">>" BitShrOp
    ]
  ]

-- adopted from the megaparsec tutorial

binary :: String -> (Expression -> Expression -> Operation) -> Operator Parser Expression
binary name f = InfixL (((OperationExpr .) <$> f) <$ symbol name)

prefix :: String -> (Expression -> Operation) -> Operator Parser Expression
prefix name f = Prefix ((OperationExpr <$> f) <$ symbol name)

pLiteral :: Parser Literal
pLiteral = unimplemented

pType :: Parser Type
pType =
  label "type" $
    choice
      [ IntegerType <$ symbol "int",
        FloatType <$ symbol "float",
        BooleanType <$ symbol "bool",
        StringType <$ symbol "string",
        TupleType <$> surround '(' ')' (many pType),
        ArrayType <$> surround '[' ']' pType,
        RefType <$> (char '&' *> pType),
        FunctionType <$> (symbol "fn" *> pSigType)
      ]

pSigType :: Parser SignatureType
pSigType = do
  sigArguments <- surround '(' ')' $ do unimplemented
  sigReturnType <- pType
  return SignatureType {..}

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

surround :: Char -> Char -> Parser a -> Parser a
surround l r a = lexeme (char l) *> a <* char r
