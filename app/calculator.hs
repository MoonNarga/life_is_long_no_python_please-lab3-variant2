{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant flip" #-}
module Main where

import Control.Monad.State
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Expression
  = Constant Double
  | Identifier String
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Modulus Expression Expression
  | Negation Expression
  | FunctionInvocation String Expression
  deriving (Show)

data FunctionBody = FunctionBody String Expression
  deriving (Show)

data Statement
  = PrintStatement Expression
  | AssignmentStatement String Expression
  | FunctionDefinition String FunctionBody
  deriving (Show)

lexer :: TokenParser ()
lexer = makeTokenParser (javaStyle {opStart = oneOf "+-*/%", opLetter = oneOf "+-*/%"})

parseNumber :: Parser Expression
parseNumber = do
  val <- naturalOrFloat lexer
  case val of
    Left i -> return $ Constant $ fromIntegral i
    Right n -> return $ Constant n

parseFunctionInvocation :: Parser Expression
parseFunctionInvocation = do
  ident <- identifier lexer
  expr <- parens lexer parseExpression
  return $ FunctionInvocation ident expr

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression <|> parseNumber <|> try parseFunctionInvocation <|> (identifier lexer >>= return . Identifier)

parsePrint :: Parser Statement
parsePrint = do
  reserved lexer "print"
  expr <- parseExpression
  return $ PrintStatement expr

parseAssignment :: Parser Statement
parseAssignment = do
  reserved lexer "let"
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseExpression
  return $ AssignmentStatement ident expr

parseFunctionDefinition :: Parser Statement
parseFunctionDefinition = do
  reserved lexer "def"
  ident <- identifier lexer
  argname <- parens lexer $ identifier lexer
  reservedOp lexer "="
  expr <- parseExpression
  return $ FunctionDefinition ident (FunctionBody argname expr)

parseInput :: Parser Statement
parseInput = do
  whiteSpace lexer
  s <- (parsePrint <|> parseAssignment <|> parseFunctionDefinition)
  eof
  return s

parseExpression :: Parser Expression
parseExpression =
  (flip buildExpressionParser)
    parseTerm
    [ [ Prefix (reservedOp lexer "-" >> return Negation),
        Prefix (reservedOp lexer "+" >> return id)
      ],
      [ Infix (reservedOp lexer "*" >> return Multiplication) AssocLeft,
        Infix (reservedOp lexer "/" >> return Division) AssocLeft,
        Infix (reservedOp lexer "%" >> return Modulus) AssocLeft
      ],
      [ Infix (reservedOp lexer "+" >> return Addition) AssocLeft,
        Infix (reservedOp lexer "-" >> return Subtraction) AssocLeft
      ]
    ]

-- Interpreter
type StoredVal = Either Double FunctionBody

type Calculator a = StateT (M.Map String StoredVal) IO a

interpretExpression :: Expression -> Calculator Double
interpretExpression (Constant n) = return n
interpretExpression (Identifier i) = do
  varmap <- get
  case M.lookup i varmap of
    Nothing -> fail ("Unknown identifier: " ++ i)
    Just (Left n) -> return n
    Just (Right _) -> fail ("you must call function: " ++ i)
interpretExpression (Addition e1 e2) = do
  v1 <- interpretExpression e1
  v2 <- interpretExpression e2
  return (v1 + v2)
interpretExpression (Subtraction e1 e2) = do
  v1 <- interpretExpression e1
  v2 <- interpretExpression e2
  return (v1 - v2)
interpretExpression (Multiplication e1 e2) = do
  v1 <- interpretExpression e1
  v2 <- interpretExpression e2
  return (v1 * v2)
interpretExpression (Division e1 e2) = do
  v1 <- interpretExpression e1
  v2 <- interpretExpression e2
  return (v1 / v2)
interpretExpression (Modulus e1 e2) = do
  v1 <- interpretExpression e1
  v2 <- interpretExpression e2
  let n1 = floor v1
      n2 = floor v2
      m = n1 `mod` n2
  return $ fromInteger m
interpretExpression (Negation e1) = do
  v1 <- interpretExpression e1
  return $ negate v1
interpretExpression (FunctionInvocation fn e) = do
  ctx <- get
  case M.lookup fn ctx of
    Nothing -> fail ("Unknown Function: " ++ fn)
    Just (Left _) -> fail ("cannot call constant: " ++ fn)
    Just (Right (FunctionBody argname expr)) -> do
      n <- interpretExpression e
      modify (M.insert argname (Left n))
      r <- interpretExpression expr
      modify (M.delete argname)
      return r

interpretStatement :: Statement -> Calculator ()
interpretStatement (PrintStatement expr) = do
  n <- interpretExpression expr
  liftIO $ print n
interpretStatement (AssignmentStatement ident expr) = do
  n <- interpretExpression expr
  modify (M.insert ident (Left n))
interpretStatement (FunctionDefinition fn body) = do
  modify (M.insert fn (Right body))

defaultVars :: M.Map String StoredVal
defaultVars =
  M.fromList
    [ ("pi", Left 3.14)
    ]

calculate :: String -> Calculator ()
calculate s = do
  case ret of
    Left e -> liftIO $ putStrLn $ "error: " ++ show e
    Right n -> interpretStatement n
  where
    ret = parse parseInput "" s

calculator :: Calculator ()
calculator =
  liftIO getContents >>= (mapM_ calculate) . lines

main :: IO ()
main = evalStateT calculator defaultVars