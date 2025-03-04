{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeQL.AST (Expr(..),Comparison(..),parseExpr) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as TP
import Text.Parsec.Language

-- | Comparison operators
data Comparison
  = Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
  deriving (Show, Eq)

-- | Unified AST for query expressions that covers both basic and quantified queries
data Expr
  = Literal String
  | LiteralList [String] -- literal1, literal2, literal3 ...
  | FieldRef String
  | FieldPath [String]  -- field.nestedField.evenMoreNestedField
  | Compare Comparison Expr Expr  -- x < y and friends
  | Between Expr Expr Expr -- x between 10 and 20
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Any Expr   -- ANY(field.access.path) or ANY(a, b, c)
  | All Expr   -- ALL(field.access.path) or ALL(a, b, c)
  | In Expr Expr   -- x IN (a, b, c, ..)
  deriving (Show, Eq)

-- | Helper function to strip quotes from strings
strip :: String -> String
strip s = case s of
  ('"':xs) | last xs == '"' -> init xs
  ('\'':xs) | last xs == '\'' -> init xs
  _ -> s

-- | Custom string literal parser that supports both single and double quotes with escaping
customStringLiteral :: Parser String
customStringLiteral = do
  quote <- char '"' <|> char '\''
  content <- many (escapedChar quote <|> noneOf [quote])
  _ <- char quote
  return content
  where
    escapedChar quote = try $ do
      _ <- char '\\'
      escaped <- oneOf [quote, '\\', 'n', 't', 'r']
      return $ case escaped of
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        _   -> escaped

-- | Unified parser that handles expressions based on the revised AST
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace *> expr <* eof) ""
  where
    -- Top-level expression parser that matches the AST structure
    expr = buildExpressionParser table term
    
    -- Main term parser - handles all expression types
    term = whiteSpace *> termExpr <* whiteSpace
    
    -- Term expressions are the core expression types
    termExpr = choice
      [ try parensExpr
      , try notExpr
      , try betweenExpr
      , try inExpr
      , try comparisonExpr
      , try atomicExpr
      ]
      
    -- Atomic expressions are the simplest expression types
    atomicExpr = choice
      [ try anyAllExpr
      , try literalListExpr
      , try literalExpr
      , try fieldAccessExpr
      ]
    
    -- Parser for parenthesized expressions
    parensExpr = parens expr
    
    -- Parser for literal values
    literalExpr = Literal . strip <$> (try customStringLiteral <|> try booleanLiteral <|> many1 (digit <|> char '.' <|> char '-'))
      where
      booleanLiteral = choice
        [ string "true" >> return "True",
          string "false" >> return "False"
        ]

    
    -- Parser for field access (both simple and path)
    fieldAccessExpr = do
      paths <- identifier `sepBy1` char '.'
      case paths of
        [single] -> return $ FieldRef single
        multiple -> return $ FieldPath multiple
    
    -- Parser for literal lists
    literalListExpr = do
      values <- parens $ sepBy1 literalValue (whiteSpace *> char ',' <* whiteSpace)
      return $ LiteralList values
      where
        literalValue = do
          (Literal x) <- literalExpr
          return x
    
    -- Parser for ANY/ALL expressions
    anyAllExpr = choice
      [ try $ do
          reserved "ANY"
          content <- parens (try fieldAccessExpr <|> expr)
          return $ Any content
      , try $ do
          reserved "ALL"
          content <- parens (try fieldAccessExpr <|> expr)
          return $ All content
      ]
    
    -- Parser for NOT expressions
    notExpr = do
      reserved "not"
      whiteSpace
      Not <$> term
    
    -- Parser for BETWEEN expressions
    betweenExpr = do
      value <- atomicExpr
      whiteSpace
      reserved "between"
      whiteSpace
      lowerBound <- term
      whiteSpace
      reserved "and"
      whiteSpace
      upperBound <- term
      return $ Between value lowerBound upperBound
    
    -- Parser for IN expressions
    inExpr = do
      left <- atomicExpr
      whiteSpace
      reserved "in"
      whiteSpace
      right <- choice [try literalListExpr, try fieldAccessExpr, parensExpr]
      return $ In left right
    
    -- Parser for comparison expressions
    comparisonExpr = do
      left <- atomicExpr
      whiteSpace
      comp <- comparisonOp
      whiteSpace
      right <- atomicExpr
      return $ Compare comp left right
    
    -- Parser for comparison operators
    comparisonOp = choice
      [ reservedOp "=" >> return Equal
      , reservedOp "!=" >> return NotEqual
      , try (reservedOp ">=") >> return GreaterThanOrEqual
      , try (reservedOp "<=") >> return LessThanOrEqual
      , reservedOp ">" >> return GreaterThan
      , reservedOp "<" >> return LessThan
      ]
    
    -- Expression table with correct operator precedence
    table =
      [ [Prefix (reserved "not" >> return Not)]
      , [Infix (reserved "and" >> whiteSpace >> return And) AssocLeft]
      , [Infix (reserved "or" >> whiteSpace >> return Or) AssocLeft]
      ]
    
    -- Language definition
    languageDef = emptyDef
      { TP.commentStart = "/*"
      , TP.commentEnd = "*/"
      , TP.commentLine = "--"
      , TP.identStart = letter
      , TP.identLetter = alphaNum <|> char '_'
      , TP.opStart = oneOf "=!<>"
      , TP.opLetter = oneOf "=!<>"
      , TP.reservedOpNames = ["=", "!=", "<", ">", "<=", ">="]
      , TP.reservedNames = ["ANY", "ALL", "BETWEEN", "AND", "OR", "NOT", "IN", "between", "and", "or", "not", "in"]
      , TP.caseSensitive = False
      }
    
    TP.TokenParser
      { TP.parens = parens
      , TP.identifier = identifier
      , TP.reservedOp = reservedOp
      , TP.reserved = reserved
      , TP.whiteSpace = whiteSpace
      } = TP.makeTokenParser languageDef
