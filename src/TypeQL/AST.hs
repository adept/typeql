{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeQL.AST where
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
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
  char quote
  return content
  where
    escapedChar quote = try $ do
      char '\\'
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
    expr = buildExpressionParser table term

    -- Separate parsers for different expression types
    term = do
      whiteSpace
      choice
        [ try (parens expr)
        , try notExpr
        , try betweenExpr
        , try inExpr
        , try anyExpr
        , try allExpr
        , comparisonExpr
        ]

    -- Simple expression parser (literals, field paths)
    simpleExpr = choice
      [ try fieldPathExpr
      , literal
      , parens expr
      ]

    -- Field path parser (handles both FieldRef and FieldPath)
    fieldPathExpr = do
      paths <- identifier `sepBy1` char '.'
      case paths of
        [single] -> return $ FieldRef single
        multiple -> return $ FieldPath multiple

    -- Custom NOT expression parser
    notExpr = do
      reserved "not"
      whiteSpace
      Not <$> term

    -- BETWEEN expression parser
    betweenExpr = do
      value <- simpleExpr
      reserved "between"
      lower <- simpleExpr
      reserved "and"
      upper <- simpleExpr
      return $ Between value lower upper

    -- IN expression parser
    inExpr = do
      left <- simpleExpr
      reserved "in"
      right <- literalListExpr <|> fieldPathExpr <|> parens expr
      return $ In left right

    -- Literal list parser
    literalListExpr = do
      literals <- parens $ commaSep literalValue
      return $ LiteralList literals
      where
        literalValue = strip <$> (try customStringLiteral <|> many1 (digit <|> char '.' <|> char '-'))

    -- ANY expression parser
    anyExpr = do
      reserved "ANY"
      expr <- parens expr
      return $ Any expr

    -- ALL expression parser
    allExpr = do
      reserved "ALL"
      expr <- parens expr
      return $ All expr

    -- Field path list parser for ANY/ALL
    fieldPathList = sepBy1 identifier (char '.')

    -- Comparison expression parser
    comparisonExpr = do
      left <- simpleExpr
      whiteSpace
      option left $ do
        op <- comparisonOp
        right <- simpleExpr
        case op of
          Nothing -> return left
          Just comp -> return $ Compare comp left right

    -- Comparison operator parser
    comparisonOp = choice
      [ try (opParser "=" >> return (Just Equal))
      , try (opParser "!=" >> return (Just NotEqual))
      , try (opParser ">=" >> return (Just GreaterThanOrEqual))
      , try (opParser "<=" >> return (Just LessThanOrEqual))
      , try (opParser ">" >> return (Just GreaterThan))
      , try (opParser "<" >> return (Just LessThan))
      ]

    -- Custom operator parser that allows surrounding whitespace
    opParser name = do
      whiteSpace
      reservedOp name
      whiteSpace
      return name

    -- Expression table with correct operator precedence
    table =
      [ [Prefix (do
          reserved "not"
          return Not
        )]
      , [Infix (do
          reserved "and"
          whiteSpace
          return And
        ) AssocLeft]
      , [Infix (do
          reserved "or"
          whiteSpace
          return Or
        ) AssocLeft]
      ]

    literal = do
      whiteSpace
      Literal <$> (try customStringLiteral <|> many1 (digit <|> char '.' <|> char '-'))

    languageDef = emptyDef
      { commentStart = "/*"
      , commentEnd = "*/"
      , commentLine = "--"
      , identStart = letter
      , identLetter = alphaNum <|> char '_'
      , opStart = oneOf "=!<>"
      , opLetter = oneOf "=!<>"
      , reservedOpNames = ["=", "!=", "<", ">", "<=", ">="]
      , reservedNames = ["ANY", "ALL", "BETWEEN", "AND", "OR", "NOT", "IN"]
      , caseSensitive = False
      }

    TokenParser
      { parens = parens
      , identifier = identifier
      , reservedOp = reservedOp
      , reserved = reserved
      , whiteSpace = whiteSpace
      , commaSep = commaSep
      } = makeTokenParser languageDef
