module Lexer (odlex, runLexer) where

import Text.Parsec
import Token (OdLexem(..), ObjTerm(..), LexemToken)

runLexer :: String -> String -> Either ParseError [LexemToken]
runLexer = parse odlex

odlex :: Parsec String () [LexemToken]
odlex = optional ws *> optional sep *> lexem' `sepEndBy` ws <* eof
    where lexem' = flip (,) <$> getPosition <*> lexem

ws = noslash *> optional slash
    where noslash = skipMany $ oneOf " \t"
          slash = char '\\' *> skipMany (oneOf " \t\\\r\n")

sep = many1 (oneOf ";\r\n") `sepEndBy1` ws

lexem = choice
    [ Sep <$ sep
    , Com <$ char ','
    , Br <$> oneOf braces
    , Op <$> many1 (oneOf operators)
    , ObjLexem <$> objLexem
    ]

objLexem = choice
    [ OdInt <$> many1 digit
    , Str <$> between (char '"') (char '"') (many $ strContent)
    , Id <$> many1 (noneOf $ special ++ braces ++ operators)
    ]

strContent = strSlash <|> noneOf "\""

strSlash = char '\\' *> choice
    [ char '\\'
    , char '"'
    , '\n' <$ char 'n'
    , '\r' <$ char 'r'
    , '\t' <$ char 't'
    ]

special = " \t\\\r\n;,\""
braces = "()[]{}"
operators = "=+-*|/&!<.>"
