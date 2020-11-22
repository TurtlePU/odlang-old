module Lexer (odlex, runLexer) where

import Text.Parsec
import Token (OdLexem(..), Term(..), LexemToken)

runLexer :: String -> String -> Either ParseError [LexemToken]
runLexer = parse odlex

odlex :: Parsec String () [LexemToken]
odlex = optional ws *> optional sep *> lexem' `sepEndBy` ws <* eof
    where lexem' = flip (,) <$> getPosition <*> lexem

ws = noslash *> optional slash
    where noslash = skipMany $ oneOf " \t"
          slash = char '\\' *> skipMany (oneOf spaceOrSlash)

sep = many1 (oneOf ";\r\n") `sepEndBy1` ws

lexem = Sep <$ sep
    <|> Com <$ char ','
    <|> Br <$> oneOf braces
    <|> Op <$> many1 (oneOf operators)
    <|> Term <$> term

term = OdInt <$> many1 digit
    <|> Str <$> between (char '"') (char '"') (many $ strContent)
    <|> Id <$> many1 (noneOf $ special ++ braces ++ operators)

strContent = (char '\\' *> shielded) <|> noneOf "\""

shielded = char '\\'
    <|> char '"'
    <|> '\n' <$ char 'n'
    <|> '\r' <$ char 'r'
    <|> '\t' <$ char 't'

spaceOrSlash = " \t\\\r\n"
special = spaceOrSlash ++ ";,\""
braces = "()[]{}"
operators = "=+-*|/&!<.>@~"
