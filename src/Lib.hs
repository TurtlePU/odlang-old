module Lib (runParser, runLexer, pprint) where

import Control.Monad

import Text.Parsec (ParseError)

import Lexer (runLexer)
import Object (Object, pprint)

import qualified Parser as P

runParser :: String -> String -> Either ParseError Object
runParser = (>=>) <$> runLexer <*> P.runParser
