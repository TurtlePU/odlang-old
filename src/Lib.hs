module Lib (runParser, runLexer, pprint) where

import Control.Monad

import Text.Parsec (ParseError)

import Lexer (runLexer)
import Expression (Block, pprint)

import qualified Parser as P

runParser :: String -> String -> Either ParseError Block
runParser = (>=>) <$> runLexer <*> P.runParser
