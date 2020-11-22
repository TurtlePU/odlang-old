module Token where

import Text.Parsec
import Control.Monad

data OdLexem
    = Sep
    | Com
    | Br Char
    | Op Operator
    | Term Term
    deriving Show

data Term
    = Real String
    | OdInt String
    | Str String
    | Id Ident
    deriving Show

type Ident = String

type Operator = String

type LexemToken = (OdLexem, SourcePos)

odToken :: (OdLexem -> Maybe a) -> Parsec [LexemToken] st a
odToken = token (show . fst) snd . (. fst)

guardP :: (a -> Bool) -> ParsecT s u m a -> ParsecT s u m ()
guardP = flip (>>=) . (guard .)

sep = odToken f
    where f Sep = Just ()
          f _ = Nothing

comma = odToken f
    where f Com = Just ()
          f _ = Nothing

brace c = odToken f
    where f (Br d) = guard $ c == d
          f _ = Nothing

operator = odToken f
    where f (Op s) = Just s
          f _ = Nothing

matchOp op = guardP (== op) operator

term = odToken f
    where f (Term t) = Just t
          f _ = Nothing

ident = odToken f
    where f (Term (Id s)) = Just s
          f _ = Nothing

notIdent = odToken f
    where f (Term (Id _)) = Nothing
          f (Term x) = Just x
          f _ = Nothing

matchId id = guardP (== id) ident
