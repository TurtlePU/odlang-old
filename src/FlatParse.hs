module FlatParse where

import Data.List (foldl')

import Object
import Token

flatSeq :: [Object] -> Object
flatSeq [x] = x
flatSeq xs = Seq xs

flatChain :: Object -> [(Ident, [(Ident, Object)])] -> Object
flatChain = foldl' (\obj (method, args) -> Call method obj args)

flatInfix :: Object -> [(Operator, Object)] -> Object
flatInfix obj [] = obj
flatInfix obj tail = Infix obj tail

flatUnary :: [Operator] -> Object -> Object
flatUnary = flip $ foldr Unary
