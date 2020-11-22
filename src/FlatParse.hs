module FlatParse where

import Data.List (foldl')

import Expression
import Sugar
import Token

flatChain :: Expression -> [(Ident, [(Ident, Expression)])] -> Expression
flatChain = foldl' (\obj (method, args) -> Call method obj args)

flatInfix :: Expression -> [(Operator, Expression)] -> Expression
flatInfix obj [] = obj
flatInfix obj tail = Infix obj tail

flatUnary :: [Operator] -> Expression -> Expression
flatUnary = flip $ foldr Unary

value :: Expression -> SugarStatement
value expr = Pure $ Value expr
