{-# LANGUAGE TupleSections #-}

module Sugar where

import FlatParse (flatSeq)
import Object
import Token

deBool :: [Ident] -> [(Ident, Object)]
deBool = map (, lit "true")

dupId :: [Ident] -> [(Ident, Object)]
dupId = map $ (,) <$> id <*> lit

data DoExpr = DoExpr Ident Ident [(Ident, Object)]

unDo :: [Either DoExpr Object] -> [Object]
unDo = foldr desugar []
    where desugar (Right obj) objs = obj : objs
          desugar (Left expr) objs = [ appDo expr objs ]

appDo :: DoExpr -> [Object] -> Object
appDo (DoExpr clb arg rest) objs = call $ rest ++ [ (clb, lambda) ]
    where lambda = Lambda [arg] $ flatSeq body
          body = if null objs then [ lit arg ] else objs
          call ((method, obj) : args) = Call method obj args

lit = Literal . Id
