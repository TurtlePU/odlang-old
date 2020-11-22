{-# LANGUAGE TupleSections #-}

module Sugar where

import Expression
import Token

deBool :: [Ident] -> [(Ident, Expression)]
deBool = map (, lit "true")

dupId :: [Ident] -> [(Ident, Expression)]
dupId = map $ (,) <$> id <*> lit

data SugarStatement
    = Pure Statement
    | LetExpr Ident (Maybe Expression)
    | DoExpr Ident [Ident] [(Ident, Expression)]

toBlock :: [SugarStatement] -> Block
toBlock = foldr desugar []
    where desugar (Pure stmt) block = stmt : block
          desugar (LetExpr id Nothing) block = Decl id : block
          desugar (LetExpr id (Just e)) b = Decl id : Value (assign id e) : b
          desugar (DoExpr clb args rest) bl = [ Value $ appDo clb args rest bl ]

assign :: Ident -> Expression -> Expression
assign id expr = Infix (lit id) [("=", expr)]

appDo :: Ident -> [Ident] -> [(Ident, Expression)] -> Block -> Expression
appDo clb args rest block = call $ rest ++ [ (clb, lambda) ]
    where lambda = Lambda args $ block
          call ((method, obj) : args) = Call method obj args

lit = TermExpr . Id
