module Expression (Expression(..), Statement(..), Block, pprint) where

import Token

data Expression
    = TermExpr Term
    | Array [Expression]
    | Call Ident Expression [(Ident, Expression)]
    | Infix Expression [(Operator, Expression)]
    | Unary Operator Expression
    | Lambda [Ident] [Statement]
    deriving Show

data Statement = Value Expression | Decl Ident deriving Show;

type Block = [Statement]


pprint :: Block -> String
pprint = unlines . map (uncurry indent) . pprintBlock 0
    where indent = (++) . spaces
          spaces = concat . flip replicate tab
          tab = replicate 4 ' '


pprintBlock :: Int -> Block -> [(Int, String)]
pprintBlock n b = (n, "{") : (concatMap (pprint'' $ n + 1) b) ++ [(n, "}")]


pprint'' :: Int -> Statement -> [(Int, String)]
pprint'' n (Value expr) = pprint' n expr
pprint'' n (Decl ident) = [(n, "let " ++ ident)]


pprint' :: Int -> Expression -> [(Int, String)]
pprint' n (TermExpr term) =
    [ (n, printt term) ]

pprint' n (Array objs) =
    (n, "[") : concatMap app objs ++ [ (n, "]") ]
    where app obj = append "," $ pprint' (n + 1) obj

pprint' n (Call met obj []) = append (")." ++ met) $ prepend "(" $ pprint' n obj

pprint' n (Call met obj args) =
    header ++ concatMap arg args ++ [ (n, "}") ]
    where arg (id, obj) = prepend (id ++ ": ") $ pprint' (n + 1) obj
          header = append (")." ++ met ++ " {") $ prepend "(" $ pprint' n obj

pprint' n (Infix obj ops) =
    foldr merge [] $
        prepend "(" (append ")" (pprint' n obj)) : map (ppOps n) ops
    where merge xs [] = xs
          merge xs ((_, y):ys) = append y xs ++ ys

pprint' n (Unary op obj) =
    prepend (op ++ " (") $ append ")" $ pprint' n obj

pprint' n (Lambda ids obj) = prepend header $ pprintBlock n obj
    where header = '\\' : unwords ids ++ " -> "


ppOps n (op, obj) = prepend (' ' : op ++ " (") $ append ")" $ pprint' n obj


prepend :: String -> [(Int, String)] -> [(Int, String)]
prepend s ((n, t) : ss) = (n, s ++ t) : ss


append :: String -> [(Int, String)] -> [(Int, String)]
append s ss =
    let ((n, x) : xs) = reverse ss
    in reverse $ (n, x ++ s) : xs


printt :: Term -> String
printt (Real str) = str ++ "f"
printt (OdInt str) = str ++ "i"
printt (Str str) = '"' : str ++ "\""
printt (Id str) = str
