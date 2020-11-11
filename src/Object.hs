module Object (Object(..), pprint) where

import Token

data Object
    = Literal ObjTerm
    | Seq [Object]
    | Call Ident Object [(Ident, Object)]
    | Infix Object [(Operator, Object)]
    | Unary Operator Object
    | Lambda [Ident] Object
    deriving Show


pprint :: Object -> String
pprint = unlines . map (uncurry indent) . pprint' 0
    where indent = (++) . spaces
          spaces n = concat $ replicate n $ replicate 4 ' '


pprint' :: Int -> Object -> [(Int, String)]
pprint' n (Literal term) =
    [ (n, printt term) ]

pprint' n (Seq objs) =
    (n, "[") : concatMap app objs ++ [ (n, "]") ]
    where app obj = append ";" $ pprint' (n + 1) obj

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

pprint' n (Lambda ids obj) = prepend header $ pprint' n obj
    where header = '\\' : unwords ids ++ " -> "

ppOps n (op, obj) = prepend (' ' : op ++ " (") $ append ")" $ pprint' n obj

prepend :: String -> [(Int, String)] -> [(Int, String)]
prepend s ((n, t) : ss) = (n, s ++ t) : ss

append :: String -> [(Int, String)] -> [(Int, String)]
append s ss =
    let ((n, x) : xs) = reverse ss
    in reverse $ (n, x ++ s) : xs

printt :: ObjTerm -> String
printt (Real str) = str ++ "f"
printt (OdInt str) = str ++ "i"
printt (Str str) = '"' : str ++ "\""
printt (Id str) = str
