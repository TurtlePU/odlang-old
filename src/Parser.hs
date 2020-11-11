module Parser (mainParser, runParser) where

import Text.Parsec hiding (runParser)

import FlatParse
import Object
import Sugar
import Token hiding (comma, operator, matchOp)

import qualified Token as T

type OdParser = Parsec [LexemToken] ()
type ObjectParser = OdParser Object

runParser :: String -> [LexemToken] -> Either ParseError Object
runParser = parse $ mainParser <* eof

mainParser :: ObjectParser
mainParser = flatSeq . unDo <$> chainOrDo `sepEndBy` sep
    where chainOrDo = Left <$> try doExpr <|> Right <$> chain

doExpr :: OdParser DoExpr
doExpr = DoExpr <$> ident <*> ident <* matchOp "<-" <*> methodTail many1

chain :: ObjectParser
chain = flatChain <$> infixx <*> many (comma *> methodCall)
    where methodCall = (,) <$> ident <*> methodTail many

infixx :: ObjectParser
infixx = flatInfix <$> operand <*> many ((,) <$> operator <*> operand)
    where operand = try canonicCall <|> unary
          canonicCall = Call <$> ident <*> recurse <*> methodTail many

methodTail manyf = (++) <$> manyf arg <*> option [] bools
    where arg = (,) <$> ident <*> unary
          bools = deBool <$> braced (ident `sepEndBy` comma)

unary :: ObjectParser
unary = flatUnary <$> many operator <*> recurse

recurse :: ObjectParser
recurse = braced mainParser <|> lambda <|> Literal <$> objterm

lambda :: ObjectParser
lambda = curled $ Lambda <$> option [] (try args) <*> mainParser
    where args = many ident <* matchOp "->"

comma = T.comma *> continue
operator = T.operator <* continue
matchOp op = T.matchOp op <* continue

braced = betweenBr '(' ')'
curled = betweenBr '{' '}'

betweenBr op cl = between (brace op *> continue) (brace cl)
continue = optional sep
