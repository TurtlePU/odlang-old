module Parser (block, runParser) where

import Text.Parsec hiding (runParser)

import FlatParse
import Expression
import Sugar
import Token hiding (comma, operator, matchOp)

import qualified Token as T

type OdParser = Parsec [LexemToken] ()
type ExpressionParser = OdParser Expression

runParser :: String -> [LexemToken] -> Either ParseError Block
runParser = parse $ block <* eof

block :: OdParser Block
block = toBlock <$> sugarStmt `sepEndBy` sep
    where sugarStmt = try letExpr <|> try doExpr <|> value <$> chain

letExpr :: OdParser SugarStatement
letExpr = LetExpr <$> binding <*> value
    where binding = matchId "let" *> ident
          value = optionMaybe $ matchOp "=" *> chain

doExpr :: OdParser SugarStatement
doExpr = DoExpr <$> ident <*> binding <* matchOp "<-" <*> methodTail many1
    where binding = squared idents <|> return <$> ident

chain :: ExpressionParser
chain = flatChain <$> infixx <*> many (comma *> methodCall)
    where methodCall = (,) <$> ident <*> methodTail many

infixx :: ExpressionParser
infixx = flatInfix <$> operand <*> many ((,) <$> operator <*> operand)
    where operand = try canonicCall <|> unary
          canonicCall = Call <$> ident <*> index <*> methodTail many

methodTail manyf = cc3 <$> manyf arg <*> option [] named <*> option [] bools
    where arg = (,) <$> ident <*> unary
          named = dupId <$> squared idents
          bools = deBool <$> braced idents
          cc3 a b c = a ++ b ++ c

unary :: ExpressionParser
unary = flatUnary <$> many operator <*> index

index :: ExpressionParser
index = flatIndex <$> recurse <*> many (squared chain)

recurse :: ExpressionParser
recurse = braced chain <|> array <|> lambda <|> TermExpr <$> term

array :: ExpressionParser
array = squared $ Array <$> chain `sepEndBy` sep

lambda :: ExpressionParser
lambda = curled $ Lambda <$> option [] (try args) <*> block
    where args = many ident <* matchOp "->"

idents = ident `sepEndBy` sep

comma = T.comma <* continue
operator = T.operator <* continue
matchOp op = T.matchOp op <* continue

braced = betweenBr '(' ')'
curled = betweenBr '{' '}'
squared = betweenBr '[' ']'

betweenBr op cl = between (brace op *> continue) (continue *> brace cl)
continue = optional sep
