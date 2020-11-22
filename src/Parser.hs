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
    where binding = braced (ident `sepEndBy` comma) <|> return <$> ident

chain :: ExpressionParser
chain = flatChain <$> infixx <*> many (comma *> methodCall)
    where methodCall = (,) <$> ident <*> methodTail many

infixx :: ExpressionParser
infixx = flatInfix <$> operand <*> many ((,) <$> operator <*> operand)
    where operand = try canonicCall <|> unary
          canonicCall = Call <$> ident <*> recurse <*> methodTail many

methodTail manyf = cc3 <$> manyf arg <*> option [] named <*> option [] bools
    where arg = (,) <$> ident <*> unary
          named = squared $ dupId <$> ident `sepEndBy` comma
          bools = braced $ deBool <$> ident `sepEndBy` comma
          cc3 a b c = a ++ b ++ c

unary :: ExpressionParser
unary = flatUnary <$> many operator <*> recurse

recurse :: ExpressionParser
recurse = braced chain <|> array <|> lambda <|> TermExpr <$> term

array :: ExpressionParser
array = squared $ Array <$> infixx `sepEndBy` comma

lambda :: ExpressionParser
lambda = curled $ Lambda <$> option [] (try args) <*> block
    where args = many ident <* matchOp "->"

comma = T.comma <* continue
operator = T.operator <* continue
matchOp op = T.matchOp op <* continue

braced = betweenBr '(' ')'
curled = betweenBr '{' '}'
squared = betweenBr '[' ']'

betweenBr op cl = between (brace op *> continue) (continue *> brace cl)
continue = optional sep
