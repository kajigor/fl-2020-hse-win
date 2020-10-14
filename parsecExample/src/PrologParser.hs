module PrologParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isLower, isUpper)
import PrologAst

languageDef =
  emptyDef { Token.identStart = lower
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = ["module", "type"]
           , Token.reservedOpNames = [",", ";", "->", ":-"]
           }

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = do
  i <- Token.identifier lexer
  return i

var :: Parser String
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (h:t)

whiteSpace = Token.whiteSpace lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
brackets = Token.parens lexer
dot = Token.dot lexer

atom :: Parser Atom
atom = do
  head <- identifier
  args <- many parseAtomArg
  return Atom {atomHead = head, atomArgs = args}

parseAtomArg :: Parser (Either Atom String)
parseAtomArg = 
  (fmap Left (do
  head <- identifier
  return Atom {atomHead = head, atomArgs = []}
                         )) <|>
  (fmap Right var) <|>
  (manyBrackets (fmap Left atom <|> fmap Right var))

manyBrackets :: Parser a -> Parser a
manyBrackets p = brackets (manyBrackets p) <|> p


relation :: Parser Relation
relation = do
  head <- atom
  body <- parseDisjOrNothing
  dot
  return Relation {relHead = head, relBody = body}

parseDisjOrNothing :: Parser (Maybe RelationBody)
parseDisjOrNothing = 
  (fmap Just (do 
    reservedOp ":-"
    d <- parseDisj
    return d
             )) <|> 
  (do; return Nothing)

parseList :: Parser a -> Parser b -> Parser [a]
parseList elem sep = do
  h <- elem
  t <- many (sep >> elem)
  return (h:t)

parseDisj :: Parser RelationBody
parseDisj = fmap (foldr1 Disj) $ parseList parseConj (reservedOp ";")

parseConj :: Parser RelationBody
parseConj = fmap (foldr1 Conj) $ parseList parseBodyElem (reservedOp ",")

parseBodyElem :: Parser RelationBody
parseBodyElem = fmap RAtom atom <|> brackets parseDisj

parseModule :: Parser String
parseModule = do
  reserved "module"
  name <- identifier
  dot
  return name

typeExpr :: Parser Type
typeExpr = fmap (foldr1 Arrow) $ parseList parseTypeElem (reservedOp "->")

parseTypeElem :: Parser Type
parseTypeElem = fmap TAtom atom <|> fmap Var var <|> brackets typeExpr

typ :: Parser TypeDef
typ = do
  reserved "type"
  name <- identifier
  t <- typeExpr
  dot
  return $ TypeDef name t

prog :: Parser PrologProgram
prog = undefined
