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
           , Token.reservedOpNames = [",", ";", "->", ":-"]
           }

lexer = Token.makeTokenParser languageDef

parseString :: Parser a -> String -> Either ParseError a
parseString p =
  parse (do r <- p; eof; return r) ""

identifier :: Parser String
identifier = Token.identifier lexer

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
squares = Token.squares lexer
symbol = Token.symbol lexer
dot = Token.dot lexer

atom :: Parser Atom
atom = do
  head <- identifier
  args <- many parseAtomArg
  return Atom {atomHead = head, atomArgs = args}

parseAtomArg :: Parser (Either Atom String)
parseAtomArg = 
  fmap Left (do
  head <- identifier
  return Atom {atomHead = head, atomArgs = []}
            ) <|>
  fmap Left list <|>
  fmap Right var <|>
  manyBrackets (fmap Left atom <|> fmap Left list <|> fmap Right var)

manyBrackets :: Parser a -> Parser a
manyBrackets p = brackets (manyBrackets p) <|> p

cons :: Either Atom String -> Either Atom String -> Atom
cons x y = Atom "cons" [x, y]

nil :: Atom
nil = Atom "nil" []

list :: Parser Atom
list = squares listInternal where
  parseListElem = fmap Left atom <|> fmap Right var <|> fmap Left list
  parseStandardListTail = do symbol ","; fmap (foldr (\x y -> Left $ cons x y) (Left nil)) $ sepBy1 parseListElem $ symbol ","
  parseVerticalLineListTail = do symbol "|"; fmap Right var
  listInternal = (do
    head <- parseListElem
    tail <- parseStandardListTail <|> parseVerticalLineListTail <|> return (Left nil)
    return $ cons head tail
                 ) <|> return nil

relation :: Parser Relation
relation = do
  head <- atom
  body <- parseDisjOrNothing
  dot
  return Relation {relHead = head, relBody = body}

parseDisjOrNothing :: Parser (Maybe RelationBody)
parseDisjOrNothing = 
  fmap Just (do 
    reservedOp ":-"
    parseDisj
            ) <|> 
  return Nothing

parseDisj :: Parser RelationBody
parseDisj = fmap (foldr1 Disj) $ sepBy1 parseConj (reservedOp ";")

parseConj :: Parser RelationBody
parseConj = fmap (foldr1 Conj) $ sepBy1 parseBodyElem (reservedOp ",")

parseBodyElem :: Parser RelationBody
parseBodyElem = fmap RAtom atom <|> brackets parseDisj

word :: String -> Parser String
word s = do
  word <- string s
  many1 space
  return word

parseModule :: Parser String
parseModule = do
  word "module"
  name <- identifier
  dot
  return name

typeExpr :: Parser Type
typeExpr = fmap (foldr1 Arrow) $ sepBy1 parseTypeElem (reservedOp "->")

parseTypeElem :: Parser Type
parseTypeElem = fmap TAtom atom <|> fmap Var var <|> brackets typeExpr

typ :: Parser TypeDef
typ = do
  spaces
  word "type"
  name <- identifier
  t <- typeExpr
  dot
  return $ TypeDef name t

prog :: Parser PrologProgram
prog = do
  spaces
  m <- try (fmap Just parseModule) <|> return Nothing
  ts <- try (many1 typ) <|> return []
  rs <- many relation
  return Program {pModule = m, types = ts, rels = rs}








