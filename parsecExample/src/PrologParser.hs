module PrologParser where

import Control.Monad
import Data.Char (isLower, isUpper)
import PrologAst
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef
    { Token.identStart = lower,
      Token.identLetter = alphaNum <|> char '_',
      Token.reservedNames = ["module", "type"],
      Token.reservedOpNames = [",", ";", "->", ":-"]
    }

lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = do
  i <- Token.identifier lexer
  guard $ isLower $ head i
  return i

var :: Parser [Char]
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (h : t)

whiteSpace = Token.whiteSpace lexer

reservedOp = Token.reservedOp lexer

reserved = Token.reserved lexer

brackets = Token.parens lexer

dot = Token.dot lexer

comma = Token.comma lexer

semi = Token.semi lexer

cork = reservedOp ":-"

many1Brackets elem = brackets $ many1Brackets elem <|> elem

manyBrackets elem = many1Brackets elem <|> elem

parseList elem sep = do
  h <- elem
  t <- many (sep >> elem)
  return (h : t)

atom :: Parser Atom -- errors might be 'expected EOF'
atom = try complexAtom <|> try simpleAtom

simpleAtom :: Parser Atom
simpleAtom = do
  head <- try identifier
  return $ Atom head []

complexAtom :: Parser Atom
complexAtom = do
  head <- try identifier
  args <- many1 $ fmap Left (many1Brackets atom <|> simpleAtom) <|> fmap Right var
  return $ Atom head args

relation :: Parser Relation
relation =
  try
    (do head <- atom; dot; return $ Relation head Nothing)
    <|> ( do
            head <- atom
            cork
            body <- relationBodyDisj
            dot
            return $ Relation head $ Just body
        )

relationBodyDisj =
  fmap (foldr1 Disj) $ parseList relationBodyConj $ reservedOp ";"

relationBodyConj =
  fmap (foldr1 Conj) $ parseList relationBodyExpr $ reservedOp ","

relationBodyExpr =
  fmap RAtom (try atom) <|> brackets relationBodyDisj

parseModule :: Parser String
parseModule = do spaces; reserved "module"; name <- identifier; dot; return name

typeExpr :: Parser Type -- "->" is infixr
typeExpr =
  let ttt =
        try (fmap TAtom atom)
          <|> try (fmap Var var)
          <|> brackets (try typeExpr)
   in try
        ( do
            typ1 <- ttt
            reservedOp "->"
            typ2 <- typeExpr
            return $ Arrow typ1 typ2
        )
        <|> try ttt

typ :: Parser TypeDef
typ = do
  reserved "type"
  name <- identifier
  ty <- typeExpr
  dot
  return $ TypeDef name ty

prog :: Parser PrologProgram
prog = undefined

parseProgram :: String -> Either ParseError PrologProgram
parseProgram =
  parse (do r <- prog; eof; return r) ""

-- parseNOW :: String -> Either ParseError Type
-- parseNOW =
--   parse (do r <- typeExpr; eof; return r) ""
