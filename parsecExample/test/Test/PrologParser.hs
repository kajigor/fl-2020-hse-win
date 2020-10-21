module Test.PrologParser where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Text.ParserCombinators.Parsec
import Data.Either (isLeft)

import PrologParser
import PrologAst

testParserSuccess :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParserSuccess p inp exp =
  parseString p inp @?= Right exp

testParserFailure :: (Eq a, Show a) => Parser a -> String -> Assertion
testParserFailure p inp =
  assertLeft $ parseString p inp

assertLeft :: (Show a, Show b) => Either a b -> Assertion
assertLeft x =
  assertBool ("expected: Left\n but got: " ++ show x) (isLeft x)

unit_ident :: Assertion
unit_ident = do
  let parser = identifier
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "abc" "abc"
  success "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
  success "type\n" "type"
  success "module\n\t " "module"
  fail "123abc"
  fail "Xyz"

unit_var :: Assertion
unit_var = do
  let parser = var
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "Abc" "Abc"
  success "H\n" "H"
  success "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
  fail "123abc"
  fail "xyz"
  fail "module"
  fail "type"

unit_manyIdent :: Assertion
unit_manyIdent = do
  let parser = many identifier
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a b c" ["a", "b", "c"]

a = Atom "a"
b = Atom "b"
c = Atom "c"
d = Atom "d"
t = Atom "type"
m = Atom "module"

a' = a []
b' = b []
c' = c []
d' = d []
t' = t []
m' = m []

unit_atom :: Assertion
unit_atom = do
  let parser = atom
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" a'
  success "type" t'
  success "a b c" (a [l b', l c'])
  success "a (b c)" (a [l $ b [l c']])
  success "a ((b c))" (a [l $ b [l c']])
  success "a ((b c)) d" (a [l $ b [l c'], l d'])
  success "a ((b c))  (d)" (a [l $ b [l c'], l d'])
  success "a ((b  c))  (d)" (a [l $ b [l c'], l d'])
  success "a ((b  c) )  ( d )" (a [l $ b [l c'], l d'])
  success "a((b c))(d)" (a [l $ b [l c'], l d'])
  success "cons H T" (Atom "cons" [r "H", r "T"])
  success "cons (cons a H) T" (Atom "cons" [l $ Atom "cons" [l a', r "H"], r "T"])
  success "a [ \n]" (a [l $ nil])
  success "a [b]" (a [l $ cons (l b') (l $ nil)])
  success "a [b,  c]" (a [l $ cons (l b') (l $ cons (l c') (l $ nil))])
  success "a (( [b,  c]))" (a [l $ cons (l b') (l $ cons (l c') (l $ nil))])
  fail "a (a"
  fail "X a"
  fail "(a)"
  fail "[a] b c"
  fail "[a, b]"

l = Left
r = Right

unit_relation :: Assertion
unit_relation = do
  let parser = relation
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a." (Relation a' Nothing)
  success "a b." (Relation (a [l b']) Nothing)
  success "a:-a." (Relation a' (Just (RAtom a')))
  success "a :-a." (Relation a' (Just (RAtom a')))
  success "a:-a b." (Relation a' (Just (RAtom (a [l b']))))
  success "a b:- (a b)  ." (Relation (a [l b']) (Just (RAtom (a [l b']))))
  success "a b:- a;b,c." (Relation (a [l b']) (Just (Disj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a b:- a;(b,c)." (Relation (a [l b']) (Just (Disj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a b:- (a;b),c." (Relation (a [l b']) (Just (Conj (Disj (RAtom a') (RAtom b')) (RAtom c'))))
  success "a b:- a;b;c." (Relation (a [l b']) (Just (Disj (RAtom a') (Disj (RAtom b') (RAtom c')))))
  success "a b:- a,b,c." (Relation (a [l b']) (Just (Conj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a (b (c))  :- (a b) ." (Relation (a [l $ b [l c']]) (Just (RAtom (a [l b']))))
  success "module [module]." (Relation (m [l $ cons (l $ m') (l $ nil)]) Nothing)
  fail "a :- a"
  fail "a :- ."
  fail ":- a."
  fail "f : - a. "
  fail "X :- a."

unit_typeExpr :: Assertion
unit_typeExpr = do
  let parser = typeExpr
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" (TAtom a')
  success "Y -> X" (Arrow (Var "Y") (Var "X"))
  success "(Y -> X)" (Arrow (Var "Y") (Var "X"))
  success "(A -> B) -> C" (Arrow (Arrow (Var "A") (Var "B")) (Var "C"))
  success "A -> B -> C" (Arrow (Var "A") (Arrow (Var "B") (Var "C")))
  success "list (list A) -> list A -> o" (Arrow (TAtom (Atom "list" [l $ Atom "list" [r "A"]])) (Arrow (TAtom (Atom "list" [r "A"])) (TAtom (Atom "o" []))))
  success "pair A B -> (A -> C) -> (B -> D) -> pair C D"
          ( Arrow
              (TAtom $ Atom "pair" [r $ "A", r $ "B"])
              (Arrow
                (Arrow
                  (Var "A")
                  (Var "C")
                )
                (Arrow
                  (Arrow
                    (Var "B")
                    (Var "D")
                  )
                  (TAtom $ Atom "pair" [r $ "C", r $ "D"])
                )
              )
          )


unit_type :: Assertion
unit_type = do
  let parser = typ
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "type a b." (TypeDef "a" (TAtom b'))
  success "type a b -> X." (TypeDef "a" (Arrow (TAtom b') (Var "X")))
  success "type filter (A -> o) -> list a -> list a -> o." (TypeDef "filter" (Arrow (Arrow (Var "A") (TAtom (Atom "o" []))) (Arrow (TAtom (Atom "list" [l $ Atom "a" []])) (Arrow (TAtom $ Atom "list" [l $ Atom "a" []]) (TAtom (Atom "o" []))))))
  success "type filter (A -> o) -> list A -> list A -> o." (TypeDef "filter" (Arrow (Arrow (Var "A") (TAtom (Atom "o" []))) (Arrow (TAtom (Atom "list" [r "A"])) (Arrow (TAtom $ Atom "list" [r "A"]) (TAtom (Atom "o" []))))))
  success "type a (((b)))." (TypeDef "a" (TAtom b'))
  success "type d a -> (((b)))." (TypeDef "d" (Arrow (TAtom a') (TAtom b')))
  success "type type type -> type." (TypeDef "type" (Arrow (TAtom t') (TAtom t')))
  success "type type type." (TypeDef "type" (TAtom t'))
  success "type type type [type]." (TypeDef "type" (TAtom (Atom "type" [l $ cons (l $ t') (l $ nil)])))
  success "type module\ntype\t-> module." (TypeDef "module" (Arrow (TAtom t') (TAtom m')))
  fail "type x -> y -> z."
  fail "tupe x o."
  fail "typex o."
  fail "type x."

unit_module :: Assertion
unit_module = do
  let parser = parseModule
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "module\n \t name." "name"
  success "module\nmodule." "module"
  success "module type." "type"
  --success " \t\nmodule\n\n  name_123." "name_123" -- spaces at the beginning of module are considered in prog
  fail "modulo name."
  fail "modulename."
  fail "mod ule name."
  fail "module 123name."
  fail "module name!"

unit_list :: Assertion
unit_list = do
  let parser = list
  let success = testParserSuccess parser
  let fail = testParserFailure parser
  success "[]" (nil)
  success "[a]" (cons (l $ a') (l $ nil))
  success "[module]" (cons (l $ m') (l $ nil))
  success "[A,B]" (cons (r "A") (l $ cons (r "B") (l $ nil)))
  success "[a (b c), B, C]" (cons (l $ a [l $ b [l $ c']]) (l $ cons (r "B") (l $ cons (r "C") (l $ nil))))
  success "[a | T]" (cons (l a') (r "T") )
  success "[ [a] | T ]" (cons (l $ cons (l a') (l $ nil)) (r "T") )
  success "[ [H | T], a ]" (cons (l $ cons (r "H") (r "T")) (l $ cons (l $ a') (l $ nil)) )
  fail "[a | a]"
  fail "[A,B,]"
  fail "[A,B"
  fail "]["
  fail "[ |T]"
  fail "[X|T|Y]"

unit_prog :: Assertion
unit_prog = do
  let parser = prog
  let success = testParserSuccess parser
  let fail = testParserFailure parser
  success "" (Program Nothing [] []) 
  success "\nmodule name.\n" (Program (Just "name") [] [])
  success "\t\n type a b." (Program Nothing [TypeDef "a" (TAtom b')] [])
  success "  a. \n  " (Program Nothing [] [Relation a' Nothing])
  success "\n\n\t module name. type a b. \t\t a.a.\n" (Program (Just "name") [TypeDef "a" (TAtom b')] [Relation a' Nothing, Relation a' Nothing])
  success "\nmodule module.module module." (Program (Just "module") [] [Relation (m [l m']) Nothing])
  success "\nmodule [module].module module." (Program Nothing [] [Relation (m [l $ cons (l $ m') (l $ nil)]) Nothing, Relation (m [l m']) Nothing])
  success "module type. type type type. type type type." (Program (Just "type") [TypeDef "type" (TAtom t'), TypeDef "type" (TAtom t')] [])
  success "module type. type type type. a. type type type." (Program (Just "type") [TypeDef "type" (TAtom t')] [Relation a' Nothing, Relation (t [l t', l t']) Nothing])
  success "type type [type]." (Program Nothing [] [Relation (t [l t', l $ cons (l $ t') (l $ nil)]) Nothing])
  success "type [type]." (Program Nothing [] [Relation (t [l $ cons (l $ t') (l $ nil)]) Nothing])
