module Main where

import Autograder
    ( HW(..),
      Solution(..),
      ParseError(..),
      LexError(..),
      Expr(..),
      Op(Div, Add, Sub, Mul),
      Token(..),
      parse,
      autograde )
import Data.Maybe
import Debug.Trace
import System.Environment
import Text.Read

{-
 - Note: these types are defined in Autograder.hs and should be used in
 - completing the undefined functions below.
 -
 - data Token = LPar | RPar | Literal String | Plus | Minus | Asterisk | Slash deriving (Eq, Show)
 - data Op = Add | Sub | Mul | Div deriving (Eq, Show)
 - data Expr = Binary Op Expr Expr | Val Double deriving (Eq, Show)
 - data RunError = MismatchedParentheses | LexFailure LexError | ParseFailure ParseError deriving (Eq, Show)
 - newtype LexError = UnknownToken String deriving (Eq, Show)
 - newtype ParseError = SyntaxError String deriving (Eq, Show)
 -}

-- TODO: put your name here
studentName = "Laura Flores"

instance HW Solution where
  name a = studentName
  parseSol a = Main.parse
  lexSol a = Main.lex
  validParensSol a = validParens
  evalSol a = eval
  runSol a = run

{-
 - run executes the input expression and returns the result as a string if the
 - given expression is valid. Otherwise, run returns a string error message. 
 -}
run :: String -> String
run s =  if (isDouble s) then s
         else if (validParens s) then step1 s
         else "Invalid parentheses."
  where step1 s = case (Main.lex s) of
          Left (UnknownToken msg) -> msg -- if the string can't be lexed, show an UnknownToken message.
          Right msg -> step2 msg -- if the string can be lexed, send it to parse.
          where step2 s = case (Main.parse s) of 
                  Left (SyntaxError msg) -> msg -- if the tokens can't be parsed, show a SyntaxError message.
                  Right msg -> show (eval msg) -- otherwise, evaluate the parsed expression.

{-
 - evaluates the given expression, which is assumed to be valid.
 -}
eval :: Expr -> Double
eval (Val d) = d
eval (Binary op (Val d1) (Val d2))
  | (op == Add) = d1 + d2
  | (op == Sub) = d1 - d2
  | (op == Mul) = d1 * d2
  | (op == Div) = d1 / d2
eval (Binary op e1 e2)
  | (op == Add) = eval(e1) + eval(e2)
  | (op == Sub) = eval(e1) - eval(e2)
  | (op == Mul) = eval(e1) * eval(e2)
  | (op == Div) = eval(e1) / eval(e2)

-- ///////////////////////////////////////////////
isDouble :: String -> Bool
isDouble x = isJust (readMaybe x :: Maybe Double)
-- ///////////////////////////////////////////////

{-
 - Checks whether the input string contains balanced parentheses. 
 -}
validParens :: String -> Bool
validParens s = balancedParens (filterParens s)
  where filterParens s = Prelude.filter (\x -> (x == ')') || (x == '(')) s
        balancedParens "" = False
        balancedParens s = helper s []
          where helper "" [] = True
                helper "" _ = False
                helper (s:ss) stack
                  | (s == '(') = helper ss ([s] ++ stack)
                  | stack == [] = False
                  | ((head stack) == '(') && (s /= ')') = False
                  | otherwise = helper ss (tail stack)

{-
 - Lexes the input string, returning either a LexError or a list of tokens. 
 -}
lex :: String -> Either LexError [Token]
lex "" = Right []
lex (s:ss) = helper (s:ss) []
  where helper "" tokenList = Right tokenList
        helper (s:ss) tokenList
          | (s == ' ') = helper ss tokenList -- skip over spaces
          | (s == '(') = helper ss (tokenList ++ [LPar])
          | (s == ')') = helper ss (tokenList ++ [RPar])
          | ((isDouble p) == True) = helper (drop (length p) (s:ss)) (tokenList ++ [Literal p]) -- gatherNum needs to return a Right or Left.
          | (s == '+') = helper ss (tokenList ++ [Plus])
          | (s == '-') = 
              if ((isDouble d) == True) then helper (drop ((length d)+1) ss) (tokenList ++ [Literal ("-"++d)])
              else helper ss (tokenList ++ [Minus])
          | (s == '*') = helper ss (tokenList ++ [Asterisk])
          | (s == '/') = helper ss (tokenList ++ [Slash])
          | otherwise = Left (UnknownToken "There was an invalid token.")
          where gatherNum "" acc = acc -- stops if there is no parenthesis at the end
                gatherNum (s:ss) acc
                  |(s == ' ') || (s == ')') = acc -- stop if you hit a space
                  |((isDouble p) == True) = gatherNum ss (acc ++ p) -- add a number to the accumulator
                  |(s == '.') = gatherNum ss (acc ++ decStr) -- add a decimal point to the accumulator
                  |otherwise = acc
                  where p = chop 1 1 (show s)
                        decStr = chop 1 1 (show '.')
                        chop from to x = drop from (take (to+1) x)
                p = gatherNum (s:ss) ""
                d = gatherNum ss ""

{-
 - Parses the token list and returns either a ParseError or the parsed expression. 
 -}
parse :: [Token] -> Either ParseError Expr 
parse [] = Left (SyntaxError "Parse error.") -- error if you enter an empty list
parse [Literal x] = Right (Val (read x::Double))
parse (t:ts) = helper (reverse (t:ts)) []
  where helper [] [] = Left (SyntaxError "Parse error.") -- case for all parentheses entered
        helper [] [expr] = Right (expr)
        helper (t:ts) exprList -- skips over parentheses
          |(t == LPar) || (t == RPar) = helper ts exprList
        helper (Literal l1 : Literal l2 : op : ts) exprList -- creates a small expression -- 2 positive numbers
          |(op == Plus)     = helper ts ([(Binary Add (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Minus)    = helper ts ([(Binary Sub (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Asterisk) = helper ts ([(Binary Mul (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Slash)    = helper ts ([(Binary Div (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
        helper (Literal l1 : LPar : RPar : Literal l2 : LPar: op : ts) exprList -- creates a small expression -- 2 negative numbers
          |(op == Plus)     = helper ts ([(Binary Add (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Minus)    = helper ts ([(Binary Sub (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Asterisk) = helper ts ([(Binary Mul (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Slash)    = helper ts ([(Binary Div (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
        helper (Literal l1 : LPar : Literal l2 : op : ts) exprList -- creates a small expression -- pos, then neg numbers
          |(op == Plus)     = helper ts ([(Binary Add (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Minus)    = helper ts ([(Binary Sub (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Asterisk) = helper ts ([(Binary Mul (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Slash)    = helper ts ([(Binary Div (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
        helper (Literal l1 : RPar : Literal l2 : LPar: op : ts) exprList -- creates a small expression -- neg, then pos numbers
          |(op == Plus)     = helper ts ([(Binary Add (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Minus)    = helper ts ([(Binary Sub (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Asterisk) = helper ts ([(Binary Mul (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
          |(op == Slash)    = helper ts ([(Binary Div (Val (read l2::Double)) (Val (read l1::Double)))] ++ exprList)
        helper (op : LPar : ts) exprList -- creates a large expression from two small expressions
          |(op == Plus)     = helper ts ([Binary Add (exprList !! 0) (exprList !! 1)] ++ (drop 2 exprList))
          |(op == Minus)    = helper ts ([Binary Sub (exprList !! 0) (exprList !! 1)] ++ (drop 2 exprList))
          |(op == Asterisk) = helper ts ([Binary Mul (exprList !! 0) (exprList !! 1)] ++ (drop 2 exprList))
          |(op == Slash)    = helper ts ([Binary Div (exprList !! 0) (exprList !! 1)] ++ (drop 2 exprList))
        helper (Literal l : ts) exprList = helper ts ([Val (read l::Double)] ++ exprList) -- adds just a Val to the stack
        helper _ _ = Left (SyntaxError "Parse error.") -- handles any other case

main = do
  let s = Student studentName
  args <- getArgs
  let exclusions = Autograder.parse args
  autograde s exclusions
