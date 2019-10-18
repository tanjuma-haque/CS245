{- Author: < Vinty(Liwen) Guo and Umme Tanjuma Haque>
   File: Lexer.hs

  -- Lexes the syntax for the Preλ interpreter
-}

module Lexer where

import Data.Char
import Data.List
import Text.Read
import Token
import Syntax

{-

a. if < 2 1 then true else false
[if,<,2,1,then,else,false]

b. hello { there } people
[hello,people]

c. (+12h)
[+,12,h]

d. iff thenn
[iff,thenn]

e. tok33<=
[tok,33,<=]

-}

-- Lex a Preλ expression into a list of tokens
-- Calls `error` if there is a lexical error (something that
-- doesn't lex)
lexPreL :: String -> [Token]
lexPreL input = lexNoPrefix(findToken input)

litDigit :: String -> (String, String)
litDigit "" = ("","")
litDigit (c:cs)
  |isDigit c = (c : fst pair, snd pair)
  |otherwise = ("", c:cs)
    where pair = litDigit cs

lex1 :: Char -> String -> (Token, String)
lex1 c (x:xs)
  |isAlpha c = checkStatements (c:x:xs)
  |isDigit c = (LiteralT (IntegerV (read (fst (litDigit (c:x:xs))))), snd(litDigit (c:x:xs)))
  |( c == '+' || c == '-'|| c == '=' || c == '>' || c == '<' || c ==  '\\' || c == '.' || c == '@' || c == '/' ) = checkOpndKey (c:x:xs)
  |otherwise = error "no token"

lex1 c ""
 |isAlpha c = (VarT [c],"")
 |isDigit c = (LiteralT (IntegerV (read [c] :: Integer) ), "")
 | c == '+' =  (OpT Plus, "")
 | c == '-' = (OpT Minus, "")
 | c == '*' = (OpT Times,"")
 | c == '=' = (OpT Equals, "")
 | c == '\\'= (LambdaT, "")
 | c == '.'= (DotT, "")
 | otherwise = (AppT, "")

checkOpndKey :: String -> (Token, String)
checkOpndKey(c:cx)
  |(length cx >= 1 && ( c == '/' && (head cx) == '=')) = (OpT NotEquals, tail cx)
  |(length cx >= 1 && ( c == '<' && (head cx) == '=')) = (OpT LessThanEquals, tail cx)
  |(length cx >= 1 && ( c == '>' && (head cx) == '=')) = (OpT GreaterThanEquals, tail cx)
  |c == '+'  = (OpT Plus, cx)
  |c == '-'= (OpT Minus , cx)
  |c == '=' = (OpT Equals, cx)
  |c == '>' = (OpT GreaterThan, cx)
  |c == '<' = (OpT LessThan, cx)
  |c == '/' = (OpT Divides, cx)
  |c == '\\' = (LambdaT, cx)
  |c == '.' = (DotT, cx)
  |c == '@' = (AppT, cx)
  |otherwise = error "token error"

checkStatements :: String -> (Token, String)
checkStatements (x:xs)
 | (((length (x:xs) == 5) || ((length (x:xs) > 5) && not (isAlpha (head (drop 5 (x:xs) ) ) ) ) ) && ("false" == take 5 (x:xs))) = ((LiteralT (BoolV False)), drop 5 (x:xs))
 | (((length (x:xs) == 4) || ((length (x:xs) > 4) && not (isAlpha (head (drop 4 (x:xs) ) ) ) ) ) && ("true" == take 4 (x:xs))) = ((LiteralT (BoolV True)), drop 4 (x:xs))
 | (((length (x:xs) == 4) || ((length (x:xs) > 4) && not (isAlpha (head (drop 4 (x:xs) ) ) ) ) ) && ("then" == take 4 (x:xs))) = (ThenT, drop 4 (x:xs))
 | (((length (x:xs) == 4) || ((length (x:xs) > 4) && not (isAlpha (head (drop 4 (x:xs) ) ) ) ) ) && ("else" == take 4 (x:xs))) = (ElseT, drop 4 (x:xs))
 | (((length (x:xs) == 3) || ((length (x:xs) > 3) && not (isAlpha (head (drop 3 (x:xs) ) ) ) ) ) && ("not" == take 3 (x:xs))) = (NotT, drop 3 (x:xs))
 | (((length (x:xs) == 2) || ((length (x:xs) > 2) && not (isAlpha (head (drop 2 (x:xs) ) ) ) ) ) && ("if" == take 2 (x:xs))) = (IfT, drop 2 (x:xs))
 | otherwise = isIdentifierFinal (x:xs)

isIdentifier :: String -> (String, String)
isIdentifier "" = ("", "")
isIdentifier (x:xs)
  |isAlpha x = ((x: fst pair), snd pair)
  |otherwise = ("", x:xs)
      where pair = isIdentifier xs

isIdentifierFinal :: String -> (Token, String)
isIdentifierFinal x = ((VarT (fst(isIdentifier x))), snd(isIdentifier x))

hasRightBracket :: String -> Bool
hasRightBracket "" = False
hasRightBracket x
 |head x == '}' = True
 |otherwise = hasRightBracket (tail x)

findToken :: String -> String
findToken "" = ""
findToken (z:xs)
 | isSpace z = findToken (xs)
 | ( z == '(' || z == ')') = findToken (xs)
 | z == '{' && hasRightBracket(xs) =  findComment xs
 | otherwise = z:xs

--this function is called by findToken if only there is // and this functin calls itself until it finds the end of the comment
-- where it recurs back to findToken to check for any whitespaces after it
findComment :: String -> String
findComment "" = ""
findComment (z:xs)
 | (z == '}' ) = findToken xs
 | otherwise = findComment (xs)

lexNoPrefix :: String -> [Token]
lexNoPrefix [] = []
lexNoPrefix (c:cs) = token : lexPreL rest
  where
   (token, rest) = lex1 c cs
