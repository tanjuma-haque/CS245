{- Author: <Vinty(Liwen) Guo, Tanjuma Haque>
   File: Parser.hs

   Parses the Preλ syntax
-}

module Parser where

import Data.List
import Token
import Syntax

{-

Expression:
  if Expression then Expression else Expression
  not Expression
	Operator	Expression	Expression
	\ Identifier . Expression
	Identifier
	@ Expression	Expression
	Literal

a. + - 5 4 3

          +
        /  \
       -   3
      / \
     5  4

b. if < 5 3 then true else * 9 3

               if
            /  |   \
           <  true  *
          / \      / \
         5  3     9  3


c. + 5
no parse exists

d. @ \ x . + x 3 5

             @
            / \
           λ   5
          / \
         x  +
           / \
          x  3

-}

-- Parse an expression, returning the parsed expression
-- and a list of unconsumed tokens
-- Calls `error` if the list of tokens has no valid parse.

{-
Write	a	parser	in	Parser.hs,	in	the	parse function. This	function	takes	in	a	list	of
tokens	(as	produced,	for	example,	by	lexPreL)	and	returns	an	expression	and	any
unparsed	tokens.	For	example,	parse [OpT Plus, LiteralT (IntegerV 3),
LiteralT (IntegerV 4), LiteralT (IntegerV 5)] (which	is	what	you	get
when	you	lex	+ 3 4 5)	returns	(OpE Plus (ValueE (IntegerV 3)) (ValueE
(IntegerV 4)), [LiteralT (IntegerV 5)]).	Note	that	this	returns	the
unparsed	5,	because	a	+ takes	only	two	operands.	(2	hours,	about	30	lines)
-}

-- parse (LambdaT : VarT x : rest) = (ValueE (LambdaV x fst(parse rest)), snd (parse rest))

parse :: [Token] -> (Expr, [Token])
parse (VarT string : rest) = (VarE string, rest)
parse (LiteralT value : rest) = (ValueE value, rest)
parse (NotT : rest) = (NotE (fst (parse rest)), snd (parse rest))
parse (AppT : rest) = (AppE (fst(parse rest)) (fst(parse (snd((parse rest))))), snd(parse (snd(parse rest))))
parse (OpT op : rest) = (OpE op (fst (parse rest)) (fst (parse (snd (parse rest)))), (snd (parse (snd (parse rest)))))



parse (IfT : rest1)
  |hasThen rest1 && hasElse (snd (takeThen rest1)) = (IfE (fst (parse (fst(takeThen rest1)))) (fst (parse (fst(takeElse(snd(takeThen rest1)))))) (fst (parse (snd(takeElse(snd(takeThen rest1)))))), snd (parse (snd(takeElse(snd(takeThen rest1))))))
  |otherwise = error "error"
parse _ = error "error"


hasThen :: [Token] -> Bool
hasThen [] = False
hasThen x
  |show (head x) == "ThenT" = True
  |otherwise = hasThen (tail x)

hasElse :: [Token] -> Bool
hasElse [] = False
hasElse x
  |show (head x) == "ElseT" = True
  |otherwise = hasElse (tail x)

takeThen :: [Token] -> ([Token],[Token])
takeThen x
  |show(last x) == "ThenT" = (init x,[])
  |otherwise = (fst(takeThen (init x)),snd(takeThen (init x))++[last x])

takeElse :: [Token] -> ([Token],[Token])
takeElse x
  |show(last x) == "ElseT" = (init x,[])
  |otherwise = (fst(takeElse (init x)),snd(takeElse (init x))++[last x])

-- makeOp ::
