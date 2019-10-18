{- Author: <Vinty(Liwen) Guo, Tanjuma Haque>
   File: Token.hs

   Defines lexical tokens.
-}

module Token where

import Syntax

{-

a. + - 5 4 3
[OpE Plus, OpE Minus, LiteralT(IntegerV 5), LiteralT(IntegerV 4), LiteralT(IntegerV 3)]

b. if < 5 3 then true else * 9 3
[IfT, OpT LessThan, LiteralT(IntegerV 5), LiteralT(IntegerV 3), ThenT, LiteralT(BoolV true), ElseT, OpE Times, LiteralT(IntegerV 9), LiteralT(IntegerV 3)]

c. + 5
no parse exists

d. @ \ x . + x 3 5
[AppT, LambdaT, VarT "x", DotT, OpE Plus, VarT "x", LiteralT(IntegerV 3)], LiteralT(IntegerV 5)]

-}


data Token
  = LiteralT Value     -- numbers, booleans
  | IfT                -- "if"
  | ThenT              -- "then"
  | ElseT              -- "else"
  | OpT Op             -- binary operators
  | NotT               -- done "not"
  | LambdaT            -- "\"
  | VarT String        -- done e.g. "x" "y"
  | DotT               -- "."
  | AppT               -- "@"
  deriving Show
