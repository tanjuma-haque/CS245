{- Author: <Vinty(Liwen) Guo, Tanjuma Haque>
   File: Syntax.hs

   Basic definitions for the abstract syntax tree.
-}

module Syntax where

{-

a. + - 5 4 3
OpE Plus (OpE Minus (ValueE (IntegerV 5)) (ValueE (IntegerV 4))) (ValueE 3)

b. if < 5 3 then true else * 9 3
IfE (OpE LessThan (ValueE (IntegerV 5)) (ValueE (IntegerV 3)) ) (ValueE (BoolV true)) (OpE Times (ValueE (IntegerV 9)) (ValueE (IntegerV 3)))

c. + 5
no parse exists

d. @ \ x . + x 3 5
AppE (LambdaV "x" (OpE (VarE "x") (ValueE (IntegerV 3)) ) (ValueE (IntegerV 5))

-}

-- Values cannot be evaluated further.
data Value
  = IntegerV Integer     -- Integers, like 5 and 674
  | BoolV Bool           -- Booleans, like true and false
  | LambdaV String Expr  -- Lambda expressions (which are values,
                         -- as you can't evaluate them further)
  deriving Show

-- Binary operators
data Op
  = Plus               -- +
  | Minus              -- -
  | Times              -- *
  | Divides            -- /
  | LessThan           -- <
  | LessThanEquals     -- <=
  | GreaterThan        -- >
  | GreaterThanEquals  -- >=
  | Equals             -- =
  | NotEquals          -- /=
  deriving Show

-- Expressions
data Expr
  = IfE Expr Expr Expr   -- doneif e1 then e2 else e3
  | OpE Op Expr Expr     -- done e.g. + e1 e2
  | NotE Expr            -- done not e1
  | ValueE Value         -- done  values are expressions, too
  | VarE String          -- done  e.g. x
  | AppE Expr Expr       --  @ e1 e2
                         -- (separate from other binary operators because
                         -- @ evaluates so differently)
  deriving Show
