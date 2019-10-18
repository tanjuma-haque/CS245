{- Author: <Vinty(Liwen) Guo, Tanjuma Haque>
   File: Eval.hs

   Defines an evaluator for Preλ
-}

module Eval where

import Syntax

-- Evaluate an expression to a value. Calls `error` if
-- this is impossible.
eval :: Expr -> Value
eval (ValueE (IntegerV a)) = (IntegerV a)
eval (ValueE (BoolV boo)) = (BoolV boo)
eval (VarE variable) = error "error"
-- eval (OpE NotEquals (ValueE (IntegerV x)) (ValueE (IntegerV y))) = performOp NotEquals x y
eval (NotE expr)
  |(BoolV bl) <- eval expr
  = BoolV (not bl)
  |otherwise = error "error"

eval (IfE expr1 expr2 expr3)
  |(BoolV True) <- eval expr1
  = eval expr2
  |(BoolV False) <- eval expr1
  = eval expr3
  |otherwise = error "error"

eval (OpE op (ValueE(IntegerV a)) (ValueE(IntegerV b))) = performOp op a b
eval (OpE op expr1 (ValueE(IntegerV b))) = performOp op (eval expr1) b

-- eval (OpE Plus (OpE Minus (ValueE(IntegerV 5)) (ValueE(IntegerV 4))) (ValueE(IntegerV 3)))


  -- eval AppE expr1 expr2
  --  |ValueE(LambdaV )


-- All binary operators take two Integer arguments. This
-- function performs the operation on the arguments, returning
-- a Value.
performOp :: Op -> Integer -> Integer -> Value
performOp Plus x y = IntegerV (x + y)
performOp Minus x y = IntegerV (x - y)
performOp Times x y = IntegerV (x * y)
performOp Divides x y = IntegerV (x `div` y)
performOp LessThan x y = BoolV (x < y)
performOp LessThanEquals x y = BoolV (x <= y)
performOp GreaterThan x y = BoolV (x > y)
performOp GreaterThanEquals x y = BoolV (x >= y)
performOp Equals x y = BoolV (x == y)
performOp NotEquals x y = BoolV (x /= y)

-- Substitute a value into an expression
-- If you want (expr)[x := val], call (subst expr "x" val)
subst :: Expr -> String -> Value -> Expr
subst (VarE v) str value
  |v == str = ValueE value
  |otherwise = VarE v
subst (ValueE (LambdaV string expr)) str value
  |string == str = subst expr str value
  |otherwise = ValueE (LambdaV string (subst expr str value))
subst (ValueE (IntegerV int)) str value = ValueE (IntegerV int)
subst (ValueE (BoolV boo)) str value = ValueE (BoolV boo)
subst (NotE expr) str value = NotE (subst expr str value)
subst (AppE expr1 expr2) str value = AppE (subst expr1 str value) (subst expr2 str value)
subst (OpE op expr1 expr2) str value = OpE op (subst expr1 str value) (subst expr2 str value)
subst (IfE expr1 expr2 expr3) str value = IfE (subst expr1 str value) (subst expr2 str value) (subst expr3 str value)
