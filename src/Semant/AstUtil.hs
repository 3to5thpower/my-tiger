module Semant.AstUtil where

import Parse.Data
import qualified Semant.Types as T

isDoubleArgsOpeExp :: Exp -> Bool
isDoubleArgsOpeExp exp = case exp of
  Plus _ _ -> True
  Minus _ _ -> True
  Times _ _ -> True
  Div _ _ -> True
  Less _ _ -> True
  LessEqual _ _ -> True
  Greater _ _ -> True
  GreaterEqual _ _ -> True
  Equal _ _ -> True
  NotEqual _ _ -> True
  And _ _ -> True
  Or _ _ -> True
  _ -> False

isOpeExp :: Exp -> Bool
isOpeExp exp = case exp of
  e | isDoubleArgsOpeExp e -> True
  Negate e -> True
  _ -> False
