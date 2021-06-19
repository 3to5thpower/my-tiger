module Semant.TransDec where

import Parse.Data (Dec)
import Semant.Types as T (TEnv, VEnv)

transDec :: T.VEnv -> T.TEnv -> [Dec] -> Either String (T.VEnv, T.TEnv)