module Experimental.Syntax

import JS
import Data.MSF

data UIEvent : Type where

record Dom (a : Type) where

-- ui : Dom (MSF IO UIEvent ())
-- ui = do
--   [btnPlus, btnMinus, txt] <-
--     at body [ button "+", button "-", label "output: ", div ]
--   pure $   on (click btnPlus 1) <|> on (click btnMinus) (-1)
--        >>> scan (+) 0
--        >>> dispCount
--        >>> text txt


