module Rhone.JS.Input

import Control.Monad.Dom
import Data.MSF
import JS
import Rhone.JS.Sink
import Web.Dom
import Web.Html

fireAndHold : o -> MSF m (Event o) (NP I [o, Event o])
fireAndHold v = fan [hold v, id <|> once v]

export
input :  LiftJSIO m
      => (getInput : ev -> Event String)
      -> (read     : String -> Either String a)
      -> (ref      : ElemRef HTMLInputElement)
      -> MSF m ev (Either String a)
input get read ref =
      map read . get
  ^>> fireAndHold (read "")
  >>> par [id, ifEvent (leftInvalid ref)]
  >>> hd
