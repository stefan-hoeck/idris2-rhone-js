module Rhone.JS.Input

import Data.MSF
import JS
import Rhone.JS.ElemRef
import Rhone.JS.Sink
import Rhone.JS.Source
import Web.Dom
import Web.Html

fireAndHold : o -> MSF m (Event o) (HList [o, Event o])
fireAndHold v = fan [hold v, id <|> once v]

export
input :
     (getInput : ev -> Event String)
  -> (read     : String -> Either String a)
  -> (ref      : ElemRef HTMLInputElement)
  -> MSF JSIO ev (Either String a)
input get read ref =
      map read . get
  ^>> fireAndHold (read "")
  >>> par [id, ifEvent (leftInvalid ref)]
  >>> hd

export
getInput :
     Eq ev
  => ev
  -> (read     : String -> Either String a)
  -> (ref      : ElemRef HTMLInputElement)
  -> MSF JSIO ev (Either String a)
getInput e read ref =
      bool (e ==)
  >>> collect [valueOf ref >>^ Ev . read, never]
  >>> fireAndHold (read "")
  >>> par [id, ifEvent (leftInvalid ref)]
  >>> hd
