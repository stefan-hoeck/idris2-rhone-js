module Experimental.Event

import Web.Dom

data RawEv : Type where
  RawClick : MouseEvent -> RawEv

data UIEv : Type where
  Click : (id : String) -> UIEv
