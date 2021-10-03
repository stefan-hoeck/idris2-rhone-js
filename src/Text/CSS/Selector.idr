module Text.CSS.Selector

import Data.List
import Data.String
import Web.Dom

%default total

public export
data Selector :  (dept : Nat)
              -> (hasPseudoClass : Bool)
              -> (hasPseudoElem  : Bool)
              -> Type where
  Star  : Selector 0 b1 b2
  Id    : String -> Selector 0 b1 b2
  Class : String -> Selector 0 b1 b2
  Elem  : {str : _} -> (0 tpe : ElementType str t) -> Selector 0 b1 b2
  Many  : List (Selector 0 True True) -> Selector 1 True True

export %inline
class : String -> Selector 0 False False
class = Class

export %inline
classes : List String -> Selector 1 True True
classes = Many . map Class

export %inline
elem : {str : _} -> (0 tpe : ElementType str t) -> Selector 0 False False
elem = Elem

export %inline
id : String -> Selector 0 False False
id = Id

export
render : Selector n b1 b2 -> String
render Star           = "*"
render (Id x)         = "#" ++ x
render (Class x)      = "." ++ x
render (Elem {str} _) = str
render (Many ss)      = fastConcat . intersperse ", " $ map render ss
