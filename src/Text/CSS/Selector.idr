module Text.CSS.Selector

import Web.Dom

public export
data Selector : Type where
  Star  : Selector
  Id    : String -> Selector
  Class : String -> Selector
  Elem  : {str : _} -> (0 tpe : ElementType str t) -> Selector

export
render : Selector -> String
render Star           = "*"
render (Id x)         = "#" ++ x
render (Class x)      = "." ++ x
render (Elem {str} _) = str
