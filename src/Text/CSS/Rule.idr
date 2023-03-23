module Text.CSS.Rule

import Data.String
import Text.CSS.Declaration
import Text.CSS.Selector
import Web.Dom

%default total

public export
data Rule : (n : Nat) -> Type where
  Sel :
       {0 selDepth : Nat}
    -> {0 hasPseudoClass, hasPseudoElem : Bool}
    -> (selector : Selector selDepth hasPseudoClass hasPseudoElem)
    -> (decls    : List Declaration)
    -> Rule n

  Media :
       (query : String)
    -> (rules : List $ Rule 0)
    -> Rule 1

export %inline
sel : Selector 0 True True -> List Declaration -> Rule n
sel = Sel

export %inline
class : String -> List Declaration -> Rule n
class s = sel (Class s)

export
classes : List String -> List Declaration -> Rule n
classes cs = Sel (Many $ map Class cs)

export %inline
elem : {str : _} -> (0 tpe : ElementType str t) -> List Declaration -> Rule n
elem v = sel (Elem v)

export %inline
id : String -> List Declaration -> Rule n
id s = sel (Id s)

export %inline
pseudo : Selector 0 False False -> PseudoClass -> List Declaration -> Rule n
pseudo x y = sel (Pseudo x y)

export %inline
star : List Declaration -> Rule n
star = sel Star

export
Interpolation (Rule n) where
  interpolate (Sel s ds)    = "\{s}{\{concat $ map interpolate ds}}"
  interpolate (Media q rs)  = "@media (\{q}){\{unlines $ map interpolate rs}}"
