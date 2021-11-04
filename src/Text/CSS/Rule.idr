module Text.CSS.Rule

import Data.String
import Text.CSS.Declaration
import Text.CSS.Render
import Text.CSS.Selector

%default total

public export
data Rule : (n : Nat) -> Type where
  MkRule :  (selector : Selector selDepth hasPseudoClass hasPseudoElem)
         -> (decls    : List Declaration)
         -> Rule n

  Media :   (query : String)
         -> (rules : List $ Rule 0)
         -> Rule 1

infix 2 !!

export %inline
(!!) : Selector n b1 b1 -> List Declaration -> Rule k
(!!) = MkRule

export
Render (Rule n) where
  render (MkRule s ds) = "\{render s}{\{fastConcat $ map render ds}}"
  render (Media q rs)  = "@media (\{q}){fastUnlines $ map render rs}"
