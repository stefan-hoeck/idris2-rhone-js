module Text.CSS.Rule

import Data.String
import Text.CSS.Declaration
import Text.CSS.Selector

%default total

public export
record Rule where
  constructor MkRule
  selector : Selector selDepth hasPseudoClass hasPseudoElem
  decls    : List Declaration

infix 2 !!

export %inline
(!!) : Selector n b1 b1 -> List Declaration -> Rule  
(!!) = MkRule

export
render : Rule -> String
render (MkRule s ds) = #"\#{render s}{\#{fastConcat $ map render ds}}"#
