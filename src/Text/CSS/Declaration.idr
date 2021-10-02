module Text.CSS.Declaration

import Text.CSS.Color
import Text.CSS.ListStyleType
import Text.CSS.Property

%default total

public export
record Declaration where
  constructor MkDeclaration
  property : Property tpe
  value    : tpe

infixl 8 .=

export %inline
(.=) : Property t -> t -> Declaration
(.=) = MkDeclaration

export %inline
render : Declaration -> String
render (MkDeclaration p v) = renderProp p v ++ ";"
