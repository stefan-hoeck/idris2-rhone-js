module Text.Html.Node

import Text.Html.Attribute

public export
data Node : Type where
  El   : (tag : String) -> List Attribute -> List Node -> Node
  Raw  : String -> Node
  Text : String -> Node
