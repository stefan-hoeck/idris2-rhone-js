module Text.Html.Node

import Control.Monad.Dom
import Data.SOP
import Text.Html.Attribute
import Web.Dom

public export
data Ref : Type -> Type where
  MkRef : String -> Ref a

public export
data Node : Type where
  El   :  {tag : String}
       -> (0 tpe : ElementType tag el)
       -> List Attribute
       -> List Node.Node
       -> Node

  UEl  :  {tag : String}
       -> (0 tpe : ElementType tag el)
       -> List Attribute
       -> List Node.Node
       -> Node

  Raw  : String -> Node
  Text : String -> Node

mutual
  public export
  0 NodesTypes : List Node.Node -> List Type
  NodesTypes []        = []
  NodesTypes (x :: xs) = NodeTypes x ++ NodesTypes xs

  public export
  0 NodeTypes : Node.Node -> List Type
  NodeTypes (El _ _ ys)       = NodesTypes ys
  NodeTypes (UEl {el} _ _ ys) = el :: NodesTypes ys
  NodeTypes (Raw _)           = []
  NodeTypes (Text _)          = []

public export
0 NodesRes : List Node.Node -> Type
NodesRes = NP Ref . NodesTypes

public export
0 NodeRes : Node.Node -> Type
NodeRes = NP Ref . NodeTypes

export
mkRef : MonadDom m => (0 et : ElementType t el) -> m (Ref el)
mkRef _ = MkRef . ("uid" ++) . show <$> unique

mutual
  export
  mkListNodes : MonadDom m => (ns : List Node.Node) -> m (NodesRes ns)
  mkListNodes []        = pure []
  mkListNodes (x :: xs) = [| mkNodes x `append` mkListNodes xs |]

  export
  mkNodes : MonadDom m => (n : Node.Node) -> m (NodeRes n)
  mkNodes (El _ _ ys)    = mkListNodes ys
  mkNodes (UEl tpe _ ys) = [| mkRef tpe :: mkListNodes ys |]
  mkNodes (Raw _)        = pure []
  mkNodes (Text _)       = pure []
