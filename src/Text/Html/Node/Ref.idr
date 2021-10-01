module Text.Html.Node.Ref

import Control.Monad.Dom
import Data.SOP
import Text.Html.Attribute
import Text.Html.Node.Type
import Web.Dom

%hide Web.Internal.DomTypes.Node
%default total

public export
record Ref (el : Type) where
  constructor MkRef
  tag    : ElementType tpe el
  nodeId : String

mutual
  public export
  0 NodesTypes : List Node -> List Type
  NodesTypes []        = []
  NodesTypes (x :: xs) = NodeTypes x ++ NodesTypes xs

  public export
  0 NodeTypes : Node -> List Type
  NodeTypes (El _ False _ ys)      = NodesTypes ys
  NodeTypes (El {el} _ True _ ys)  = el :: NodesTypes ys
  NodeTypes (Raw _)                = []
  NodeTypes (Text _)               = []

public export
0 NodesRes : List Node -> Type
NodesRes = NP Ref . NodesTypes

public export
0 NodeRes : Node -> Type
NodeRes = NP Ref . NodeTypes

export
mkRef : MonadDom m => (et : ElementType t el) -> m (Ref el)
mkRef t = MkRef t . ("uid" ++) . show <$> unique


mutual
  export
  mkNodes : MonadDom m => (ns : List Node) -> m (NodesRes ns, List Node)
  mkNodes []        = pure ([], [])
  mkNodes (x :: xs) = do
    (rh, h) <- mkNode  x
    (rt, t) <- mkNodes xs
    pure (rh `append` rt, h :: t)

  export
  mkNode : MonadDom m => (n : Node) -> m (NodeRes n, Node)
  mkNode (El tpe True as ys) = do
    (ref,as2) <- case getId as of
      Just id => pure (MkRef tpe id, as)
      Nothing => map (\ref => (ref, (id .= nodeId ref) :: as)) (mkRef tpe)
    (rys, ys2) <- mkNodes ys
    pure (ref :: rys, El tpe True as2 ys2)
    
  mkNode n@(El tpe False as ys) = (\(r,ys2) => (r, El tpe False as ys2)) <$> mkNodes ys
  mkNode n@(Raw _)              = pure ([], n)
  mkNode n@(Text _)             = pure ([], n)

export
adjacentAt : MonadDom m => ElemRef t -> (n : Node) -> Position -> m (NodeRes n)
adjacentAt ref n pos = do
  (res,n2) <- mkNode n
  insertAdjacent ref pos n2 $> res

export
innerHtmlAt : MonadDom m => ElemRef t -> (n : Node) -> m (NodeRes n)
innerHtmlAt ref n = do
  (res,n2) <- mkNode n
  innerHtml ref n2 $> res
