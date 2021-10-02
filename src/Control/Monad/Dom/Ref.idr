module Control.Monad.Dom.Ref

import Control.Monad.Dom.Interface
import Data.SOP
import Text.Html.Attribute
import Text.Html.Event
import Text.Html.Node
import Web.Dom

%hide Web.Internal.DomTypes.Node
%default total

mutual
  public export
  0 NodesTypes : List Node -> List Type
  NodesTypes []        = []
  NodesTypes (x :: xs) = NodeTypes x ++ NodesTypes xs

  public export
  0 NodeTypes : Node -> List Type
  NodeTypes (El _ False _ _ ys)      = NodesTypes ys
  NodeTypes (El {el} _ True es _ ys) = ElemRef el es :: NodesTypes ys
  NodeTypes (Raw _)                  = []
  NodeTypes (Text _)                 = []

public export
0 NodesRes : List Node -> Type
NodesRes = NP I . NodesTypes

public export
0 NodeRes : Node -> Type
NodeRes = NP I . NodeTypes

mkRef :  MonadDom m
      => (et : ElementType t el)
      -> (es : List EventType)
      -> m (ElemRef el es, String)
mkRef t es = do
  n <- unique
  let id = "uid" ++ show n
  pure (Ref t id es, id)

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
  mkNode (El tpe True es as ys) = do
    (ref,as2) <- case getId as of
      Just id => pure (Ref tpe id es, as)
      Nothing => map (\(r,i) => (r, (id .= i) :: as)) (mkRef tpe es)
    (rys, ys2) <- mkNodes ys
    pure (ref :: rys, El tpe True es as2 ys2)
    
  mkNode (El tpe False es as ys) =
    (\(r,ys2) => (r, El tpe False es as ys2)) <$> mkNodes ys
  mkNode n@(Raw _)               = pure ([], n)
  mkNode n@(Text _)              = pure ([], n)
