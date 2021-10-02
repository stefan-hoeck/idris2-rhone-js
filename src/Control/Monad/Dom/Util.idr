module Control.Monad.Dom.Util

import Control.Monad.Dom.Interface
import Control.Monad.Dom.Ref
import Text.Html

mutual
  export
  listenToNodes : MonadDom m => List Node -> m ()
  listenToNodes []        = pure ()
  listenToNodes (x :: xs) = listenToNode x >> listenToNodes xs

  export
  listenToNode : MonadDom m => Node -> m ()
  listenToNode (El t _ es as ns) = case getId as of
    Just id => listenTo (Ref t id es) >> listenToNodes ns
    Nothing => listenToNodes ns

  listenToNode (Raw _) = pure ()
  listenToNode (Text _) = pure ()

export
adjacentAt : MonadDom m => ElemRef t es -> (n : Node) -> Position -> m (NodeRes n)
adjacentAt ref n pos = do
  (res,n2) <- mkNode n
  insertAdjacent ref pos n2
  listenToNode n2
  pure res

export
innerHtmlAt : MonadDom m => ElemRef t es -> (n : Node) -> m (NodeRes n)
innerHtmlAt ref n = do
  (res,n2) <- mkNode n
  innerHtml ref n2
  listenToNode n2
  pure res
