module TheGame.Handlers (handleAct) where

import Data.Functor.Const (Const)
import TheGame.Types (GameAction)

handleAct :: GameAction (Const ()) 
handleAct = _
