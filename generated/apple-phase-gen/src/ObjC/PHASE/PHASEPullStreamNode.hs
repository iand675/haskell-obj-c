{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEPullStreamNode
--
-- An object for addessing an instance of a stream in an executing sound event
--
-- Generated bindings for @PHASEPullStreamNode@.
module ObjC.PHASE.PHASEPullStreamNode
  ( PHASEPullStreamNode
  , IsPHASEPullStreamNode(..)
  , init_
  , new
  , renderBlock
  , setRenderBlock
  , initSelector
  , newSelector
  , renderBlockSelector
  , setRenderBlockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEPullStreamNode phasePullStreamNode => phasePullStreamNode -> IO (Id PHASEPullStreamNode)
init_ phasePullStreamNode =
  sendOwnedMessage phasePullStreamNode initSelector

-- | @+ new@
new :: IO (Id PHASEPullStreamNode)
new  =
  do
    cls' <- getRequiredClass "PHASEPullStreamNode"
    sendOwnedClassMessage cls' newSelector

-- | renderBlock
--
-- A property to set the render block callback that will render the samplesIW
--
-- The renderBlock must be set before the PHASESoundEvent is prepared or started.  The callback will be called from a high priority realtime thread.        Your implementation must be performant and not perform any realtime unsafe operations such as lock mutexes or allocate memory.
--
-- ObjC selector: @- renderBlock@
renderBlock :: IsPHASEPullStreamNode phasePullStreamNode => phasePullStreamNode -> IO (Ptr ())
renderBlock phasePullStreamNode =
  sendMessage phasePullStreamNode renderBlockSelector

-- | renderBlock
--
-- A property to set the render block callback that will render the samplesIW
--
-- The renderBlock must be set before the PHASESoundEvent is prepared or started.  The callback will be called from a high priority realtime thread.        Your implementation must be performant and not perform any realtime unsafe operations such as lock mutexes or allocate memory.
--
-- ObjC selector: @- setRenderBlock:@
setRenderBlock :: IsPHASEPullStreamNode phasePullStreamNode => phasePullStreamNode -> Ptr () -> IO ()
setRenderBlock phasePullStreamNode value =
  sendMessage phasePullStreamNode setRenderBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEPullStreamNode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEPullStreamNode)
newSelector = mkSelector "new"

-- | @Selector@ for @renderBlock@
renderBlockSelector :: Selector '[] (Ptr ())
renderBlockSelector = mkSelector "renderBlock"

-- | @Selector@ for @setRenderBlock:@
setRenderBlockSelector :: Selector '[Ptr ()] ()
setRenderBlockSelector = mkSelector "setRenderBlock:"

