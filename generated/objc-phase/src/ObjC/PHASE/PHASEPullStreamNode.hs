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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEPullStreamNode phasePullStreamNode => phasePullStreamNode -> IO (Id PHASEPullStreamNode)
init_ phasePullStreamNode  =
  sendMsg phasePullStreamNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEPullStreamNode)
new  =
  do
    cls' <- getRequiredClass "PHASEPullStreamNode"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | renderBlock
--
-- A property to set the render block callback that will render the samplesIW
--
-- The renderBlock must be set before the PHASESoundEvent is prepared or started.  The callback will be called from a high priority realtime thread.        Your implementation must be performant and not perform any realtime unsafe operations such as lock mutexes or allocate memory.
--
-- ObjC selector: @- renderBlock@
renderBlock :: IsPHASEPullStreamNode phasePullStreamNode => phasePullStreamNode -> IO (Ptr ())
renderBlock phasePullStreamNode  =
  fmap castPtr $ sendMsg phasePullStreamNode (mkSelector "renderBlock") (retPtr retVoid) []

-- | renderBlock
--
-- A property to set the render block callback that will render the samplesIW
--
-- The renderBlock must be set before the PHASESoundEvent is prepared or started.  The callback will be called from a high priority realtime thread.        Your implementation must be performant and not perform any realtime unsafe operations such as lock mutexes or allocate memory.
--
-- ObjC selector: @- setRenderBlock:@
setRenderBlock :: IsPHASEPullStreamNode phasePullStreamNode => phasePullStreamNode -> Ptr () -> IO ()
setRenderBlock phasePullStreamNode  value =
  sendMsg phasePullStreamNode (mkSelector "setRenderBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @renderBlock@
renderBlockSelector :: Selector
renderBlockSelector = mkSelector "renderBlock"

-- | @Selector@ for @setRenderBlock:@
setRenderBlockSelector :: Selector
setRenderBlockSelector = mkSelector "setRenderBlock:"

