{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNAnimationEvent encapsulates a block to trigger at a specific time.
--
-- Generated bindings for @SCNAnimationEvent@.
module ObjC.SceneKit.SCNAnimationEvent
  ( SCNAnimationEvent
  , IsSCNAnimationEvent(..)
  , animationEventWithKeyTime_block
  , animationEventWithKeyTime_blockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | animationEventWithKeyTime:block:
--
-- Returns an animation event instance
--
-- @time@ — The relative time to trigger the event.
--
-- @eventBlock@ — The block to call when the event is triggered.
--
-- "time" is relative to animation duration and therefor it has to be a value in the range [0,1].
--
-- ObjC selector: @+ animationEventWithKeyTime:block:@
animationEventWithKeyTime_block :: CDouble -> Ptr () -> IO (Id SCNAnimationEvent)
animationEventWithKeyTime_block time eventBlock =
  do
    cls' <- getRequiredClass "SCNAnimationEvent"
    sendClassMessage cls' animationEventWithKeyTime_blockSelector time eventBlock

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @animationEventWithKeyTime:block:@
animationEventWithKeyTime_blockSelector :: Selector '[CDouble, Ptr ()] (Id SCNAnimationEvent)
animationEventWithKeyTime_blockSelector = mkSelector "animationEventWithKeyTime:block:"

