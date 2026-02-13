{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterDSTStatusEvent@.
module ObjC.Matter.MTRTimeSynchronizationClusterDSTStatusEvent
  ( MTRTimeSynchronizationClusterDSTStatusEvent
  , IsMTRTimeSynchronizationClusterDSTStatusEvent(..)
  , dstOffsetActive
  , setDstOffsetActive
  , dstOffsetActiveSelector
  , setDstOffsetActiveSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- dstOffsetActive@
dstOffsetActive :: IsMTRTimeSynchronizationClusterDSTStatusEvent mtrTimeSynchronizationClusterDSTStatusEvent => mtrTimeSynchronizationClusterDSTStatusEvent -> IO (Id NSNumber)
dstOffsetActive mtrTimeSynchronizationClusterDSTStatusEvent =
  sendMessage mtrTimeSynchronizationClusterDSTStatusEvent dstOffsetActiveSelector

-- | @- setDstOffsetActive:@
setDstOffsetActive :: (IsMTRTimeSynchronizationClusterDSTStatusEvent mtrTimeSynchronizationClusterDSTStatusEvent, IsNSNumber value) => mtrTimeSynchronizationClusterDSTStatusEvent -> value -> IO ()
setDstOffsetActive mtrTimeSynchronizationClusterDSTStatusEvent value =
  sendMessage mtrTimeSynchronizationClusterDSTStatusEvent setDstOffsetActiveSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dstOffsetActive@
dstOffsetActiveSelector :: Selector '[] (Id NSNumber)
dstOffsetActiveSelector = mkSelector "dstOffsetActive"

-- | @Selector@ for @setDstOffsetActive:@
setDstOffsetActiveSelector :: Selector '[Id NSNumber] ()
setDstOffsetActiveSelector = mkSelector "setDstOffsetActive:"

