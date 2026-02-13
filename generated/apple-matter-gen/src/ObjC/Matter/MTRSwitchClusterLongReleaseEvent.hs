{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterLongReleaseEvent@.
module ObjC.Matter.MTRSwitchClusterLongReleaseEvent
  ( MTRSwitchClusterLongReleaseEvent
  , IsMTRSwitchClusterLongReleaseEvent(..)
  , previousPosition
  , setPreviousPosition
  , previousPositionSelector
  , setPreviousPositionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousPosition@
previousPosition :: IsMTRSwitchClusterLongReleaseEvent mtrSwitchClusterLongReleaseEvent => mtrSwitchClusterLongReleaseEvent -> IO (Id NSNumber)
previousPosition mtrSwitchClusterLongReleaseEvent =
  sendMessage mtrSwitchClusterLongReleaseEvent previousPositionSelector

-- | @- setPreviousPosition:@
setPreviousPosition :: (IsMTRSwitchClusterLongReleaseEvent mtrSwitchClusterLongReleaseEvent, IsNSNumber value) => mtrSwitchClusterLongReleaseEvent -> value -> IO ()
setPreviousPosition mtrSwitchClusterLongReleaseEvent value =
  sendMessage mtrSwitchClusterLongReleaseEvent setPreviousPositionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousPosition@
previousPositionSelector :: Selector '[] (Id NSNumber)
previousPositionSelector = mkSelector "previousPosition"

-- | @Selector@ for @setPreviousPosition:@
setPreviousPositionSelector :: Selector '[Id NSNumber] ()
setPreviousPositionSelector = mkSelector "setPreviousPosition:"

