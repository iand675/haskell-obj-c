{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterShortReleaseEvent@.
module ObjC.Matter.MTRSwitchClusterShortReleaseEvent
  ( MTRSwitchClusterShortReleaseEvent
  , IsMTRSwitchClusterShortReleaseEvent(..)
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
previousPosition :: IsMTRSwitchClusterShortReleaseEvent mtrSwitchClusterShortReleaseEvent => mtrSwitchClusterShortReleaseEvent -> IO (Id NSNumber)
previousPosition mtrSwitchClusterShortReleaseEvent =
  sendMessage mtrSwitchClusterShortReleaseEvent previousPositionSelector

-- | @- setPreviousPosition:@
setPreviousPosition :: (IsMTRSwitchClusterShortReleaseEvent mtrSwitchClusterShortReleaseEvent, IsNSNumber value) => mtrSwitchClusterShortReleaseEvent -> value -> IO ()
setPreviousPosition mtrSwitchClusterShortReleaseEvent value =
  sendMessage mtrSwitchClusterShortReleaseEvent setPreviousPositionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousPosition@
previousPositionSelector :: Selector '[] (Id NSNumber)
previousPositionSelector = mkSelector "previousPosition"

-- | @Selector@ for @setPreviousPosition:@
setPreviousPositionSelector :: Selector '[Id NSNumber] ()
setPreviousPositionSelector = mkSelector "setPreviousPosition:"

