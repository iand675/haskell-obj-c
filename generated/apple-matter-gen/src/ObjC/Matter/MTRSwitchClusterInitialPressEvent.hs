{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterInitialPressEvent@.
module ObjC.Matter.MTRSwitchClusterInitialPressEvent
  ( MTRSwitchClusterInitialPressEvent
  , IsMTRSwitchClusterInitialPressEvent(..)
  , newPosition
  , setNewPosition
  , newPositionSelector
  , setNewPositionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- newPosition@
newPosition :: IsMTRSwitchClusterInitialPressEvent mtrSwitchClusterInitialPressEvent => mtrSwitchClusterInitialPressEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterInitialPressEvent =
  sendOwnedMessage mtrSwitchClusterInitialPressEvent newPositionSelector

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterInitialPressEvent mtrSwitchClusterInitialPressEvent, IsNSNumber value) => mtrSwitchClusterInitialPressEvent -> value -> IO ()
setNewPosition mtrSwitchClusterInitialPressEvent value =
  sendMessage mtrSwitchClusterInitialPressEvent setNewPositionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector '[] (Id NSNumber)
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector '[Id NSNumber] ()
setNewPositionSelector = mkSelector "setNewPosition:"

