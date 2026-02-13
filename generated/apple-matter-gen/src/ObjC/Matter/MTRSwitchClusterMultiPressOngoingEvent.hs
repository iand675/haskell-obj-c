{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterMultiPressOngoingEvent@.
module ObjC.Matter.MTRSwitchClusterMultiPressOngoingEvent
  ( MTRSwitchClusterMultiPressOngoingEvent
  , IsMTRSwitchClusterMultiPressOngoingEvent(..)
  , newPosition
  , setNewPosition
  , currentNumberOfPressesCounted
  , setCurrentNumberOfPressesCounted
  , currentNumberOfPressesCountedSelector
  , newPositionSelector
  , setCurrentNumberOfPressesCountedSelector
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
newPosition :: IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent => mtrSwitchClusterMultiPressOngoingEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterMultiPressOngoingEvent =
  sendOwnedMessage mtrSwitchClusterMultiPressOngoingEvent newPositionSelector

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent, IsNSNumber value) => mtrSwitchClusterMultiPressOngoingEvent -> value -> IO ()
setNewPosition mtrSwitchClusterMultiPressOngoingEvent value =
  sendMessage mtrSwitchClusterMultiPressOngoingEvent setNewPositionSelector (toNSNumber value)

-- | @- currentNumberOfPressesCounted@
currentNumberOfPressesCounted :: IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent => mtrSwitchClusterMultiPressOngoingEvent -> IO (Id NSNumber)
currentNumberOfPressesCounted mtrSwitchClusterMultiPressOngoingEvent =
  sendMessage mtrSwitchClusterMultiPressOngoingEvent currentNumberOfPressesCountedSelector

-- | @- setCurrentNumberOfPressesCounted:@
setCurrentNumberOfPressesCounted :: (IsMTRSwitchClusterMultiPressOngoingEvent mtrSwitchClusterMultiPressOngoingEvent, IsNSNumber value) => mtrSwitchClusterMultiPressOngoingEvent -> value -> IO ()
setCurrentNumberOfPressesCounted mtrSwitchClusterMultiPressOngoingEvent value =
  sendMessage mtrSwitchClusterMultiPressOngoingEvent setCurrentNumberOfPressesCountedSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector '[] (Id NSNumber)
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector '[Id NSNumber] ()
setNewPositionSelector = mkSelector "setNewPosition:"

-- | @Selector@ for @currentNumberOfPressesCounted@
currentNumberOfPressesCountedSelector :: Selector '[] (Id NSNumber)
currentNumberOfPressesCountedSelector = mkSelector "currentNumberOfPressesCounted"

-- | @Selector@ for @setCurrentNumberOfPressesCounted:@
setCurrentNumberOfPressesCountedSelector :: Selector '[Id NSNumber] ()
setCurrentNumberOfPressesCountedSelector = mkSelector "setCurrentNumberOfPressesCounted:"

