{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSwitchClusterMultiPressCompleteEvent@.
module ObjC.Matter.MTRSwitchClusterMultiPressCompleteEvent
  ( MTRSwitchClusterMultiPressCompleteEvent
  , IsMTRSwitchClusterMultiPressCompleteEvent(..)
  , previousPosition
  , setPreviousPosition
  , newPosition
  , setNewPosition
  , totalNumberOfPressesCounted
  , setTotalNumberOfPressesCounted
  , newPositionSelector
  , previousPositionSelector
  , setNewPositionSelector
  , setPreviousPositionSelector
  , setTotalNumberOfPressesCountedSelector
  , totalNumberOfPressesCountedSelector


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
previousPosition :: IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent => mtrSwitchClusterMultiPressCompleteEvent -> IO (Id NSNumber)
previousPosition mtrSwitchClusterMultiPressCompleteEvent =
  sendMessage mtrSwitchClusterMultiPressCompleteEvent previousPositionSelector

-- | @- setPreviousPosition:@
setPreviousPosition :: (IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent, IsNSNumber value) => mtrSwitchClusterMultiPressCompleteEvent -> value -> IO ()
setPreviousPosition mtrSwitchClusterMultiPressCompleteEvent value =
  sendMessage mtrSwitchClusterMultiPressCompleteEvent setPreviousPositionSelector (toNSNumber value)

-- | @- newPosition@
newPosition :: IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent => mtrSwitchClusterMultiPressCompleteEvent -> IO (Id NSNumber)
newPosition mtrSwitchClusterMultiPressCompleteEvent =
  sendOwnedMessage mtrSwitchClusterMultiPressCompleteEvent newPositionSelector

-- | @- setNewPosition:@
setNewPosition :: (IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent, IsNSNumber value) => mtrSwitchClusterMultiPressCompleteEvent -> value -> IO ()
setNewPosition mtrSwitchClusterMultiPressCompleteEvent value =
  sendMessage mtrSwitchClusterMultiPressCompleteEvent setNewPositionSelector (toNSNumber value)

-- | @- totalNumberOfPressesCounted@
totalNumberOfPressesCounted :: IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent => mtrSwitchClusterMultiPressCompleteEvent -> IO (Id NSNumber)
totalNumberOfPressesCounted mtrSwitchClusterMultiPressCompleteEvent =
  sendMessage mtrSwitchClusterMultiPressCompleteEvent totalNumberOfPressesCountedSelector

-- | @- setTotalNumberOfPressesCounted:@
setTotalNumberOfPressesCounted :: (IsMTRSwitchClusterMultiPressCompleteEvent mtrSwitchClusterMultiPressCompleteEvent, IsNSNumber value) => mtrSwitchClusterMultiPressCompleteEvent -> value -> IO ()
setTotalNumberOfPressesCounted mtrSwitchClusterMultiPressCompleteEvent value =
  sendMessage mtrSwitchClusterMultiPressCompleteEvent setTotalNumberOfPressesCountedSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousPosition@
previousPositionSelector :: Selector '[] (Id NSNumber)
previousPositionSelector = mkSelector "previousPosition"

-- | @Selector@ for @setPreviousPosition:@
setPreviousPositionSelector :: Selector '[Id NSNumber] ()
setPreviousPositionSelector = mkSelector "setPreviousPosition:"

-- | @Selector@ for @newPosition@
newPositionSelector :: Selector '[] (Id NSNumber)
newPositionSelector = mkSelector "newPosition"

-- | @Selector@ for @setNewPosition:@
setNewPositionSelector :: Selector '[Id NSNumber] ()
setNewPositionSelector = mkSelector "setNewPosition:"

-- | @Selector@ for @totalNumberOfPressesCounted@
totalNumberOfPressesCountedSelector :: Selector '[] (Id NSNumber)
totalNumberOfPressesCountedSelector = mkSelector "totalNumberOfPressesCounted"

-- | @Selector@ for @setTotalNumberOfPressesCounted:@
setTotalNumberOfPressesCountedSelector :: Selector '[Id NSNumber] ()
setTotalNumberOfPressesCountedSelector = mkSelector "setTotalNumberOfPressesCounted:"

