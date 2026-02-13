{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTargetNavigatorClusterTargetUpdatedEvent@.
module ObjC.Matter.MTRTargetNavigatorClusterTargetUpdatedEvent
  ( MTRTargetNavigatorClusterTargetUpdatedEvent
  , IsMTRTargetNavigatorClusterTargetUpdatedEvent(..)
  , targetList
  , setTargetList
  , currentTarget
  , setCurrentTarget
  , data_
  , setData
  , currentTargetSelector
  , dataSelector
  , setCurrentTargetSelector
  , setDataSelector
  , setTargetListSelector
  , targetListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- targetList@
targetList :: IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent => mtrTargetNavigatorClusterTargetUpdatedEvent -> IO (Id NSArray)
targetList mtrTargetNavigatorClusterTargetUpdatedEvent =
  sendMessage mtrTargetNavigatorClusterTargetUpdatedEvent targetListSelector

-- | @- setTargetList:@
setTargetList :: (IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent, IsNSArray value) => mtrTargetNavigatorClusterTargetUpdatedEvent -> value -> IO ()
setTargetList mtrTargetNavigatorClusterTargetUpdatedEvent value =
  sendMessage mtrTargetNavigatorClusterTargetUpdatedEvent setTargetListSelector (toNSArray value)

-- | @- currentTarget@
currentTarget :: IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent => mtrTargetNavigatorClusterTargetUpdatedEvent -> IO (Id NSNumber)
currentTarget mtrTargetNavigatorClusterTargetUpdatedEvent =
  sendMessage mtrTargetNavigatorClusterTargetUpdatedEvent currentTargetSelector

-- | @- setCurrentTarget:@
setCurrentTarget :: (IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent, IsNSNumber value) => mtrTargetNavigatorClusterTargetUpdatedEvent -> value -> IO ()
setCurrentTarget mtrTargetNavigatorClusterTargetUpdatedEvent value =
  sendMessage mtrTargetNavigatorClusterTargetUpdatedEvent setCurrentTargetSelector (toNSNumber value)

-- | @- data@
data_ :: IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent => mtrTargetNavigatorClusterTargetUpdatedEvent -> IO (Id NSData)
data_ mtrTargetNavigatorClusterTargetUpdatedEvent =
  sendMessage mtrTargetNavigatorClusterTargetUpdatedEvent dataSelector

-- | @- setData:@
setData :: (IsMTRTargetNavigatorClusterTargetUpdatedEvent mtrTargetNavigatorClusterTargetUpdatedEvent, IsNSData value) => mtrTargetNavigatorClusterTargetUpdatedEvent -> value -> IO ()
setData mtrTargetNavigatorClusterTargetUpdatedEvent value =
  sendMessage mtrTargetNavigatorClusterTargetUpdatedEvent setDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @targetList@
targetListSelector :: Selector '[] (Id NSArray)
targetListSelector = mkSelector "targetList"

-- | @Selector@ for @setTargetList:@
setTargetListSelector :: Selector '[Id NSArray] ()
setTargetListSelector = mkSelector "setTargetList:"

-- | @Selector@ for @currentTarget@
currentTargetSelector :: Selector '[] (Id NSNumber)
currentTargetSelector = mkSelector "currentTarget"

-- | @Selector@ for @setCurrentTarget:@
setCurrentTargetSelector :: Selector '[Id NSNumber] ()
setCurrentTargetSelector = mkSelector "setCurrentTarget:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSData] ()
setDataSelector = mkSelector "setData:"

