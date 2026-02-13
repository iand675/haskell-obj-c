{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterWiredFaultChangeEvent@.
module ObjC.Matter.MTRPowerSourceClusterWiredFaultChangeEvent
  ( MTRPowerSourceClusterWiredFaultChangeEvent
  , IsMTRPowerSourceClusterWiredFaultChangeEvent(..)
  , current
  , setCurrent
  , previous
  , setPrevious
  , currentSelector
  , previousSelector
  , setCurrentSelector
  , setPreviousSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- current@
current :: IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent => mtrPowerSourceClusterWiredFaultChangeEvent -> IO (Id NSArray)
current mtrPowerSourceClusterWiredFaultChangeEvent =
  sendMessage mtrPowerSourceClusterWiredFaultChangeEvent currentSelector

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeEvent -> value -> IO ()
setCurrent mtrPowerSourceClusterWiredFaultChangeEvent value =
  sendMessage mtrPowerSourceClusterWiredFaultChangeEvent setCurrentSelector (toNSArray value)

-- | @- previous@
previous :: IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent => mtrPowerSourceClusterWiredFaultChangeEvent -> IO (Id NSArray)
previous mtrPowerSourceClusterWiredFaultChangeEvent =
  sendMessage mtrPowerSourceClusterWiredFaultChangeEvent previousSelector

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterWiredFaultChangeEvent mtrPowerSourceClusterWiredFaultChangeEvent, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeEvent -> value -> IO ()
setPrevious mtrPowerSourceClusterWiredFaultChangeEvent value =
  sendMessage mtrPowerSourceClusterWiredFaultChangeEvent setPreviousSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @current@
currentSelector :: Selector '[] (Id NSArray)
currentSelector = mkSelector "current"

-- | @Selector@ for @setCurrent:@
setCurrentSelector :: Selector '[Id NSArray] ()
setCurrentSelector = mkSelector "setCurrent:"

-- | @Selector@ for @previous@
previousSelector :: Selector '[] (Id NSArray)
previousSelector = mkSelector "previous"

-- | @Selector@ for @setPrevious:@
setPreviousSelector :: Selector '[Id NSArray] ()
setPreviousSelector = mkSelector "setPrevious:"

