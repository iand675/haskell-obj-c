{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPowerSourceClusterWiredFaultChangeType@.
module ObjC.Matter.MTRPowerSourceClusterWiredFaultChangeType
  ( MTRPowerSourceClusterWiredFaultChangeType
  , IsMTRPowerSourceClusterWiredFaultChangeType(..)
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
current :: IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType => mtrPowerSourceClusterWiredFaultChangeType -> IO (Id NSArray)
current mtrPowerSourceClusterWiredFaultChangeType =
  sendMessage mtrPowerSourceClusterWiredFaultChangeType currentSelector

-- | @- setCurrent:@
setCurrent :: (IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeType -> value -> IO ()
setCurrent mtrPowerSourceClusterWiredFaultChangeType value =
  sendMessage mtrPowerSourceClusterWiredFaultChangeType setCurrentSelector (toNSArray value)

-- | @- previous@
previous :: IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType => mtrPowerSourceClusterWiredFaultChangeType -> IO (Id NSArray)
previous mtrPowerSourceClusterWiredFaultChangeType =
  sendMessage mtrPowerSourceClusterWiredFaultChangeType previousSelector

-- | @- setPrevious:@
setPrevious :: (IsMTRPowerSourceClusterWiredFaultChangeType mtrPowerSourceClusterWiredFaultChangeType, IsNSArray value) => mtrPowerSourceClusterWiredFaultChangeType -> value -> IO ()
setPrevious mtrPowerSourceClusterWiredFaultChangeType value =
  sendMessage mtrPowerSourceClusterWiredFaultChangeType setPreviousSelector (toNSArray value)

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

