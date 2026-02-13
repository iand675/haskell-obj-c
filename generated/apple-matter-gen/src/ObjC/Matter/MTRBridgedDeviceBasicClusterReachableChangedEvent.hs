{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicClusterReachableChangedEvent@.
module ObjC.Matter.MTRBridgedDeviceBasicClusterReachableChangedEvent
  ( MTRBridgedDeviceBasicClusterReachableChangedEvent
  , IsMTRBridgedDeviceBasicClusterReachableChangedEvent(..)
  , reachableNewValue
  , setReachableNewValue
  , reachableNewValueSelector
  , setReachableNewValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reachableNewValue@
reachableNewValue :: IsMTRBridgedDeviceBasicClusterReachableChangedEvent mtrBridgedDeviceBasicClusterReachableChangedEvent => mtrBridgedDeviceBasicClusterReachableChangedEvent -> IO (Id NSNumber)
reachableNewValue mtrBridgedDeviceBasicClusterReachableChangedEvent =
  sendMessage mtrBridgedDeviceBasicClusterReachableChangedEvent reachableNewValueSelector

-- | @- setReachableNewValue:@
setReachableNewValue :: (IsMTRBridgedDeviceBasicClusterReachableChangedEvent mtrBridgedDeviceBasicClusterReachableChangedEvent, IsNSNumber value) => mtrBridgedDeviceBasicClusterReachableChangedEvent -> value -> IO ()
setReachableNewValue mtrBridgedDeviceBasicClusterReachableChangedEvent value =
  sendMessage mtrBridgedDeviceBasicClusterReachableChangedEvent setReachableNewValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reachableNewValue@
reachableNewValueSelector :: Selector '[] (Id NSNumber)
reachableNewValueSelector = mkSelector "reachableNewValue"

-- | @Selector@ for @setReachableNewValue:@
setReachableNewValueSelector :: Selector '[Id NSNumber] ()
setReachableNewValueSelector = mkSelector "setReachableNewValue:"

