{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterReachableChangedEvent@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterReachableChangedEvent
  ( MTRBridgedDeviceBasicInformationClusterReachableChangedEvent
  , IsMTRBridgedDeviceBasicInformationClusterReachableChangedEvent(..)
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
reachableNewValue :: IsMTRBridgedDeviceBasicInformationClusterReachableChangedEvent mtrBridgedDeviceBasicInformationClusterReachableChangedEvent => mtrBridgedDeviceBasicInformationClusterReachableChangedEvent -> IO (Id NSNumber)
reachableNewValue mtrBridgedDeviceBasicInformationClusterReachableChangedEvent =
  sendMessage mtrBridgedDeviceBasicInformationClusterReachableChangedEvent reachableNewValueSelector

-- | @- setReachableNewValue:@
setReachableNewValue :: (IsMTRBridgedDeviceBasicInformationClusterReachableChangedEvent mtrBridgedDeviceBasicInformationClusterReachableChangedEvent, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterReachableChangedEvent -> value -> IO ()
setReachableNewValue mtrBridgedDeviceBasicInformationClusterReachableChangedEvent value =
  sendMessage mtrBridgedDeviceBasicInformationClusterReachableChangedEvent setReachableNewValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reachableNewValue@
reachableNewValueSelector :: Selector '[] (Id NSNumber)
reachableNewValueSelector = mkSelector "reachableNewValue"

-- | @Selector@ for @setReachableNewValue:@
setReachableNewValueSelector :: Selector '[Id NSNumber] ()
setReachableNewValueSelector = mkSelector "setReachableNewValue:"

