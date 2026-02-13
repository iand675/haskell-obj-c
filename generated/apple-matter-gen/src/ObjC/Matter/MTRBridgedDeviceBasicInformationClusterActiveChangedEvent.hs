{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterActiveChangedEvent@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterActiveChangedEvent
  ( MTRBridgedDeviceBasicInformationClusterActiveChangedEvent
  , IsMTRBridgedDeviceBasicInformationClusterActiveChangedEvent(..)
  , promisedActiveDuration
  , setPromisedActiveDuration
  , promisedActiveDurationSelector
  , setPromisedActiveDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- promisedActiveDuration@
promisedActiveDuration :: IsMTRBridgedDeviceBasicInformationClusterActiveChangedEvent mtrBridgedDeviceBasicInformationClusterActiveChangedEvent => mtrBridgedDeviceBasicInformationClusterActiveChangedEvent -> IO (Id NSNumber)
promisedActiveDuration mtrBridgedDeviceBasicInformationClusterActiveChangedEvent =
  sendMessage mtrBridgedDeviceBasicInformationClusterActiveChangedEvent promisedActiveDurationSelector

-- | @- setPromisedActiveDuration:@
setPromisedActiveDuration :: (IsMTRBridgedDeviceBasicInformationClusterActiveChangedEvent mtrBridgedDeviceBasicInformationClusterActiveChangedEvent, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterActiveChangedEvent -> value -> IO ()
setPromisedActiveDuration mtrBridgedDeviceBasicInformationClusterActiveChangedEvent value =
  sendMessage mtrBridgedDeviceBasicInformationClusterActiveChangedEvent setPromisedActiveDurationSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @promisedActiveDuration@
promisedActiveDurationSelector :: Selector '[] (Id NSNumber)
promisedActiveDurationSelector = mkSelector "promisedActiveDuration"

-- | @Selector@ for @setPromisedActiveDuration:@
setPromisedActiveDurationSelector :: Selector '[Id NSNumber] ()
setPromisedActiveDurationSelector = mkSelector "setPromisedActiveDuration:"

