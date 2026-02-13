{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterPushTransportBeginEvent@.
module ObjC.Matter.MTRPushAVStreamTransportClusterPushTransportBeginEvent
  ( MTRPushAVStreamTransportClusterPushTransportBeginEvent
  , IsMTRPushAVStreamTransportClusterPushTransportBeginEvent(..)
  , connectionID
  , setConnectionID
  , triggerType
  , setTriggerType
  , activationReason
  , setActivationReason
  , activationReasonSelector
  , connectionIDSelector
  , setActivationReasonSelector
  , setConnectionIDSelector
  , setTriggerTypeSelector
  , triggerTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- connectionID@
connectionID :: IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterPushTransportBeginEvent =
  sendMessage mtrPushAVStreamTransportClusterPushTransportBeginEvent connectionIDSelector

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterPushTransportBeginEvent value =
  sendMessage mtrPushAVStreamTransportClusterPushTransportBeginEvent setConnectionIDSelector (toNSNumber value)

-- | @- triggerType@
triggerType :: IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> IO (Id NSNumber)
triggerType mtrPushAVStreamTransportClusterPushTransportBeginEvent =
  sendMessage mtrPushAVStreamTransportClusterPushTransportBeginEvent triggerTypeSelector

-- | @- setTriggerType:@
setTriggerType :: (IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> value -> IO ()
setTriggerType mtrPushAVStreamTransportClusterPushTransportBeginEvent value =
  sendMessage mtrPushAVStreamTransportClusterPushTransportBeginEvent setTriggerTypeSelector (toNSNumber value)

-- | @- activationReason@
activationReason :: IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> IO (Id NSNumber)
activationReason mtrPushAVStreamTransportClusterPushTransportBeginEvent =
  sendMessage mtrPushAVStreamTransportClusterPushTransportBeginEvent activationReasonSelector

-- | @- setActivationReason:@
setActivationReason :: (IsMTRPushAVStreamTransportClusterPushTransportBeginEvent mtrPushAVStreamTransportClusterPushTransportBeginEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportBeginEvent -> value -> IO ()
setActivationReason mtrPushAVStreamTransportClusterPushTransportBeginEvent value =
  sendMessage mtrPushAVStreamTransportClusterPushTransportBeginEvent setActivationReasonSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector '[] (Id NSNumber)
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector '[Id NSNumber] ()
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @triggerType@
triggerTypeSelector :: Selector '[] (Id NSNumber)
triggerTypeSelector = mkSelector "triggerType"

-- | @Selector@ for @setTriggerType:@
setTriggerTypeSelector :: Selector '[Id NSNumber] ()
setTriggerTypeSelector = mkSelector "setTriggerType:"

-- | @Selector@ for @activationReason@
activationReasonSelector :: Selector '[] (Id NSNumber)
activationReasonSelector = mkSelector "activationReason"

-- | @Selector@ for @setActivationReason:@
setActivationReasonSelector :: Selector '[Id NSNumber] ()
setActivationReasonSelector = mkSelector "setActivationReason:"

