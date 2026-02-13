{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRefrigeratorAlarmClusterNotifyEvent@.
module ObjC.Matter.MTRRefrigeratorAlarmClusterNotifyEvent
  ( MTRRefrigeratorAlarmClusterNotifyEvent
  , IsMTRRefrigeratorAlarmClusterNotifyEvent(..)
  , active
  , setActive
  , inactive
  , setInactive
  , state
  , setState
  , mask
  , setMask
  , activeSelector
  , inactiveSelector
  , maskSelector
  , setActiveSelector
  , setInactiveSelector
  , setMaskSelector
  , setStateSelector
  , stateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- active@
active :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
active mtrRefrigeratorAlarmClusterNotifyEvent =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent activeSelector

-- | @- setActive:@
setActive :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setActive mtrRefrigeratorAlarmClusterNotifyEvent value =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent setActiveSelector (toNSNumber value)

-- | @- inactive@
inactive :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
inactive mtrRefrigeratorAlarmClusterNotifyEvent =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent inactiveSelector

-- | @- setInactive:@
setInactive :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setInactive mtrRefrigeratorAlarmClusterNotifyEvent value =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent setInactiveSelector (toNSNumber value)

-- | @- state@
state :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
state mtrRefrigeratorAlarmClusterNotifyEvent =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent stateSelector

-- | @- setState:@
setState :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setState mtrRefrigeratorAlarmClusterNotifyEvent value =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent setStateSelector (toNSNumber value)

-- | @- mask@
mask :: IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent => mtrRefrigeratorAlarmClusterNotifyEvent -> IO (Id NSNumber)
mask mtrRefrigeratorAlarmClusterNotifyEvent =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent maskSelector

-- | @- setMask:@
setMask :: (IsMTRRefrigeratorAlarmClusterNotifyEvent mtrRefrigeratorAlarmClusterNotifyEvent, IsNSNumber value) => mtrRefrigeratorAlarmClusterNotifyEvent -> value -> IO ()
setMask mtrRefrigeratorAlarmClusterNotifyEvent value =
  sendMessage mtrRefrigeratorAlarmClusterNotifyEvent setMaskSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @active@
activeSelector :: Selector '[] (Id NSNumber)
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector '[Id NSNumber] ()
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @inactive@
inactiveSelector :: Selector '[] (Id NSNumber)
inactiveSelector = mkSelector "inactive"

-- | @Selector@ for @setInactive:@
setInactiveSelector :: Selector '[Id NSNumber] ()
setInactiveSelector = mkSelector "setInactive:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSNumber)
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[Id NSNumber] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @mask@
maskSelector :: Selector '[] (Id NSNumber)
maskSelector = mkSelector "mask"

-- | @Selector@ for @setMask:@
setMaskSelector :: Selector '[Id NSNumber] ()
setMaskSelector = mkSelector "setMask:"

