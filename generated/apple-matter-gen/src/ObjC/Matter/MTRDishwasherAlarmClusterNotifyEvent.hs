{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherAlarmClusterNotifyEvent@.
module ObjC.Matter.MTRDishwasherAlarmClusterNotifyEvent
  ( MTRDishwasherAlarmClusterNotifyEvent
  , IsMTRDishwasherAlarmClusterNotifyEvent(..)
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
active :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
active mtrDishwasherAlarmClusterNotifyEvent =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent activeSelector

-- | @- setActive:@
setActive :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setActive mtrDishwasherAlarmClusterNotifyEvent value =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent setActiveSelector (toNSNumber value)

-- | @- inactive@
inactive :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
inactive mtrDishwasherAlarmClusterNotifyEvent =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent inactiveSelector

-- | @- setInactive:@
setInactive :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setInactive mtrDishwasherAlarmClusterNotifyEvent value =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent setInactiveSelector (toNSNumber value)

-- | @- state@
state :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
state mtrDishwasherAlarmClusterNotifyEvent =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent stateSelector

-- | @- setState:@
setState :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setState mtrDishwasherAlarmClusterNotifyEvent value =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent setStateSelector (toNSNumber value)

-- | @- mask@
mask :: IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent => mtrDishwasherAlarmClusterNotifyEvent -> IO (Id NSNumber)
mask mtrDishwasherAlarmClusterNotifyEvent =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent maskSelector

-- | @- setMask:@
setMask :: (IsMTRDishwasherAlarmClusterNotifyEvent mtrDishwasherAlarmClusterNotifyEvent, IsNSNumber value) => mtrDishwasherAlarmClusterNotifyEvent -> value -> IO ()
setMask mtrDishwasherAlarmClusterNotifyEvent value =
  sendMessage mtrDishwasherAlarmClusterNotifyEvent setMaskSelector (toNSNumber value)

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

