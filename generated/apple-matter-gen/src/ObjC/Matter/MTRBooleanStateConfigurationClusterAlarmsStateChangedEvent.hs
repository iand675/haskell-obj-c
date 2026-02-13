{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBooleanStateConfigurationClusterAlarmsStateChangedEvent@.
module ObjC.Matter.MTRBooleanStateConfigurationClusterAlarmsStateChangedEvent
  ( MTRBooleanStateConfigurationClusterAlarmsStateChangedEvent
  , IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent(..)
  , alarmsActive
  , setAlarmsActive
  , alarmsSuppressed
  , setAlarmsSuppressed
  , alarmsActiveSelector
  , alarmsSuppressedSelector
  , setAlarmsActiveSelector
  , setAlarmsSuppressedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- alarmsActive@
alarmsActive :: IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> IO (Id NSNumber)
alarmsActive mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent =
  sendMessage mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent alarmsActiveSelector

-- | @- setAlarmsActive:@
setAlarmsActive :: (IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent, IsNSNumber value) => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> value -> IO ()
setAlarmsActive mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent value =
  sendMessage mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent setAlarmsActiveSelector (toNSNumber value)

-- | @- alarmsSuppressed@
alarmsSuppressed :: IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> IO (Id NSNumber)
alarmsSuppressed mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent =
  sendMessage mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent alarmsSuppressedSelector

-- | @- setAlarmsSuppressed:@
setAlarmsSuppressed :: (IsMTRBooleanStateConfigurationClusterAlarmsStateChangedEvent mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent, IsNSNumber value) => mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent -> value -> IO ()
setAlarmsSuppressed mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent value =
  sendMessage mtrBooleanStateConfigurationClusterAlarmsStateChangedEvent setAlarmsSuppressedSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmsActive@
alarmsActiveSelector :: Selector '[] (Id NSNumber)
alarmsActiveSelector = mkSelector "alarmsActive"

-- | @Selector@ for @setAlarmsActive:@
setAlarmsActiveSelector :: Selector '[Id NSNumber] ()
setAlarmsActiveSelector = mkSelector "setAlarmsActive:"

-- | @Selector@ for @alarmsSuppressed@
alarmsSuppressedSelector :: Selector '[] (Id NSNumber)
alarmsSuppressedSelector = mkSelector "alarmsSuppressed"

-- | @Selector@ for @setAlarmsSuppressed:@
setAlarmsSuppressedSelector :: Selector '[Id NSNumber] ()
setAlarmsSuppressedSelector = mkSelector "setAlarmsSuppressed:"

