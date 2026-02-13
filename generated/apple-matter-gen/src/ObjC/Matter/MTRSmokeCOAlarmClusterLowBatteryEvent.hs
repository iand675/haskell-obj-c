{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterLowBatteryEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterLowBatteryEvent
  ( MTRSmokeCOAlarmClusterLowBatteryEvent
  , IsMTRSmokeCOAlarmClusterLowBatteryEvent(..)
  , alarmSeverityLevel
  , setAlarmSeverityLevel
  , alarmSeverityLevelSelector
  , setAlarmSeverityLevelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- alarmSeverityLevel@
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterLowBatteryEvent mtrSmokeCOAlarmClusterLowBatteryEvent => mtrSmokeCOAlarmClusterLowBatteryEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterLowBatteryEvent =
  sendMessage mtrSmokeCOAlarmClusterLowBatteryEvent alarmSeverityLevelSelector

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterLowBatteryEvent mtrSmokeCOAlarmClusterLowBatteryEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterLowBatteryEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterLowBatteryEvent value =
  sendMessage mtrSmokeCOAlarmClusterLowBatteryEvent setAlarmSeverityLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector '[] (Id NSNumber)
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector '[Id NSNumber] ()
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

