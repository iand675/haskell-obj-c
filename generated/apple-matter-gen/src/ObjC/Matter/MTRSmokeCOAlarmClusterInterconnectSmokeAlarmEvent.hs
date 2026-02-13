{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent
  ( MTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent
  , IsMTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent(..)
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
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent => mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent =
  sendMessage mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent alarmSeverityLevelSelector

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterInterconnectSmokeAlarmEvent mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent value =
  sendMessage mtrSmokeCOAlarmClusterInterconnectSmokeAlarmEvent setAlarmSeverityLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector '[] (Id NSNumber)
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector '[Id NSNumber] ()
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

