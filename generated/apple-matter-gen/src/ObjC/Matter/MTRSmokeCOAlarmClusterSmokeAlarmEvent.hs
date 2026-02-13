{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterSmokeAlarmEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterSmokeAlarmEvent
  ( MTRSmokeCOAlarmClusterSmokeAlarmEvent
  , IsMTRSmokeCOAlarmClusterSmokeAlarmEvent(..)
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
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterSmokeAlarmEvent mtrSmokeCOAlarmClusterSmokeAlarmEvent => mtrSmokeCOAlarmClusterSmokeAlarmEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterSmokeAlarmEvent =
  sendMessage mtrSmokeCOAlarmClusterSmokeAlarmEvent alarmSeverityLevelSelector

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterSmokeAlarmEvent mtrSmokeCOAlarmClusterSmokeAlarmEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterSmokeAlarmEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterSmokeAlarmEvent value =
  sendMessage mtrSmokeCOAlarmClusterSmokeAlarmEvent setAlarmSeverityLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector '[] (Id NSNumber)
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector '[Id NSNumber] ()
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

