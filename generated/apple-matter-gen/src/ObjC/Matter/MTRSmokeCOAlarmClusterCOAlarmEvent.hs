{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterCOAlarmEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterCOAlarmEvent
  ( MTRSmokeCOAlarmClusterCOAlarmEvent
  , IsMTRSmokeCOAlarmClusterCOAlarmEvent(..)
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
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterCOAlarmEvent mtrSmokeCOAlarmClusterCOAlarmEvent => mtrSmokeCOAlarmClusterCOAlarmEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterCOAlarmEvent =
  sendMessage mtrSmokeCOAlarmClusterCOAlarmEvent alarmSeverityLevelSelector

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterCOAlarmEvent mtrSmokeCOAlarmClusterCOAlarmEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterCOAlarmEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterCOAlarmEvent value =
  sendMessage mtrSmokeCOAlarmClusterCOAlarmEvent setAlarmSeverityLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector '[] (Id NSNumber)
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector '[Id NSNumber] ()
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

