{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSmokeCOAlarmClusterInterconnectCOAlarmEvent@.
module ObjC.Matter.MTRSmokeCOAlarmClusterInterconnectCOAlarmEvent
  ( MTRSmokeCOAlarmClusterInterconnectCOAlarmEvent
  , IsMTRSmokeCOAlarmClusterInterconnectCOAlarmEvent(..)
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
alarmSeverityLevel :: IsMTRSmokeCOAlarmClusterInterconnectCOAlarmEvent mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent => mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent -> IO (Id NSNumber)
alarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent =
  sendMessage mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent alarmSeverityLevelSelector

-- | @- setAlarmSeverityLevel:@
setAlarmSeverityLevel :: (IsMTRSmokeCOAlarmClusterInterconnectCOAlarmEvent mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent, IsNSNumber value) => mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent -> value -> IO ()
setAlarmSeverityLevel mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent value =
  sendMessage mtrSmokeCOAlarmClusterInterconnectCOAlarmEvent setAlarmSeverityLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmSeverityLevel@
alarmSeverityLevelSelector :: Selector '[] (Id NSNumber)
alarmSeverityLevelSelector = mkSelector "alarmSeverityLevel"

-- | @Selector@ for @setAlarmSeverityLevel:@
setAlarmSeverityLevelSelector :: Selector '[Id NSNumber] ()
setAlarmSeverityLevelSelector = mkSelector "setAlarmSeverityLevel:"

