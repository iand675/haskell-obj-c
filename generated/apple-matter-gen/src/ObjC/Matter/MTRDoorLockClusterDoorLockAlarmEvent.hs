{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterDoorLockAlarmEvent@.
module ObjC.Matter.MTRDoorLockClusterDoorLockAlarmEvent
  ( MTRDoorLockClusterDoorLockAlarmEvent
  , IsMTRDoorLockClusterDoorLockAlarmEvent(..)
  , alarmCode
  , setAlarmCode
  , alarmCodeSelector
  , setAlarmCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- alarmCode@
alarmCode :: IsMTRDoorLockClusterDoorLockAlarmEvent mtrDoorLockClusterDoorLockAlarmEvent => mtrDoorLockClusterDoorLockAlarmEvent -> IO (Id NSNumber)
alarmCode mtrDoorLockClusterDoorLockAlarmEvent =
  sendMessage mtrDoorLockClusterDoorLockAlarmEvent alarmCodeSelector

-- | @- setAlarmCode:@
setAlarmCode :: (IsMTRDoorLockClusterDoorLockAlarmEvent mtrDoorLockClusterDoorLockAlarmEvent, IsNSNumber value) => mtrDoorLockClusterDoorLockAlarmEvent -> value -> IO ()
setAlarmCode mtrDoorLockClusterDoorLockAlarmEvent value =
  sendMessage mtrDoorLockClusterDoorLockAlarmEvent setAlarmCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alarmCode@
alarmCodeSelector :: Selector '[] (Id NSNumber)
alarmCodeSelector = mkSelector "alarmCode"

-- | @Selector@ for @setAlarmCode:@
setAlarmCodeSelector :: Selector '[Id NSNumber] ()
setAlarmCodeSelector = mkSelector "setAlarmCode:"

