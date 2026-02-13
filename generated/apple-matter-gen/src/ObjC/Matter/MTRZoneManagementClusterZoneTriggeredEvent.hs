{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneTriggeredEvent@.
module ObjC.Matter.MTRZoneManagementClusterZoneTriggeredEvent
  ( MTRZoneManagementClusterZoneTriggeredEvent
  , IsMTRZoneManagementClusterZoneTriggeredEvent(..)
  , zone
  , setZone
  , reason
  , setReason
  , reasonSelector
  , setReasonSelector
  , setZoneSelector
  , zoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- zone@
zone :: IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent => mtrZoneManagementClusterZoneTriggeredEvent -> IO (Id NSNumber)
zone mtrZoneManagementClusterZoneTriggeredEvent =
  sendMessage mtrZoneManagementClusterZoneTriggeredEvent zoneSelector

-- | @- setZone:@
setZone :: (IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent, IsNSNumber value) => mtrZoneManagementClusterZoneTriggeredEvent -> value -> IO ()
setZone mtrZoneManagementClusterZoneTriggeredEvent value =
  sendMessage mtrZoneManagementClusterZoneTriggeredEvent setZoneSelector (toNSNumber value)

-- | @- reason@
reason :: IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent => mtrZoneManagementClusterZoneTriggeredEvent -> IO (Id NSNumber)
reason mtrZoneManagementClusterZoneTriggeredEvent =
  sendMessage mtrZoneManagementClusterZoneTriggeredEvent reasonSelector

-- | @- setReason:@
setReason :: (IsMTRZoneManagementClusterZoneTriggeredEvent mtrZoneManagementClusterZoneTriggeredEvent, IsNSNumber value) => mtrZoneManagementClusterZoneTriggeredEvent -> value -> IO ()
setReason mtrZoneManagementClusterZoneTriggeredEvent value =
  sendMessage mtrZoneManagementClusterZoneTriggeredEvent setReasonSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zone@
zoneSelector :: Selector '[] (Id NSNumber)
zoneSelector = mkSelector "zone"

-- | @Selector@ for @setZone:@
setZoneSelector :: Selector '[Id NSNumber] ()
setZoneSelector = mkSelector "setZone:"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] (Id NSNumber)
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector '[Id NSNumber] ()
setReasonSelector = mkSelector "setReason:"

