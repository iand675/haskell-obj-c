{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterZoneStoppedEvent@.
module ObjC.Matter.MTRZoneManagementClusterZoneStoppedEvent
  ( MTRZoneManagementClusterZoneStoppedEvent
  , IsMTRZoneManagementClusterZoneStoppedEvent(..)
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
zone :: IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent => mtrZoneManagementClusterZoneStoppedEvent -> IO (Id NSNumber)
zone mtrZoneManagementClusterZoneStoppedEvent =
  sendMessage mtrZoneManagementClusterZoneStoppedEvent zoneSelector

-- | @- setZone:@
setZone :: (IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent, IsNSNumber value) => mtrZoneManagementClusterZoneStoppedEvent -> value -> IO ()
setZone mtrZoneManagementClusterZoneStoppedEvent value =
  sendMessage mtrZoneManagementClusterZoneStoppedEvent setZoneSelector (toNSNumber value)

-- | @- reason@
reason :: IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent => mtrZoneManagementClusterZoneStoppedEvent -> IO (Id NSNumber)
reason mtrZoneManagementClusterZoneStoppedEvent =
  sendMessage mtrZoneManagementClusterZoneStoppedEvent reasonSelector

-- | @- setReason:@
setReason :: (IsMTRZoneManagementClusterZoneStoppedEvent mtrZoneManagementClusterZoneStoppedEvent, IsNSNumber value) => mtrZoneManagementClusterZoneStoppedEvent -> value -> IO ()
setReason mtrZoneManagementClusterZoneStoppedEvent value =
  sendMessage mtrZoneManagementClusterZoneStoppedEvent setReasonSelector (toNSNumber value)

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

