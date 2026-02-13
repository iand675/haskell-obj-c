{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A zone save that needs to be sent to the server.
--
-- Generated bindings for @CKSyncEnginePendingZoneSave@.
module ObjC.CloudKit.CKSyncEnginePendingZoneSave
  ( CKSyncEnginePendingZoneSave
  , IsCKSyncEnginePendingZoneSave(..)
  , initWithZone
  , zone
  , initWithZoneSelector
  , zoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithZone:@
initWithZone :: (IsCKSyncEnginePendingZoneSave ckSyncEnginePendingZoneSave, IsCKRecordZone zone) => ckSyncEnginePendingZoneSave -> zone -> IO (Id CKSyncEnginePendingZoneSave)
initWithZone ckSyncEnginePendingZoneSave zone =
  sendOwnedMessage ckSyncEnginePendingZoneSave initWithZoneSelector (toCKRecordZone zone)

-- | @- zone@
zone :: IsCKSyncEnginePendingZoneSave ckSyncEnginePendingZoneSave => ckSyncEnginePendingZoneSave -> IO (Id CKRecordZone)
zone ckSyncEnginePendingZoneSave =
  sendMessage ckSyncEnginePendingZoneSave zoneSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZone:@
initWithZoneSelector :: Selector '[Id CKRecordZone] (Id CKSyncEnginePendingZoneSave)
initWithZoneSelector = mkSelector "initWithZone:"

-- | @Selector@ for @zone@
zoneSelector :: Selector '[] (Id CKRecordZone)
zoneSelector = mkSelector "zone"

