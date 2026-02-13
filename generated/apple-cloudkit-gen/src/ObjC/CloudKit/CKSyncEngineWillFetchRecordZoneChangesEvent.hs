{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine is about to fetch record zone changes from the server for a specific zone.
--
-- This might be a good signal to prepare your local data store for incoming changes if necessary.
--
-- Generated bindings for @CKSyncEngineWillFetchRecordZoneChangesEvent@.
module ObjC.CloudKit.CKSyncEngineWillFetchRecordZoneChangesEvent
  ( CKSyncEngineWillFetchRecordZoneChangesEvent
  , IsCKSyncEngineWillFetchRecordZoneChangesEvent(..)
  , zoneID
  , zoneIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- zoneID@
zoneID :: IsCKSyncEngineWillFetchRecordZoneChangesEvent ckSyncEngineWillFetchRecordZoneChangesEvent => ckSyncEngineWillFetchRecordZoneChangesEvent -> IO (Id CKRecordZoneID)
zoneID ckSyncEngineWillFetchRecordZoneChangesEvent =
  sendMessage ckSyncEngineWillFetchRecordZoneChangesEvent zoneIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

