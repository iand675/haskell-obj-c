{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished fetching record zone changes from the server for a specific zone.
--
-- This might be a good signal to perform any post-processing tasks on a per-zone basis if necessary.
--
-- You should receive one @CKSyncEngineDidFetchRecordZoneChangesEvent@ for each @CKSyncEngineWillFetchRecordZoneChangesEvent@.
--
-- Generated bindings for @CKSyncEngineDidFetchRecordZoneChangesEvent@.
module ObjC.CloudKit.CKSyncEngineDidFetchRecordZoneChangesEvent
  ( CKSyncEngineDidFetchRecordZoneChangesEvent
  , IsCKSyncEngineDidFetchRecordZoneChangesEvent(..)
  , zoneID
  , error_
  , errorSelector
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
zoneID :: IsCKSyncEngineDidFetchRecordZoneChangesEvent ckSyncEngineDidFetchRecordZoneChangesEvent => ckSyncEngineDidFetchRecordZoneChangesEvent -> IO (Id CKRecordZoneID)
zoneID ckSyncEngineDidFetchRecordZoneChangesEvent =
  sendMessage ckSyncEngineDidFetchRecordZoneChangesEvent zoneIDSelector

-- | @- error@
error_ :: IsCKSyncEngineDidFetchRecordZoneChangesEvent ckSyncEngineDidFetchRecordZoneChangesEvent => ckSyncEngineDidFetchRecordZoneChangesEvent -> IO (Id NSError)
error_ ckSyncEngineDidFetchRecordZoneChangesEvent =
  sendMessage ckSyncEngineDidFetchRecordZoneChangesEvent errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

