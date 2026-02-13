{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished sending a batch of record zone changes to the server.
--
-- If a record save succeeded, you should encode the system fields of this record to use the next time you save. See @encodeSystemFields@ on ``CKRecord``.
--
-- If a record deletion succeeded, you should remove any local system fields for that record.
--
-- If the record change failed, try to resolve the issue causing the error and save the record again if necessary.
--
-- Generated bindings for @CKSyncEngineSentRecordZoneChangesEvent@.
module ObjC.CloudKit.CKSyncEngineSentRecordZoneChangesEvent
  ( CKSyncEngineSentRecordZoneChangesEvent
  , IsCKSyncEngineSentRecordZoneChangesEvent(..)
  , savedRecords
  , failedRecordSaves
  , deletedRecordIDs
  , failedRecordDeletes
  , deletedRecordIDsSelector
  , failedRecordDeletesSelector
  , failedRecordSavesSelector
  , savedRecordsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- savedRecords@
savedRecords :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSArray)
savedRecords ckSyncEngineSentRecordZoneChangesEvent =
  sendMessage ckSyncEngineSentRecordZoneChangesEvent savedRecordsSelector

-- | @- failedRecordSaves@
failedRecordSaves :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSArray)
failedRecordSaves ckSyncEngineSentRecordZoneChangesEvent =
  sendMessage ckSyncEngineSentRecordZoneChangesEvent failedRecordSavesSelector

-- | @- deletedRecordIDs@
deletedRecordIDs :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSArray)
deletedRecordIDs ckSyncEngineSentRecordZoneChangesEvent =
  sendMessage ckSyncEngineSentRecordZoneChangesEvent deletedRecordIDsSelector

-- | @- failedRecordDeletes@
failedRecordDeletes :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSDictionary)
failedRecordDeletes ckSyncEngineSentRecordZoneChangesEvent =
  sendMessage ckSyncEngineSentRecordZoneChangesEvent failedRecordDeletesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @savedRecords@
savedRecordsSelector :: Selector '[] (Id NSArray)
savedRecordsSelector = mkSelector "savedRecords"

-- | @Selector@ for @failedRecordSaves@
failedRecordSavesSelector :: Selector '[] (Id NSArray)
failedRecordSavesSelector = mkSelector "failedRecordSaves"

-- | @Selector@ for @deletedRecordIDs@
deletedRecordIDsSelector :: Selector '[] (Id NSArray)
deletedRecordIDsSelector = mkSelector "deletedRecordIDs"

-- | @Selector@ for @failedRecordDeletes@
failedRecordDeletesSelector :: Selector '[] (Id NSDictionary)
failedRecordDeletesSelector = mkSelector "failedRecordDeletes"

