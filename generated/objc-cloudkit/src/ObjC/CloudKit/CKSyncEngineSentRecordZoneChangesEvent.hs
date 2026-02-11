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
  , savedRecordsSelector
  , failedRecordSavesSelector
  , deletedRecordIDsSelector
  , failedRecordDeletesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- savedRecords@
savedRecords :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSArray)
savedRecords ckSyncEngineSentRecordZoneChangesEvent  =
  sendMsg ckSyncEngineSentRecordZoneChangesEvent (mkSelector "savedRecords") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- failedRecordSaves@
failedRecordSaves :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSArray)
failedRecordSaves ckSyncEngineSentRecordZoneChangesEvent  =
  sendMsg ckSyncEngineSentRecordZoneChangesEvent (mkSelector "failedRecordSaves") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deletedRecordIDs@
deletedRecordIDs :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSArray)
deletedRecordIDs ckSyncEngineSentRecordZoneChangesEvent  =
  sendMsg ckSyncEngineSentRecordZoneChangesEvent (mkSelector "deletedRecordIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- failedRecordDeletes@
failedRecordDeletes :: IsCKSyncEngineSentRecordZoneChangesEvent ckSyncEngineSentRecordZoneChangesEvent => ckSyncEngineSentRecordZoneChangesEvent -> IO (Id NSDictionary)
failedRecordDeletes ckSyncEngineSentRecordZoneChangesEvent  =
  sendMsg ckSyncEngineSentRecordZoneChangesEvent (mkSelector "failedRecordDeletes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @savedRecords@
savedRecordsSelector :: Selector
savedRecordsSelector = mkSelector "savedRecords"

-- | @Selector@ for @failedRecordSaves@
failedRecordSavesSelector :: Selector
failedRecordSavesSelector = mkSelector "failedRecordSaves"

-- | @Selector@ for @deletedRecordIDs@
deletedRecordIDsSelector :: Selector
deletedRecordIDsSelector = mkSelector "deletedRecordIDs"

-- | @Selector@ for @failedRecordDeletes@
failedRecordDeletesSelector :: Selector
failedRecordDeletesSelector = mkSelector "failedRecordDeletes"

