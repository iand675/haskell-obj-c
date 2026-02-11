{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A scope in which the sync engine will send changes to  the server.
--
-- Generated bindings for @CKSyncEngineSendChangesScope@.
module ObjC.CloudKit.CKSyncEngineSendChangesScope
  ( CKSyncEngineSendChangesScope
  , IsCKSyncEngineSendChangesScope(..)
  , initWithZoneIDs
  , initWithExcludedZoneIDs
  , initWithRecordIDs
  , containsRecordID
  , containsPendingRecordZoneChange
  , zoneIDs
  , excludedZoneIDs
  , recordIDs
  , initWithZoneIDsSelector
  , initWithExcludedZoneIDsSelector
  , initWithRecordIDsSelector
  , containsRecordIDSelector
  , containsPendingRecordZoneChangeSelector
  , zoneIDsSelector
  , excludedZoneIDsSelector
  , recordIDsSelector


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

-- | Creates a scope that contains only the given zone IDs. If @zoneIDs@ is nil, then this scope contains all zones.
--
-- ObjC selector: @- initWithZoneIDs:@
initWithZoneIDs :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsNSSet zoneIDs) => ckSyncEngineSendChangesScope -> zoneIDs -> IO (Id CKSyncEngineSendChangesScope)
initWithZoneIDs ckSyncEngineSendChangesScope  zoneIDs =
withObjCPtr zoneIDs $ \raw_zoneIDs ->
    sendMsg ckSyncEngineSendChangesScope (mkSelector "initWithZoneIDs:") (retPtr retVoid) [argPtr (castPtr raw_zoneIDs :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a scope that contains all zones except for the given zone IDs.
--
-- ObjC selector: @- initWithExcludedZoneIDs:@
initWithExcludedZoneIDs :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsNSSet excludedZoneIDs) => ckSyncEngineSendChangesScope -> excludedZoneIDs -> IO (Id CKSyncEngineSendChangesScope)
initWithExcludedZoneIDs ckSyncEngineSendChangesScope  excludedZoneIDs =
withObjCPtr excludedZoneIDs $ \raw_excludedZoneIDs ->
    sendMsg ckSyncEngineSendChangesScope (mkSelector "initWithExcludedZoneIDs:") (retPtr retVoid) [argPtr (castPtr raw_excludedZoneIDs :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a scope that includes only the given record IDs. If @recordIDs@ is nil, this scope contains all records.
--
-- ObjC selector: @- initWithRecordIDs:@
initWithRecordIDs :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsNSSet recordIDs) => ckSyncEngineSendChangesScope -> recordIDs -> IO (Id CKSyncEngineSendChangesScope)
initWithRecordIDs ckSyncEngineSendChangesScope  recordIDs =
withObjCPtr recordIDs $ \raw_recordIDs ->
    sendMsg ckSyncEngineSendChangesScope (mkSelector "initWithRecordIDs:") (retPtr retVoid) [argPtr (castPtr raw_recordIDs :: Ptr ())] >>= ownedObject . castPtr

-- | Returns true if this scope includes the given record ID.
--
-- ObjC selector: @- containsRecordID:@
containsRecordID :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsCKRecordID recordID) => ckSyncEngineSendChangesScope -> recordID -> IO Bool
containsRecordID ckSyncEngineSendChangesScope  recordID =
withObjCPtr recordID $ \raw_recordID ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckSyncEngineSendChangesScope (mkSelector "containsRecordID:") retCULong [argPtr (castPtr raw_recordID :: Ptr ())]

-- | Returns true if this scope includes the given pending change.
--
-- ObjC selector: @- containsPendingRecordZoneChange:@
containsPendingRecordZoneChange :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsCKSyncEnginePendingRecordZoneChange pendingRecordZoneChange) => ckSyncEngineSendChangesScope -> pendingRecordZoneChange -> IO Bool
containsPendingRecordZoneChange ckSyncEngineSendChangesScope  pendingRecordZoneChange =
withObjCPtr pendingRecordZoneChange $ \raw_pendingRecordZoneChange ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckSyncEngineSendChangesScope (mkSelector "containsPendingRecordZoneChange:") retCULong [argPtr (castPtr raw_pendingRecordZoneChange :: Ptr ())]

-- | The scope of zone IDs in which to send changes.
--
-- If you only want to send changes for a particular set of zones, you can initialize your scope with those zone IDs. When creating the next batch of changes to send to the server, consult this and only send changes within these zones. If this and @recordIDs@ are @nil@, then you should send all changes.
--
-- ObjC selector: @- zoneIDs@
zoneIDs :: IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope => ckSyncEngineSendChangesScope -> IO (Id NSSet)
zoneIDs ckSyncEngineSendChangesScope  =
  sendMsg ckSyncEngineSendChangesScope (mkSelector "zoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A specific set of zone IDs to exclude from this scope. If you know that you don't want to send changes for a particular set of zones, you can set those zones here.
--
-- Note that if @zoneIDs@ is set, then  @excludedZoneIDs@ will always be empty.
--
-- ObjC selector: @- excludedZoneIDs@
excludedZoneIDs :: IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope => ckSyncEngineSendChangesScope -> IO (Id NSSet)
excludedZoneIDs ckSyncEngineSendChangesScope  =
  sendMsg ckSyncEngineSendChangesScope (mkSelector "excludedZoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The scope of record IDs in which to send changes.
--
-- If you only want to send changes for a particular set of records, you can initialize your scope with those records IDs. When creating the next batch of changes to send to the server, consult this property and only send changes for these record IDs. If this and @zoneIDs@ are @nil@, then you should send all changes.
--
-- ObjC selector: @- recordIDs@
recordIDs :: IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope => ckSyncEngineSendChangesScope -> IO (Id NSSet)
recordIDs ckSyncEngineSendChangesScope  =
  sendMsg ckSyncEngineSendChangesScope (mkSelector "recordIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneIDs:@
initWithZoneIDsSelector :: Selector
initWithZoneIDsSelector = mkSelector "initWithZoneIDs:"

-- | @Selector@ for @initWithExcludedZoneIDs:@
initWithExcludedZoneIDsSelector :: Selector
initWithExcludedZoneIDsSelector = mkSelector "initWithExcludedZoneIDs:"

-- | @Selector@ for @initWithRecordIDs:@
initWithRecordIDsSelector :: Selector
initWithRecordIDsSelector = mkSelector "initWithRecordIDs:"

-- | @Selector@ for @containsRecordID:@
containsRecordIDSelector :: Selector
containsRecordIDSelector = mkSelector "containsRecordID:"

-- | @Selector@ for @containsPendingRecordZoneChange:@
containsPendingRecordZoneChangeSelector :: Selector
containsPendingRecordZoneChangeSelector = mkSelector "containsPendingRecordZoneChange:"

-- | @Selector@ for @zoneIDs@
zoneIDsSelector :: Selector
zoneIDsSelector = mkSelector "zoneIDs"

-- | @Selector@ for @excludedZoneIDs@
excludedZoneIDsSelector :: Selector
excludedZoneIDsSelector = mkSelector "excludedZoneIDs"

-- | @Selector@ for @recordIDs@
recordIDsSelector :: Selector
recordIDsSelector = mkSelector "recordIDs"

