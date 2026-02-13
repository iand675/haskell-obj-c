{-# LANGUAGE DataKinds #-}
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
  , containsPendingRecordZoneChangeSelector
  , containsRecordIDSelector
  , excludedZoneIDsSelector
  , initWithExcludedZoneIDsSelector
  , initWithRecordIDsSelector
  , initWithZoneIDsSelector
  , recordIDsSelector
  , zoneIDsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a scope that contains only the given zone IDs. If @zoneIDs@ is nil, then this scope contains all zones.
--
-- ObjC selector: @- initWithZoneIDs:@
initWithZoneIDs :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsNSSet zoneIDs) => ckSyncEngineSendChangesScope -> zoneIDs -> IO (Id CKSyncEngineSendChangesScope)
initWithZoneIDs ckSyncEngineSendChangesScope zoneIDs =
  sendOwnedMessage ckSyncEngineSendChangesScope initWithZoneIDsSelector (toNSSet zoneIDs)

-- | Creates a scope that contains all zones except for the given zone IDs.
--
-- ObjC selector: @- initWithExcludedZoneIDs:@
initWithExcludedZoneIDs :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsNSSet excludedZoneIDs) => ckSyncEngineSendChangesScope -> excludedZoneIDs -> IO (Id CKSyncEngineSendChangesScope)
initWithExcludedZoneIDs ckSyncEngineSendChangesScope excludedZoneIDs =
  sendOwnedMessage ckSyncEngineSendChangesScope initWithExcludedZoneIDsSelector (toNSSet excludedZoneIDs)

-- | Creates a scope that includes only the given record IDs. If @recordIDs@ is nil, this scope contains all records.
--
-- ObjC selector: @- initWithRecordIDs:@
initWithRecordIDs :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsNSSet recordIDs) => ckSyncEngineSendChangesScope -> recordIDs -> IO (Id CKSyncEngineSendChangesScope)
initWithRecordIDs ckSyncEngineSendChangesScope recordIDs =
  sendOwnedMessage ckSyncEngineSendChangesScope initWithRecordIDsSelector (toNSSet recordIDs)

-- | Returns true if this scope includes the given record ID.
--
-- ObjC selector: @- containsRecordID:@
containsRecordID :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsCKRecordID recordID) => ckSyncEngineSendChangesScope -> recordID -> IO Bool
containsRecordID ckSyncEngineSendChangesScope recordID =
  sendMessage ckSyncEngineSendChangesScope containsRecordIDSelector (toCKRecordID recordID)

-- | Returns true if this scope includes the given pending change.
--
-- ObjC selector: @- containsPendingRecordZoneChange:@
containsPendingRecordZoneChange :: (IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope, IsCKSyncEnginePendingRecordZoneChange pendingRecordZoneChange) => ckSyncEngineSendChangesScope -> pendingRecordZoneChange -> IO Bool
containsPendingRecordZoneChange ckSyncEngineSendChangesScope pendingRecordZoneChange =
  sendMessage ckSyncEngineSendChangesScope containsPendingRecordZoneChangeSelector (toCKSyncEnginePendingRecordZoneChange pendingRecordZoneChange)

-- | The scope of zone IDs in which to send changes.
--
-- If you only want to send changes for a particular set of zones, you can initialize your scope with those zone IDs. When creating the next batch of changes to send to the server, consult this and only send changes within these zones. If this and @recordIDs@ are @nil@, then you should send all changes.
--
-- ObjC selector: @- zoneIDs@
zoneIDs :: IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope => ckSyncEngineSendChangesScope -> IO (Id NSSet)
zoneIDs ckSyncEngineSendChangesScope =
  sendMessage ckSyncEngineSendChangesScope zoneIDsSelector

-- | A specific set of zone IDs to exclude from this scope. If you know that you don't want to send changes for a particular set of zones, you can set those zones here.
--
-- Note that if @zoneIDs@ is set, then  @excludedZoneIDs@ will always be empty.
--
-- ObjC selector: @- excludedZoneIDs@
excludedZoneIDs :: IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope => ckSyncEngineSendChangesScope -> IO (Id NSSet)
excludedZoneIDs ckSyncEngineSendChangesScope =
  sendMessage ckSyncEngineSendChangesScope excludedZoneIDsSelector

-- | The scope of record IDs in which to send changes.
--
-- If you only want to send changes for a particular set of records, you can initialize your scope with those records IDs. When creating the next batch of changes to send to the server, consult this property and only send changes for these record IDs. If this and @zoneIDs@ are @nil@, then you should send all changes.
--
-- ObjC selector: @- recordIDs@
recordIDs :: IsCKSyncEngineSendChangesScope ckSyncEngineSendChangesScope => ckSyncEngineSendChangesScope -> IO (Id NSSet)
recordIDs ckSyncEngineSendChangesScope =
  sendMessage ckSyncEngineSendChangesScope recordIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneIDs:@
initWithZoneIDsSelector :: Selector '[Id NSSet] (Id CKSyncEngineSendChangesScope)
initWithZoneIDsSelector = mkSelector "initWithZoneIDs:"

-- | @Selector@ for @initWithExcludedZoneIDs:@
initWithExcludedZoneIDsSelector :: Selector '[Id NSSet] (Id CKSyncEngineSendChangesScope)
initWithExcludedZoneIDsSelector = mkSelector "initWithExcludedZoneIDs:"

-- | @Selector@ for @initWithRecordIDs:@
initWithRecordIDsSelector :: Selector '[Id NSSet] (Id CKSyncEngineSendChangesScope)
initWithRecordIDsSelector = mkSelector "initWithRecordIDs:"

-- | @Selector@ for @containsRecordID:@
containsRecordIDSelector :: Selector '[Id CKRecordID] Bool
containsRecordIDSelector = mkSelector "containsRecordID:"

-- | @Selector@ for @containsPendingRecordZoneChange:@
containsPendingRecordZoneChangeSelector :: Selector '[Id CKSyncEnginePendingRecordZoneChange] Bool
containsPendingRecordZoneChangeSelector = mkSelector "containsPendingRecordZoneChange:"

-- | @Selector@ for @zoneIDs@
zoneIDsSelector :: Selector '[] (Id NSSet)
zoneIDsSelector = mkSelector "zoneIDs"

-- | @Selector@ for @excludedZoneIDs@
excludedZoneIDsSelector :: Selector '[] (Id NSSet)
excludedZoneIDsSelector = mkSelector "excludedZoneIDs"

-- | @Selector@ for @recordIDs@
recordIDsSelector :: Selector '[] (Id NSSet)
recordIDsSelector = mkSelector "recordIDs"

