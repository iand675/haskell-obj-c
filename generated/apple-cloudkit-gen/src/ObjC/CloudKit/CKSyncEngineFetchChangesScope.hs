{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A scope in which the sync engine will fetch changes from the server.
--
-- Generated bindings for @CKSyncEngineFetchChangesScope@.
module ObjC.CloudKit.CKSyncEngineFetchChangesScope
  ( CKSyncEngineFetchChangesScope
  , IsCKSyncEngineFetchChangesScope(..)
  , initWithZoneIDs
  , initWithExcludedZoneIDs
  , containsZoneID
  , zoneIDs
  , excludedZoneIDs
  , containsZoneIDSelector
  , excludedZoneIDsSelector
  , initWithExcludedZoneIDsSelector
  , initWithZoneIDsSelector
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

-- | Creates a scope that includes only the specified set of zones.
--
-- ObjC selector: @- initWithZoneIDs:@
initWithZoneIDs :: (IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope, IsNSSet zoneIDs) => ckSyncEngineFetchChangesScope -> zoneIDs -> IO (Id CKSyncEngineFetchChangesScope)
initWithZoneIDs ckSyncEngineFetchChangesScope zoneIDs =
  sendOwnedMessage ckSyncEngineFetchChangesScope initWithZoneIDsSelector (toNSSet zoneIDs)

-- | Creates a scope that includes all zones except the specified excluded zones.
--
-- ObjC selector: @- initWithExcludedZoneIDs:@
initWithExcludedZoneIDs :: (IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope, IsNSSet zoneIDs) => ckSyncEngineFetchChangesScope -> zoneIDs -> IO (Id CKSyncEngineFetchChangesScope)
initWithExcludedZoneIDs ckSyncEngineFetchChangesScope zoneIDs =
  sendOwnedMessage ckSyncEngineFetchChangesScope initWithExcludedZoneIDsSelector (toNSSet zoneIDs)

-- | Returns true if the specified zone ID is included in this scope.
--
-- ObjC selector: @- containsZoneID:@
containsZoneID :: (IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope, IsCKRecordZoneID zoneID) => ckSyncEngineFetchChangesScope -> zoneID -> IO Bool
containsZoneID ckSyncEngineFetchChangesScope zoneID =
  sendMessage ckSyncEngineFetchChangesScope containsZoneIDSelector (toCKRecordZoneID zoneID)

-- | A specific set of zone IDs to include in the scope. For example, if you want to fetch changes for a specific set of zones, you can specify them here. If @nil@, this scope includes all zones except those in @excludedZoneIDs@.
--
-- ObjC selector: @- zoneIDs@
zoneIDs :: IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope => ckSyncEngineFetchChangesScope -> IO (Id NSSet)
zoneIDs ckSyncEngineFetchChangesScope =
  sendMessage ckSyncEngineFetchChangesScope zoneIDsSelector

-- | A specific set of zone IDs to exclude from this scope. If you know that you don't want to fetch changes for a particular set of zones, you can set those zones here.
--
-- ObjC selector: @- excludedZoneIDs@
excludedZoneIDs :: IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope => ckSyncEngineFetchChangesScope -> IO (Id NSSet)
excludedZoneIDs ckSyncEngineFetchChangesScope =
  sendMessage ckSyncEngineFetchChangesScope excludedZoneIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneIDs:@
initWithZoneIDsSelector :: Selector '[Id NSSet] (Id CKSyncEngineFetchChangesScope)
initWithZoneIDsSelector = mkSelector "initWithZoneIDs:"

-- | @Selector@ for @initWithExcludedZoneIDs:@
initWithExcludedZoneIDsSelector :: Selector '[Id NSSet] (Id CKSyncEngineFetchChangesScope)
initWithExcludedZoneIDsSelector = mkSelector "initWithExcludedZoneIDs:"

-- | @Selector@ for @containsZoneID:@
containsZoneIDSelector :: Selector '[Id CKRecordZoneID] Bool
containsZoneIDSelector = mkSelector "containsZoneID:"

-- | @Selector@ for @zoneIDs@
zoneIDsSelector :: Selector '[] (Id NSSet)
zoneIDsSelector = mkSelector "zoneIDs"

-- | @Selector@ for @excludedZoneIDs@
excludedZoneIDsSelector :: Selector '[] (Id NSSet)
excludedZoneIDsSelector = mkSelector "excludedZoneIDs"

