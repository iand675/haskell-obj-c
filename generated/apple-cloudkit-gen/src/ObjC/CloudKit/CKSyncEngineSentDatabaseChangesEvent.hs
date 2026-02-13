{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished sending a batch of database changes to the server.
--
-- If a change failed, try to resolve the issue causing the error and make the change again if necessary.
--
-- Generated bindings for @CKSyncEngineSentDatabaseChangesEvent@.
module ObjC.CloudKit.CKSyncEngineSentDatabaseChangesEvent
  ( CKSyncEngineSentDatabaseChangesEvent
  , IsCKSyncEngineSentDatabaseChangesEvent(..)
  , savedZones
  , failedZoneSaves
  , deletedZoneIDs
  , failedZoneDeletes
  , deletedZoneIDsSelector
  , failedZoneDeletesSelector
  , failedZoneSavesSelector
  , savedZonesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- savedZones@
savedZones :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSArray)
savedZones ckSyncEngineSentDatabaseChangesEvent =
  sendMessage ckSyncEngineSentDatabaseChangesEvent savedZonesSelector

-- | @- failedZoneSaves@
failedZoneSaves :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSArray)
failedZoneSaves ckSyncEngineSentDatabaseChangesEvent =
  sendMessage ckSyncEngineSentDatabaseChangesEvent failedZoneSavesSelector

-- | @- deletedZoneIDs@
deletedZoneIDs :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSArray)
deletedZoneIDs ckSyncEngineSentDatabaseChangesEvent =
  sendMessage ckSyncEngineSentDatabaseChangesEvent deletedZoneIDsSelector

-- | @- failedZoneDeletes@
failedZoneDeletes :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSDictionary)
failedZoneDeletes ckSyncEngineSentDatabaseChangesEvent =
  sendMessage ckSyncEngineSentDatabaseChangesEvent failedZoneDeletesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @savedZones@
savedZonesSelector :: Selector '[] (Id NSArray)
savedZonesSelector = mkSelector "savedZones"

-- | @Selector@ for @failedZoneSaves@
failedZoneSavesSelector :: Selector '[] (Id NSArray)
failedZoneSavesSelector = mkSelector "failedZoneSaves"

-- | @Selector@ for @deletedZoneIDs@
deletedZoneIDsSelector :: Selector '[] (Id NSArray)
deletedZoneIDsSelector = mkSelector "deletedZoneIDs"

-- | @Selector@ for @failedZoneDeletes@
failedZoneDeletesSelector :: Selector '[] (Id NSDictionary)
failedZoneDeletesSelector = mkSelector "failedZoneDeletes"

