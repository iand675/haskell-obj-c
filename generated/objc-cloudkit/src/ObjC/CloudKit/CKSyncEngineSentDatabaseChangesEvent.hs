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
  , savedZonesSelector
  , failedZoneSavesSelector
  , deletedZoneIDsSelector
  , failedZoneDeletesSelector


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

-- | @- savedZones@
savedZones :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSArray)
savedZones ckSyncEngineSentDatabaseChangesEvent  =
  sendMsg ckSyncEngineSentDatabaseChangesEvent (mkSelector "savedZones") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- failedZoneSaves@
failedZoneSaves :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSArray)
failedZoneSaves ckSyncEngineSentDatabaseChangesEvent  =
  sendMsg ckSyncEngineSentDatabaseChangesEvent (mkSelector "failedZoneSaves") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deletedZoneIDs@
deletedZoneIDs :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSArray)
deletedZoneIDs ckSyncEngineSentDatabaseChangesEvent  =
  sendMsg ckSyncEngineSentDatabaseChangesEvent (mkSelector "deletedZoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- failedZoneDeletes@
failedZoneDeletes :: IsCKSyncEngineSentDatabaseChangesEvent ckSyncEngineSentDatabaseChangesEvent => ckSyncEngineSentDatabaseChangesEvent -> IO (Id NSDictionary)
failedZoneDeletes ckSyncEngineSentDatabaseChangesEvent  =
  sendMsg ckSyncEngineSentDatabaseChangesEvent (mkSelector "failedZoneDeletes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @savedZones@
savedZonesSelector :: Selector
savedZonesSelector = mkSelector "savedZones"

-- | @Selector@ for @failedZoneSaves@
failedZoneSavesSelector :: Selector
failedZoneSavesSelector = mkSelector "failedZoneSaves"

-- | @Selector@ for @deletedZoneIDs@
deletedZoneIDsSelector :: Selector
deletedZoneIDsSelector = mkSelector "deletedZoneIDs"

-- | @Selector@ for @failedZoneDeletes@
failedZoneDeletesSelector :: Selector
failedZoneDeletesSelector = mkSelector "failedZoneDeletes"

