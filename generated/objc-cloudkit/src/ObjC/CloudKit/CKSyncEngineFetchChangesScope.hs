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
  , initWithZoneIDsSelector
  , initWithExcludedZoneIDsSelector
  , containsZoneIDSelector
  , zoneIDsSelector
  , excludedZoneIDsSelector


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

-- | Creates a scope that includes only the specified set of zones.
--
-- ObjC selector: @- initWithZoneIDs:@
initWithZoneIDs :: (IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope, IsNSSet zoneIDs) => ckSyncEngineFetchChangesScope -> zoneIDs -> IO (Id CKSyncEngineFetchChangesScope)
initWithZoneIDs ckSyncEngineFetchChangesScope  zoneIDs =
withObjCPtr zoneIDs $ \raw_zoneIDs ->
    sendMsg ckSyncEngineFetchChangesScope (mkSelector "initWithZoneIDs:") (retPtr retVoid) [argPtr (castPtr raw_zoneIDs :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a scope that includes all zones except the specified excluded zones.
--
-- ObjC selector: @- initWithExcludedZoneIDs:@
initWithExcludedZoneIDs :: (IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope, IsNSSet zoneIDs) => ckSyncEngineFetchChangesScope -> zoneIDs -> IO (Id CKSyncEngineFetchChangesScope)
initWithExcludedZoneIDs ckSyncEngineFetchChangesScope  zoneIDs =
withObjCPtr zoneIDs $ \raw_zoneIDs ->
    sendMsg ckSyncEngineFetchChangesScope (mkSelector "initWithExcludedZoneIDs:") (retPtr retVoid) [argPtr (castPtr raw_zoneIDs :: Ptr ())] >>= ownedObject . castPtr

-- | Returns true if the specified zone ID is included in this scope.
--
-- ObjC selector: @- containsZoneID:@
containsZoneID :: (IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope, IsCKRecordZoneID zoneID) => ckSyncEngineFetchChangesScope -> zoneID -> IO Bool
containsZoneID ckSyncEngineFetchChangesScope  zoneID =
withObjCPtr zoneID $ \raw_zoneID ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckSyncEngineFetchChangesScope (mkSelector "containsZoneID:") retCULong [argPtr (castPtr raw_zoneID :: Ptr ())]

-- | A specific set of zone IDs to include in the scope. For example, if you want to fetch changes for a specific set of zones, you can specify them here. If @nil@, this scope includes all zones except those in @excludedZoneIDs@.
--
-- ObjC selector: @- zoneIDs@
zoneIDs :: IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope => ckSyncEngineFetchChangesScope -> IO (Id NSSet)
zoneIDs ckSyncEngineFetchChangesScope  =
  sendMsg ckSyncEngineFetchChangesScope (mkSelector "zoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A specific set of zone IDs to exclude from this scope. If you know that you don't want to fetch changes for a particular set of zones, you can set those zones here.
--
-- ObjC selector: @- excludedZoneIDs@
excludedZoneIDs :: IsCKSyncEngineFetchChangesScope ckSyncEngineFetchChangesScope => ckSyncEngineFetchChangesScope -> IO (Id NSSet)
excludedZoneIDs ckSyncEngineFetchChangesScope  =
  sendMsg ckSyncEngineFetchChangesScope (mkSelector "excludedZoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneIDs:@
initWithZoneIDsSelector :: Selector
initWithZoneIDsSelector = mkSelector "initWithZoneIDs:"

-- | @Selector@ for @initWithExcludedZoneIDs:@
initWithExcludedZoneIDsSelector :: Selector
initWithExcludedZoneIDsSelector = mkSelector "initWithExcludedZoneIDs:"

-- | @Selector@ for @containsZoneID:@
containsZoneIDSelector :: Selector
containsZoneIDSelector = mkSelector "containsZoneID:"

-- | @Selector@ for @zoneIDs@
zoneIDsSelector :: Selector
zoneIDsSelector = mkSelector "zoneIDs"

-- | @Selector@ for @excludedZoneIDs@
excludedZoneIDsSelector :: Selector
excludedZoneIDsSelector = mkSelector "excludedZoneIDs"

