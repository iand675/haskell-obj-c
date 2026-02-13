{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSyncEngineFetchedZoneDeletion@.
module ObjC.CloudKit.CKSyncEngineFetchedZoneDeletion
  ( CKSyncEngineFetchedZoneDeletion
  , IsCKSyncEngineFetchedZoneDeletion(..)
  , init_
  , new
  , zoneID
  , reason
  , initSelector
  , newSelector
  , reasonSelector
  , zoneIDSelector

  -- * Enum types
  , CKSyncEngineZoneDeletionReason(CKSyncEngineZoneDeletionReason)
  , pattern CKSyncEngineZoneDeletionReasonDeleted
  , pattern CKSyncEngineZoneDeletionReasonPurged
  , pattern CKSyncEngineZoneDeletionReasonEncryptedDataReset

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineFetchedZoneDeletion ckSyncEngineFetchedZoneDeletion => ckSyncEngineFetchedZoneDeletion -> IO (Id CKSyncEngineFetchedZoneDeletion)
init_ ckSyncEngineFetchedZoneDeletion =
  sendOwnedMessage ckSyncEngineFetchedZoneDeletion initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineFetchedZoneDeletion)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFetchedZoneDeletion"
    sendOwnedClassMessage cls' newSelector

-- | @- zoneID@
zoneID :: IsCKSyncEngineFetchedZoneDeletion ckSyncEngineFetchedZoneDeletion => ckSyncEngineFetchedZoneDeletion -> IO (Id CKRecordZoneID)
zoneID ckSyncEngineFetchedZoneDeletion =
  sendMessage ckSyncEngineFetchedZoneDeletion zoneIDSelector

-- | @- reason@
reason :: IsCKSyncEngineFetchedZoneDeletion ckSyncEngineFetchedZoneDeletion => ckSyncEngineFetchedZoneDeletion -> IO CKSyncEngineZoneDeletionReason
reason ckSyncEngineFetchedZoneDeletion =
  sendMessage ckSyncEngineFetchedZoneDeletion reasonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineFetchedZoneDeletion)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineFetchedZoneDeletion)
newSelector = mkSelector "new"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] CKSyncEngineZoneDeletionReason
reasonSelector = mkSelector "reason"

