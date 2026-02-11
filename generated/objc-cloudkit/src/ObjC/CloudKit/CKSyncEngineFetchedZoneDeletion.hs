{-# LANGUAGE PatternSynonyms #-}
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
  , zoneIDSelector
  , reasonSelector

  -- * Enum types
  , CKSyncEngineZoneDeletionReason(CKSyncEngineZoneDeletionReason)
  , pattern CKSyncEngineZoneDeletionReasonDeleted
  , pattern CKSyncEngineZoneDeletionReasonPurged
  , pattern CKSyncEngineZoneDeletionReasonEncryptedDataReset

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
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineFetchedZoneDeletion ckSyncEngineFetchedZoneDeletion => ckSyncEngineFetchedZoneDeletion -> IO (Id CKSyncEngineFetchedZoneDeletion)
init_ ckSyncEngineFetchedZoneDeletion  =
  sendMsg ckSyncEngineFetchedZoneDeletion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineFetchedZoneDeletion)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFetchedZoneDeletion"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- zoneID@
zoneID :: IsCKSyncEngineFetchedZoneDeletion ckSyncEngineFetchedZoneDeletion => ckSyncEngineFetchedZoneDeletion -> IO (Id CKRecordZoneID)
zoneID ckSyncEngineFetchedZoneDeletion  =
  sendMsg ckSyncEngineFetchedZoneDeletion (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reason@
reason :: IsCKSyncEngineFetchedZoneDeletion ckSyncEngineFetchedZoneDeletion => ckSyncEngineFetchedZoneDeletion -> IO CKSyncEngineZoneDeletionReason
reason ckSyncEngineFetchedZoneDeletion  =
  fmap (coerce :: CLong -> CKSyncEngineZoneDeletionReason) $ sendMsg ckSyncEngineFetchedZoneDeletion (mkSelector "reason") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

