{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A change in a database that needs to be sent to the server.
--
-- Generated bindings for @CKSyncEnginePendingDatabaseChange@.
module ObjC.CloudKit.CKSyncEnginePendingDatabaseChange
  ( CKSyncEnginePendingDatabaseChange
  , IsCKSyncEnginePendingDatabaseChange(..)
  , init_
  , new
  , zoneID
  , type_
  , initSelector
  , newSelector
  , zoneIDSelector
  , typeSelector

  -- * Enum types
  , CKSyncEnginePendingDatabaseChangeType(CKSyncEnginePendingDatabaseChangeType)
  , pattern CKSyncEnginePendingDatabaseChangeTypeSaveZone
  , pattern CKSyncEnginePendingDatabaseChangeTypeDeleteZone

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
init_ :: IsCKSyncEnginePendingDatabaseChange ckSyncEnginePendingDatabaseChange => ckSyncEnginePendingDatabaseChange -> IO (Id CKSyncEnginePendingDatabaseChange)
init_ ckSyncEnginePendingDatabaseChange  =
  sendMsg ckSyncEnginePendingDatabaseChange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEnginePendingDatabaseChange)
new  =
  do
    cls' <- getRequiredClass "CKSyncEnginePendingDatabaseChange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- zoneID@
zoneID :: IsCKSyncEnginePendingDatabaseChange ckSyncEnginePendingDatabaseChange => ckSyncEnginePendingDatabaseChange -> IO (Id CKRecordZoneID)
zoneID ckSyncEnginePendingDatabaseChange  =
  sendMsg ckSyncEnginePendingDatabaseChange (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsCKSyncEnginePendingDatabaseChange ckSyncEnginePendingDatabaseChange => ckSyncEnginePendingDatabaseChange -> IO CKSyncEnginePendingDatabaseChangeType
type_ ckSyncEnginePendingDatabaseChange  =
  fmap (coerce :: CLong -> CKSyncEnginePendingDatabaseChangeType) $ sendMsg ckSyncEnginePendingDatabaseChange (mkSelector "type") retCLong []

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

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

