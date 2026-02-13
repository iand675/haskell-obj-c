{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , typeSelector
  , zoneIDSelector

  -- * Enum types
  , CKSyncEnginePendingDatabaseChangeType(CKSyncEnginePendingDatabaseChangeType)
  , pattern CKSyncEnginePendingDatabaseChangeTypeSaveZone
  , pattern CKSyncEnginePendingDatabaseChangeTypeDeleteZone

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
init_ :: IsCKSyncEnginePendingDatabaseChange ckSyncEnginePendingDatabaseChange => ckSyncEnginePendingDatabaseChange -> IO (Id CKSyncEnginePendingDatabaseChange)
init_ ckSyncEnginePendingDatabaseChange =
  sendOwnedMessage ckSyncEnginePendingDatabaseChange initSelector

-- | @+ new@
new :: IO (Id CKSyncEnginePendingDatabaseChange)
new  =
  do
    cls' <- getRequiredClass "CKSyncEnginePendingDatabaseChange"
    sendOwnedClassMessage cls' newSelector

-- | @- zoneID@
zoneID :: IsCKSyncEnginePendingDatabaseChange ckSyncEnginePendingDatabaseChange => ckSyncEnginePendingDatabaseChange -> IO (Id CKRecordZoneID)
zoneID ckSyncEnginePendingDatabaseChange =
  sendMessage ckSyncEnginePendingDatabaseChange zoneIDSelector

-- | @- type@
type_ :: IsCKSyncEnginePendingDatabaseChange ckSyncEnginePendingDatabaseChange => ckSyncEnginePendingDatabaseChange -> IO CKSyncEnginePendingDatabaseChangeType
type_ ckSyncEnginePendingDatabaseChange =
  sendMessage ckSyncEnginePendingDatabaseChange typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEnginePendingDatabaseChange)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEnginePendingDatabaseChange)
newSelector = mkSelector "new"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CKSyncEnginePendingDatabaseChangeType
typeSelector = mkSelector "type"

