{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A change in a record zone that needs to be sent to the server.
--
-- Generated bindings for @CKSyncEnginePendingRecordZoneChange@.
module ObjC.CloudKit.CKSyncEnginePendingRecordZoneChange
  ( CKSyncEnginePendingRecordZoneChange
  , IsCKSyncEnginePendingRecordZoneChange(..)
  , initWithRecordID_type
  , init_
  , new
  , recordID
  , type_
  , initSelector
  , initWithRecordID_typeSelector
  , newSelector
  , recordIDSelector
  , typeSelector

  -- * Enum types
  , CKSyncEnginePendingRecordZoneChangeType(CKSyncEnginePendingRecordZoneChangeType)
  , pattern CKSyncEnginePendingRecordZoneChangeTypeSaveRecord
  , pattern CKSyncEnginePendingRecordZoneChangeTypeDeleteRecord

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

-- | @- initWithRecordID:type:@
initWithRecordID_type :: (IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange, IsCKRecordID recordID) => ckSyncEnginePendingRecordZoneChange -> recordID -> CKSyncEnginePendingRecordZoneChangeType -> IO (Id CKSyncEnginePendingRecordZoneChange)
initWithRecordID_type ckSyncEnginePendingRecordZoneChange recordID type_ =
  sendOwnedMessage ckSyncEnginePendingRecordZoneChange initWithRecordID_typeSelector (toCKRecordID recordID) type_

-- | @- init@
init_ :: IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange => ckSyncEnginePendingRecordZoneChange -> IO (Id CKSyncEnginePendingRecordZoneChange)
init_ ckSyncEnginePendingRecordZoneChange =
  sendOwnedMessage ckSyncEnginePendingRecordZoneChange initSelector

-- | @+ new@
new :: IO (Id CKSyncEnginePendingRecordZoneChange)
new  =
  do
    cls' <- getRequiredClass "CKSyncEnginePendingRecordZoneChange"
    sendOwnedClassMessage cls' newSelector

-- | @- recordID@
recordID :: IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange => ckSyncEnginePendingRecordZoneChange -> IO (Id CKRecordID)
recordID ckSyncEnginePendingRecordZoneChange =
  sendMessage ckSyncEnginePendingRecordZoneChange recordIDSelector

-- | @- type@
type_ :: IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange => ckSyncEnginePendingRecordZoneChange -> IO CKSyncEnginePendingRecordZoneChangeType
type_ ckSyncEnginePendingRecordZoneChange =
  sendMessage ckSyncEnginePendingRecordZoneChange typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecordID:type:@
initWithRecordID_typeSelector :: Selector '[Id CKRecordID, CKSyncEnginePendingRecordZoneChangeType] (Id CKSyncEnginePendingRecordZoneChange)
initWithRecordID_typeSelector = mkSelector "initWithRecordID:type:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEnginePendingRecordZoneChange)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEnginePendingRecordZoneChange)
newSelector = mkSelector "new"

-- | @Selector@ for @recordID@
recordIDSelector :: Selector '[] (Id CKRecordID)
recordIDSelector = mkSelector "recordID"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CKSyncEnginePendingRecordZoneChangeType
typeSelector = mkSelector "type"

