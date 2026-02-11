{-# LANGUAGE PatternSynonyms #-}
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
  , initWithRecordID_typeSelector
  , initSelector
  , newSelector
  , recordIDSelector
  , typeSelector

  -- * Enum types
  , CKSyncEnginePendingRecordZoneChangeType(CKSyncEnginePendingRecordZoneChangeType)
  , pattern CKSyncEnginePendingRecordZoneChangeTypeSaveRecord
  , pattern CKSyncEnginePendingRecordZoneChangeTypeDeleteRecord

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

-- | @- initWithRecordID:type:@
initWithRecordID_type :: (IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange, IsCKRecordID recordID) => ckSyncEnginePendingRecordZoneChange -> recordID -> CKSyncEnginePendingRecordZoneChangeType -> IO (Id CKSyncEnginePendingRecordZoneChange)
initWithRecordID_type ckSyncEnginePendingRecordZoneChange  recordID type_ =
withObjCPtr recordID $ \raw_recordID ->
    sendMsg ckSyncEnginePendingRecordZoneChange (mkSelector "initWithRecordID:type:") (retPtr retVoid) [argPtr (castPtr raw_recordID :: Ptr ()), argCLong (coerce type_)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange => ckSyncEnginePendingRecordZoneChange -> IO (Id CKSyncEnginePendingRecordZoneChange)
init_ ckSyncEnginePendingRecordZoneChange  =
  sendMsg ckSyncEnginePendingRecordZoneChange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEnginePendingRecordZoneChange)
new  =
  do
    cls' <- getRequiredClass "CKSyncEnginePendingRecordZoneChange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- recordID@
recordID :: IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange => ckSyncEnginePendingRecordZoneChange -> IO (Id CKRecordID)
recordID ckSyncEnginePendingRecordZoneChange  =
  sendMsg ckSyncEnginePendingRecordZoneChange (mkSelector "recordID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsCKSyncEnginePendingRecordZoneChange ckSyncEnginePendingRecordZoneChange => ckSyncEnginePendingRecordZoneChange -> IO CKSyncEnginePendingRecordZoneChangeType
type_ ckSyncEnginePendingRecordZoneChange  =
  fmap (coerce :: CLong -> CKSyncEnginePendingRecordZoneChangeType) $ sendMsg ckSyncEnginePendingRecordZoneChange (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecordID:type:@
initWithRecordID_typeSelector :: Selector
initWithRecordID_typeSelector = mkSelector "initWithRecordID:type:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @recordID@
recordIDSelector :: Selector
recordIDSelector = mkSelector "recordID"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

