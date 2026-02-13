{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSyncEngineFailedRecordSave@.
module ObjC.CloudKit.CKSyncEngineFailedRecordSave
  ( CKSyncEngineFailedRecordSave
  , IsCKSyncEngineFailedRecordSave(..)
  , init_
  , new
  , record
  , error_
  , errorSelector
  , initSelector
  , newSelector
  , recordSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineFailedRecordSave ckSyncEngineFailedRecordSave => ckSyncEngineFailedRecordSave -> IO (Id CKSyncEngineFailedRecordSave)
init_ ckSyncEngineFailedRecordSave =
  sendOwnedMessage ckSyncEngineFailedRecordSave initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineFailedRecordSave)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFailedRecordSave"
    sendOwnedClassMessage cls' newSelector

-- | @- record@
record :: IsCKSyncEngineFailedRecordSave ckSyncEngineFailedRecordSave => ckSyncEngineFailedRecordSave -> IO (Id CKRecord)
record ckSyncEngineFailedRecordSave =
  sendMessage ckSyncEngineFailedRecordSave recordSelector

-- | @- error@
error_ :: IsCKSyncEngineFailedRecordSave ckSyncEngineFailedRecordSave => ckSyncEngineFailedRecordSave -> IO (Id NSError)
error_ ckSyncEngineFailedRecordSave =
  sendMessage ckSyncEngineFailedRecordSave errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineFailedRecordSave)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineFailedRecordSave)
newSelector = mkSelector "new"

-- | @Selector@ for @record@
recordSelector :: Selector '[] (Id CKRecord)
recordSelector = mkSelector "record"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

