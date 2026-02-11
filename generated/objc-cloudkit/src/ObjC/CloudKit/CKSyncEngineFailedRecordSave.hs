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
  , initSelector
  , newSelector
  , recordSelector
  , errorSelector


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

-- | @- init@
init_ :: IsCKSyncEngineFailedRecordSave ckSyncEngineFailedRecordSave => ckSyncEngineFailedRecordSave -> IO (Id CKSyncEngineFailedRecordSave)
init_ ckSyncEngineFailedRecordSave  =
  sendMsg ckSyncEngineFailedRecordSave (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineFailedRecordSave)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFailedRecordSave"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- record@
record :: IsCKSyncEngineFailedRecordSave ckSyncEngineFailedRecordSave => ckSyncEngineFailedRecordSave -> IO (Id CKRecord)
record ckSyncEngineFailedRecordSave  =
  sendMsg ckSyncEngineFailedRecordSave (mkSelector "record") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- error@
error_ :: IsCKSyncEngineFailedRecordSave ckSyncEngineFailedRecordSave => ckSyncEngineFailedRecordSave -> IO (Id NSError)
error_ ckSyncEngineFailedRecordSave  =
  sendMsg ckSyncEngineFailedRecordSave (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @record@
recordSelector :: Selector
recordSelector = mkSelector "record"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

