{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSyncEngineFetchedRecordDeletion@.
module ObjC.CloudKit.CKSyncEngineFetchedRecordDeletion
  ( CKSyncEngineFetchedRecordDeletion
  , IsCKSyncEngineFetchedRecordDeletion(..)
  , init_
  , new
  , recordID
  , recordType
  , initSelector
  , newSelector
  , recordIDSelector
  , recordTypeSelector


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
init_ :: IsCKSyncEngineFetchedRecordDeletion ckSyncEngineFetchedRecordDeletion => ckSyncEngineFetchedRecordDeletion -> IO (Id CKSyncEngineFetchedRecordDeletion)
init_ ckSyncEngineFetchedRecordDeletion  =
  sendMsg ckSyncEngineFetchedRecordDeletion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineFetchedRecordDeletion)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFetchedRecordDeletion"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- recordID@
recordID :: IsCKSyncEngineFetchedRecordDeletion ckSyncEngineFetchedRecordDeletion => ckSyncEngineFetchedRecordDeletion -> IO (Id CKRecordID)
recordID ckSyncEngineFetchedRecordDeletion  =
  sendMsg ckSyncEngineFetchedRecordDeletion (mkSelector "recordID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recordType@
recordType :: IsCKSyncEngineFetchedRecordDeletion ckSyncEngineFetchedRecordDeletion => ckSyncEngineFetchedRecordDeletion -> IO (Id NSString)
recordType ckSyncEngineFetchedRecordDeletion  =
  sendMsg ckSyncEngineFetchedRecordDeletion (mkSelector "recordType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @recordID@
recordIDSelector :: Selector
recordIDSelector = mkSelector "recordID"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector
recordTypeSelector = mkSelector "recordType"

