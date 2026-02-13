{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineFetchedRecordDeletion ckSyncEngineFetchedRecordDeletion => ckSyncEngineFetchedRecordDeletion -> IO (Id CKSyncEngineFetchedRecordDeletion)
init_ ckSyncEngineFetchedRecordDeletion =
  sendOwnedMessage ckSyncEngineFetchedRecordDeletion initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineFetchedRecordDeletion)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFetchedRecordDeletion"
    sendOwnedClassMessage cls' newSelector

-- | @- recordID@
recordID :: IsCKSyncEngineFetchedRecordDeletion ckSyncEngineFetchedRecordDeletion => ckSyncEngineFetchedRecordDeletion -> IO (Id CKRecordID)
recordID ckSyncEngineFetchedRecordDeletion =
  sendMessage ckSyncEngineFetchedRecordDeletion recordIDSelector

-- | @- recordType@
recordType :: IsCKSyncEngineFetchedRecordDeletion ckSyncEngineFetchedRecordDeletion => ckSyncEngineFetchedRecordDeletion -> IO (Id NSString)
recordType ckSyncEngineFetchedRecordDeletion =
  sendMessage ckSyncEngineFetchedRecordDeletion recordTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineFetchedRecordDeletion)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineFetchedRecordDeletion)
newSelector = mkSelector "new"

-- | @Selector@ for @recordID@
recordIDSelector :: Selector '[] (Id CKRecordID)
recordIDSelector = mkSelector "recordID"

-- | @Selector@ for @recordType@
recordTypeSelector :: Selector '[] (Id NSString)
recordTypeSelector = mkSelector "recordType"

