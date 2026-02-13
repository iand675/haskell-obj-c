{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKModifyRecordZonesOperation@.
module ObjC.CloudKit.CKModifyRecordZonesOperation
  ( CKModifyRecordZonesOperation
  , IsCKModifyRecordZonesOperation(..)
  , init_
  , initWithRecordZonesToSave_recordZoneIDsToDelete
  , recordZonesToSave
  , setRecordZonesToSave
  , recordZoneIDsToDelete
  , setRecordZoneIDsToDelete
  , perRecordZoneSaveBlock
  , setPerRecordZoneSaveBlock
  , perRecordZoneDeleteBlock
  , setPerRecordZoneDeleteBlock
  , initSelector
  , initWithRecordZonesToSave_recordZoneIDsToDeleteSelector
  , perRecordZoneDeleteBlockSelector
  , perRecordZoneSaveBlockSelector
  , recordZoneIDsToDeleteSelector
  , recordZonesToSaveSelector
  , setPerRecordZoneDeleteBlockSelector
  , setPerRecordZoneSaveBlockSelector
  , setRecordZoneIDsToDeleteSelector
  , setRecordZonesToSaveSelector


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
init_ :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Id CKModifyRecordZonesOperation)
init_ ckModifyRecordZonesOperation =
  sendOwnedMessage ckModifyRecordZonesOperation initSelector

-- | @- initWithRecordZonesToSave:recordZoneIDsToDelete:@
initWithRecordZonesToSave_recordZoneIDsToDelete :: (IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation, IsNSArray recordZonesToSave, IsNSArray recordZoneIDsToDelete) => ckModifyRecordZonesOperation -> recordZonesToSave -> recordZoneIDsToDelete -> IO (Id CKModifyRecordZonesOperation)
initWithRecordZonesToSave_recordZoneIDsToDelete ckModifyRecordZonesOperation recordZonesToSave recordZoneIDsToDelete =
  sendOwnedMessage ckModifyRecordZonesOperation initWithRecordZonesToSave_recordZoneIDsToDeleteSelector (toNSArray recordZonesToSave) (toNSArray recordZoneIDsToDelete)

-- | @- recordZonesToSave@
recordZonesToSave :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Id NSArray)
recordZonesToSave ckModifyRecordZonesOperation =
  sendMessage ckModifyRecordZonesOperation recordZonesToSaveSelector

-- | @- setRecordZonesToSave:@
setRecordZonesToSave :: (IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation, IsNSArray value) => ckModifyRecordZonesOperation -> value -> IO ()
setRecordZonesToSave ckModifyRecordZonesOperation value =
  sendMessage ckModifyRecordZonesOperation setRecordZonesToSaveSelector (toNSArray value)

-- | @- recordZoneIDsToDelete@
recordZoneIDsToDelete :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Id NSArray)
recordZoneIDsToDelete ckModifyRecordZonesOperation =
  sendMessage ckModifyRecordZonesOperation recordZoneIDsToDeleteSelector

-- | @- setRecordZoneIDsToDelete:@
setRecordZoneIDsToDelete :: (IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation, IsNSArray value) => ckModifyRecordZonesOperation -> value -> IO ()
setRecordZoneIDsToDelete ckModifyRecordZonesOperation value =
  sendMessage ckModifyRecordZonesOperation setRecordZoneIDsToDeleteSelector (toNSArray value)

-- | Called on success or failure of a record zone save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordZoneSaveBlock@
perRecordZoneSaveBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Ptr ())
perRecordZoneSaveBlock ckModifyRecordZonesOperation =
  sendMessage ckModifyRecordZonesOperation perRecordZoneSaveBlockSelector

-- | Called on success or failure of a record zone save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordZoneSaveBlock:@
setPerRecordZoneSaveBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> Ptr () -> IO ()
setPerRecordZoneSaveBlock ckModifyRecordZonesOperation value =
  sendMessage ckModifyRecordZonesOperation setPerRecordZoneSaveBlockSelector value

-- | Called on success or failure of a record zone deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordZoneDeleteBlock@
perRecordZoneDeleteBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Ptr ())
perRecordZoneDeleteBlock ckModifyRecordZonesOperation =
  sendMessage ckModifyRecordZonesOperation perRecordZoneDeleteBlockSelector

-- | Called on success or failure of a record zone deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordZoneDeleteBlock:@
setPerRecordZoneDeleteBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> Ptr () -> IO ()
setPerRecordZoneDeleteBlock ckModifyRecordZonesOperation value =
  sendMessage ckModifyRecordZonesOperation setPerRecordZoneDeleteBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKModifyRecordZonesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZonesToSave:recordZoneIDsToDelete:@
initWithRecordZonesToSave_recordZoneIDsToDeleteSelector :: Selector '[Id NSArray, Id NSArray] (Id CKModifyRecordZonesOperation)
initWithRecordZonesToSave_recordZoneIDsToDeleteSelector = mkSelector "initWithRecordZonesToSave:recordZoneIDsToDelete:"

-- | @Selector@ for @recordZonesToSave@
recordZonesToSaveSelector :: Selector '[] (Id NSArray)
recordZonesToSaveSelector = mkSelector "recordZonesToSave"

-- | @Selector@ for @setRecordZonesToSave:@
setRecordZonesToSaveSelector :: Selector '[Id NSArray] ()
setRecordZonesToSaveSelector = mkSelector "setRecordZonesToSave:"

-- | @Selector@ for @recordZoneIDsToDelete@
recordZoneIDsToDeleteSelector :: Selector '[] (Id NSArray)
recordZoneIDsToDeleteSelector = mkSelector "recordZoneIDsToDelete"

-- | @Selector@ for @setRecordZoneIDsToDelete:@
setRecordZoneIDsToDeleteSelector :: Selector '[Id NSArray] ()
setRecordZoneIDsToDeleteSelector = mkSelector "setRecordZoneIDsToDelete:"

-- | @Selector@ for @perRecordZoneSaveBlock@
perRecordZoneSaveBlockSelector :: Selector '[] (Ptr ())
perRecordZoneSaveBlockSelector = mkSelector "perRecordZoneSaveBlock"

-- | @Selector@ for @setPerRecordZoneSaveBlock:@
setPerRecordZoneSaveBlockSelector :: Selector '[Ptr ()] ()
setPerRecordZoneSaveBlockSelector = mkSelector "setPerRecordZoneSaveBlock:"

-- | @Selector@ for @perRecordZoneDeleteBlock@
perRecordZoneDeleteBlockSelector :: Selector '[] (Ptr ())
perRecordZoneDeleteBlockSelector = mkSelector "perRecordZoneDeleteBlock"

-- | @Selector@ for @setPerRecordZoneDeleteBlock:@
setPerRecordZoneDeleteBlockSelector :: Selector '[Ptr ()] ()
setPerRecordZoneDeleteBlockSelector = mkSelector "setPerRecordZoneDeleteBlock:"

