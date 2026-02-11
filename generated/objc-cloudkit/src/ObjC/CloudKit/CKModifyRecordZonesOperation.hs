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
  , recordZonesToSaveSelector
  , setRecordZonesToSaveSelector
  , recordZoneIDsToDeleteSelector
  , setRecordZoneIDsToDeleteSelector
  , perRecordZoneSaveBlockSelector
  , setPerRecordZoneSaveBlockSelector
  , perRecordZoneDeleteBlockSelector
  , setPerRecordZoneDeleteBlockSelector


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
init_ :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Id CKModifyRecordZonesOperation)
init_ ckModifyRecordZonesOperation  =
  sendMsg ckModifyRecordZonesOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRecordZonesToSave:recordZoneIDsToDelete:@
initWithRecordZonesToSave_recordZoneIDsToDelete :: (IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation, IsNSArray recordZonesToSave, IsNSArray recordZoneIDsToDelete) => ckModifyRecordZonesOperation -> recordZonesToSave -> recordZoneIDsToDelete -> IO (Id CKModifyRecordZonesOperation)
initWithRecordZonesToSave_recordZoneIDsToDelete ckModifyRecordZonesOperation  recordZonesToSave recordZoneIDsToDelete =
withObjCPtr recordZonesToSave $ \raw_recordZonesToSave ->
  withObjCPtr recordZoneIDsToDelete $ \raw_recordZoneIDsToDelete ->
      sendMsg ckModifyRecordZonesOperation (mkSelector "initWithRecordZonesToSave:recordZoneIDsToDelete:") (retPtr retVoid) [argPtr (castPtr raw_recordZonesToSave :: Ptr ()), argPtr (castPtr raw_recordZoneIDsToDelete :: Ptr ())] >>= ownedObject . castPtr

-- | @- recordZonesToSave@
recordZonesToSave :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Id NSArray)
recordZonesToSave ckModifyRecordZonesOperation  =
  sendMsg ckModifyRecordZonesOperation (mkSelector "recordZonesToSave") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordZonesToSave:@
setRecordZonesToSave :: (IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation, IsNSArray value) => ckModifyRecordZonesOperation -> value -> IO ()
setRecordZonesToSave ckModifyRecordZonesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckModifyRecordZonesOperation (mkSelector "setRecordZonesToSave:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recordZoneIDsToDelete@
recordZoneIDsToDelete :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Id NSArray)
recordZoneIDsToDelete ckModifyRecordZonesOperation  =
  sendMsg ckModifyRecordZonesOperation (mkSelector "recordZoneIDsToDelete") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordZoneIDsToDelete:@
setRecordZoneIDsToDelete :: (IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation, IsNSArray value) => ckModifyRecordZonesOperation -> value -> IO ()
setRecordZoneIDsToDelete ckModifyRecordZonesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckModifyRecordZonesOperation (mkSelector "setRecordZoneIDsToDelete:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called on success or failure of a record zone save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordZoneSaveBlock@
perRecordZoneSaveBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Ptr ())
perRecordZoneSaveBlock ckModifyRecordZonesOperation  =
  fmap castPtr $ sendMsg ckModifyRecordZonesOperation (mkSelector "perRecordZoneSaveBlock") (retPtr retVoid) []

-- | Called on success or failure of a record zone save
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordZoneSaveBlock:@
setPerRecordZoneSaveBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> Ptr () -> IO ()
setPerRecordZoneSaveBlock ckModifyRecordZonesOperation  value =
  sendMsg ckModifyRecordZonesOperation (mkSelector "setPerRecordZoneSaveBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called on success or failure of a record zone deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordZoneDeleteBlock@
perRecordZoneDeleteBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> IO (Ptr ())
perRecordZoneDeleteBlock ckModifyRecordZonesOperation  =
  fmap castPtr $ sendMsg ckModifyRecordZonesOperation (mkSelector "perRecordZoneDeleteBlock") (retPtr retVoid) []

-- | Called on success or failure of a record zone deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordZoneDeleteBlock:@
setPerRecordZoneDeleteBlock :: IsCKModifyRecordZonesOperation ckModifyRecordZonesOperation => ckModifyRecordZonesOperation -> Ptr () -> IO ()
setPerRecordZoneDeleteBlock ckModifyRecordZonesOperation  value =
  sendMsg ckModifyRecordZonesOperation (mkSelector "setPerRecordZoneDeleteBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZonesToSave:recordZoneIDsToDelete:@
initWithRecordZonesToSave_recordZoneIDsToDeleteSelector :: Selector
initWithRecordZonesToSave_recordZoneIDsToDeleteSelector = mkSelector "initWithRecordZonesToSave:recordZoneIDsToDelete:"

-- | @Selector@ for @recordZonesToSave@
recordZonesToSaveSelector :: Selector
recordZonesToSaveSelector = mkSelector "recordZonesToSave"

-- | @Selector@ for @setRecordZonesToSave:@
setRecordZonesToSaveSelector :: Selector
setRecordZonesToSaveSelector = mkSelector "setRecordZonesToSave:"

-- | @Selector@ for @recordZoneIDsToDelete@
recordZoneIDsToDeleteSelector :: Selector
recordZoneIDsToDeleteSelector = mkSelector "recordZoneIDsToDelete"

-- | @Selector@ for @setRecordZoneIDsToDelete:@
setRecordZoneIDsToDeleteSelector :: Selector
setRecordZoneIDsToDeleteSelector = mkSelector "setRecordZoneIDsToDelete:"

-- | @Selector@ for @perRecordZoneSaveBlock@
perRecordZoneSaveBlockSelector :: Selector
perRecordZoneSaveBlockSelector = mkSelector "perRecordZoneSaveBlock"

-- | @Selector@ for @setPerRecordZoneSaveBlock:@
setPerRecordZoneSaveBlockSelector :: Selector
setPerRecordZoneSaveBlockSelector = mkSelector "setPerRecordZoneSaveBlock:"

-- | @Selector@ for @perRecordZoneDeleteBlock@
perRecordZoneDeleteBlockSelector :: Selector
perRecordZoneDeleteBlockSelector = mkSelector "perRecordZoneDeleteBlock"

-- | @Selector@ for @setPerRecordZoneDeleteBlock:@
setPerRecordZoneDeleteBlockSelector :: Selector
setPerRecordZoneDeleteBlockSelector = mkSelector "setPerRecordZoneDeleteBlock:"

