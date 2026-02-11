{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKModifyRecordsOperation@.
module ObjC.CloudKit.CKModifyRecordsOperation
  ( CKModifyRecordsOperation
  , IsCKModifyRecordsOperation(..)
  , init_
  , initWithRecordsToSave_recordIDsToDelete
  , recordsToSave
  , setRecordsToSave
  , recordIDsToDelete
  , setRecordIDsToDelete
  , savePolicy
  , setSavePolicy
  , clientChangeTokenData
  , setClientChangeTokenData
  , atomic
  , setAtomic
  , perRecordProgressBlock
  , setPerRecordProgressBlock
  , perRecordCompletionBlock
  , setPerRecordCompletionBlock
  , perRecordSaveBlock
  , setPerRecordSaveBlock
  , perRecordDeleteBlock
  , setPerRecordDeleteBlock
  , initSelector
  , initWithRecordsToSave_recordIDsToDeleteSelector
  , recordsToSaveSelector
  , setRecordsToSaveSelector
  , recordIDsToDeleteSelector
  , setRecordIDsToDeleteSelector
  , savePolicySelector
  , setSavePolicySelector
  , clientChangeTokenDataSelector
  , setClientChangeTokenDataSelector
  , atomicSelector
  , setAtomicSelector
  , perRecordProgressBlockSelector
  , setPerRecordProgressBlockSelector
  , perRecordCompletionBlockSelector
  , setPerRecordCompletionBlockSelector
  , perRecordSaveBlockSelector
  , setPerRecordSaveBlockSelector
  , perRecordDeleteBlockSelector
  , setPerRecordDeleteBlockSelector

  -- * Enum types
  , CKRecordSavePolicy(CKRecordSavePolicy)
  , pattern CKRecordSaveIfServerRecordUnchanged
  , pattern CKRecordSaveChangedKeys
  , pattern CKRecordSaveAllKeys

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
init_ :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id CKModifyRecordsOperation)
init_ ckModifyRecordsOperation  =
  sendMsg ckModifyRecordsOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRecordsToSave:recordIDsToDelete:@
initWithRecordsToSave_recordIDsToDelete :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSArray records, IsNSArray recordIDs) => ckModifyRecordsOperation -> records -> recordIDs -> IO (Id CKModifyRecordsOperation)
initWithRecordsToSave_recordIDsToDelete ckModifyRecordsOperation  records recordIDs =
withObjCPtr records $ \raw_records ->
  withObjCPtr recordIDs $ \raw_recordIDs ->
      sendMsg ckModifyRecordsOperation (mkSelector "initWithRecordsToSave:recordIDsToDelete:") (retPtr retVoid) [argPtr (castPtr raw_records :: Ptr ()), argPtr (castPtr raw_recordIDs :: Ptr ())] >>= ownedObject . castPtr

-- | @- recordsToSave@
recordsToSave :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id NSArray)
recordsToSave ckModifyRecordsOperation  =
  sendMsg ckModifyRecordsOperation (mkSelector "recordsToSave") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordsToSave:@
setRecordsToSave :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSArray value) => ckModifyRecordsOperation -> value -> IO ()
setRecordsToSave ckModifyRecordsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckModifyRecordsOperation (mkSelector "setRecordsToSave:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recordIDsToDelete@
recordIDsToDelete :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id NSArray)
recordIDsToDelete ckModifyRecordsOperation  =
  sendMsg ckModifyRecordsOperation (mkSelector "recordIDsToDelete") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordIDsToDelete:@
setRecordIDsToDelete :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSArray value) => ckModifyRecordsOperation -> value -> IO ()
setRecordIDsToDelete ckModifyRecordsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckModifyRecordsOperation (mkSelector "setRecordIDsToDelete:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Determines what data is sent to the server and whether the save should succeed even if the record on the server has changed.
--
-- :  The default value is @CKRecordSaveIfServerRecordUnchanged,@ which is the recommended value for regular use.  A @CKShare@ record is always treated as @CKRecordSaveIfServerRecordUnchanged,@ regardless of the @savePolicy@ specified.
--
-- ObjC selector: @- savePolicy@
savePolicy :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO CKRecordSavePolicy
savePolicy ckModifyRecordsOperation  =
  fmap (coerce :: CLong -> CKRecordSavePolicy) $ sendMsg ckModifyRecordsOperation (mkSelector "savePolicy") retCLong []

-- | Determines what data is sent to the server and whether the save should succeed even if the record on the server has changed.
--
-- :  The default value is @CKRecordSaveIfServerRecordUnchanged,@ which is the recommended value for regular use.  A @CKShare@ record is always treated as @CKRecordSaveIfServerRecordUnchanged,@ regardless of the @savePolicy@ specified.
--
-- ObjC selector: @- setSavePolicy:@
setSavePolicy :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> CKRecordSavePolicy -> IO ()
setSavePolicy ckModifyRecordsOperation  value =
  sendMsg ckModifyRecordsOperation (mkSelector "setSavePolicy:") retVoid [argCLong (coerce value)]

-- | This property is kept by the server to identify the last known request from this client.  Multiple requests from the client with the same change token will be ignored by the server.
--
-- ObjC selector: @- clientChangeTokenData@
clientChangeTokenData :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id NSData)
clientChangeTokenData ckModifyRecordsOperation  =
  sendMsg ckModifyRecordsOperation (mkSelector "clientChangeTokenData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property is kept by the server to identify the last known request from this client.  Multiple requests from the client with the same change token will be ignored by the server.
--
-- ObjC selector: @- setClientChangeTokenData:@
setClientChangeTokenData :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSData value) => ckModifyRecordsOperation -> value -> IO ()
setClientChangeTokenData ckModifyRecordsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckModifyRecordsOperation (mkSelector "setClientChangeTokenData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Determines whether the batch should fail atomically or not.
--
-- YES by default.  Server-side write atomicity is only enforced on zones that have @CKRecordZoneCapabilityAtomic.@  If @isAtomic@ is YES, client-side checks are enforced regardless of the zone's capabilities.  (For example, if a record is malformed, and cannot be sent to the server, the client will forcibly fail all other records-to-be-modified in that zone)
--
-- ObjC selector: @- atomic@
atomic :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO Bool
atomic ckModifyRecordsOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckModifyRecordsOperation (mkSelector "atomic") retCULong []

-- | Determines whether the batch should fail atomically or not.
--
-- YES by default.  Server-side write atomicity is only enforced on zones that have @CKRecordZoneCapabilityAtomic.@  If @isAtomic@ is YES, client-side checks are enforced regardless of the zone's capabilities.  (For example, if a record is malformed, and cannot be sent to the server, the client will forcibly fail all other records-to-be-modified in that zone)
--
-- ObjC selector: @- setAtomic:@
setAtomic :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Bool -> IO ()
setAtomic ckModifyRecordsOperation  value =
  sendMsg ckModifyRecordsOperation (mkSelector "setAtomic:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordProgressBlock@
perRecordProgressBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordProgressBlock ckModifyRecordsOperation  =
  fmap castPtr $ sendMsg ckModifyRecordsOperation (mkSelector "perRecordProgressBlock") (retPtr retVoid) []

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordProgressBlock:@
setPerRecordProgressBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordProgressBlock ckModifyRecordsOperation  value =
  sendMsg ckModifyRecordsOperation (mkSelector "setPerRecordProgressBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called on success or failure for each record.
--
-- Will not be invoked if @perRecordSaveBlock@ is set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordCompletionBlock@
perRecordCompletionBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordCompletionBlock ckModifyRecordsOperation  =
  fmap castPtr $ sendMsg ckModifyRecordsOperation (mkSelector "perRecordCompletionBlock") (retPtr retVoid) []

-- | Called on success or failure for each record.
--
-- Will not be invoked if @perRecordSaveBlock@ is set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordCompletionBlock:@
setPerRecordCompletionBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordCompletionBlock ckModifyRecordsOperation  value =
  sendMsg ckModifyRecordsOperation (mkSelector "setPerRecordCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called on success or failure of a record save
--
-- Following a successful record save, this callback will be invoked with a nonnull @record,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nil @record,@ and a nonnull @error@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordSaveBlock@
perRecordSaveBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordSaveBlock ckModifyRecordsOperation  =
  fmap castPtr $ sendMsg ckModifyRecordsOperation (mkSelector "perRecordSaveBlock") (retPtr retVoid) []

-- | Called on success or failure of a record save
--
-- Following a successful record save, this callback will be invoked with a nonnull @record,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nil @record,@ and a nonnull @error@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordSaveBlock:@
setPerRecordSaveBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordSaveBlock ckModifyRecordsOperation  value =
  sendMsg ckModifyRecordsOperation (mkSelector "setPerRecordSaveBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called on success or failure of a record deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordDeleteBlock@
perRecordDeleteBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordDeleteBlock ckModifyRecordsOperation  =
  fmap castPtr $ sendMsg ckModifyRecordsOperation (mkSelector "perRecordDeleteBlock") (retPtr retVoid) []

-- | Called on success or failure of a record deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordDeleteBlock:@
setPerRecordDeleteBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordDeleteBlock ckModifyRecordsOperation  value =
  sendMsg ckModifyRecordsOperation (mkSelector "setPerRecordDeleteBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordsToSave:recordIDsToDelete:@
initWithRecordsToSave_recordIDsToDeleteSelector :: Selector
initWithRecordsToSave_recordIDsToDeleteSelector = mkSelector "initWithRecordsToSave:recordIDsToDelete:"

-- | @Selector@ for @recordsToSave@
recordsToSaveSelector :: Selector
recordsToSaveSelector = mkSelector "recordsToSave"

-- | @Selector@ for @setRecordsToSave:@
setRecordsToSaveSelector :: Selector
setRecordsToSaveSelector = mkSelector "setRecordsToSave:"

-- | @Selector@ for @recordIDsToDelete@
recordIDsToDeleteSelector :: Selector
recordIDsToDeleteSelector = mkSelector "recordIDsToDelete"

-- | @Selector@ for @setRecordIDsToDelete:@
setRecordIDsToDeleteSelector :: Selector
setRecordIDsToDeleteSelector = mkSelector "setRecordIDsToDelete:"

-- | @Selector@ for @savePolicy@
savePolicySelector :: Selector
savePolicySelector = mkSelector "savePolicy"

-- | @Selector@ for @setSavePolicy:@
setSavePolicySelector :: Selector
setSavePolicySelector = mkSelector "setSavePolicy:"

-- | @Selector@ for @clientChangeTokenData@
clientChangeTokenDataSelector :: Selector
clientChangeTokenDataSelector = mkSelector "clientChangeTokenData"

-- | @Selector@ for @setClientChangeTokenData:@
setClientChangeTokenDataSelector :: Selector
setClientChangeTokenDataSelector = mkSelector "setClientChangeTokenData:"

-- | @Selector@ for @atomic@
atomicSelector :: Selector
atomicSelector = mkSelector "atomic"

-- | @Selector@ for @setAtomic:@
setAtomicSelector :: Selector
setAtomicSelector = mkSelector "setAtomic:"

-- | @Selector@ for @perRecordProgressBlock@
perRecordProgressBlockSelector :: Selector
perRecordProgressBlockSelector = mkSelector "perRecordProgressBlock"

-- | @Selector@ for @setPerRecordProgressBlock:@
setPerRecordProgressBlockSelector :: Selector
setPerRecordProgressBlockSelector = mkSelector "setPerRecordProgressBlock:"

-- | @Selector@ for @perRecordCompletionBlock@
perRecordCompletionBlockSelector :: Selector
perRecordCompletionBlockSelector = mkSelector "perRecordCompletionBlock"

-- | @Selector@ for @setPerRecordCompletionBlock:@
setPerRecordCompletionBlockSelector :: Selector
setPerRecordCompletionBlockSelector = mkSelector "setPerRecordCompletionBlock:"

-- | @Selector@ for @perRecordSaveBlock@
perRecordSaveBlockSelector :: Selector
perRecordSaveBlockSelector = mkSelector "perRecordSaveBlock"

-- | @Selector@ for @setPerRecordSaveBlock:@
setPerRecordSaveBlockSelector :: Selector
setPerRecordSaveBlockSelector = mkSelector "setPerRecordSaveBlock:"

-- | @Selector@ for @perRecordDeleteBlock@
perRecordDeleteBlockSelector :: Selector
perRecordDeleteBlockSelector = mkSelector "perRecordDeleteBlock"

-- | @Selector@ for @setPerRecordDeleteBlock:@
setPerRecordDeleteBlockSelector :: Selector
setPerRecordDeleteBlockSelector = mkSelector "setPerRecordDeleteBlock:"

