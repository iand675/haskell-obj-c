{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , atomicSelector
  , clientChangeTokenDataSelector
  , initSelector
  , initWithRecordsToSave_recordIDsToDeleteSelector
  , perRecordCompletionBlockSelector
  , perRecordDeleteBlockSelector
  , perRecordProgressBlockSelector
  , perRecordSaveBlockSelector
  , recordIDsToDeleteSelector
  , recordsToSaveSelector
  , savePolicySelector
  , setAtomicSelector
  , setClientChangeTokenDataSelector
  , setPerRecordCompletionBlockSelector
  , setPerRecordDeleteBlockSelector
  , setPerRecordProgressBlockSelector
  , setPerRecordSaveBlockSelector
  , setRecordIDsToDeleteSelector
  , setRecordsToSaveSelector
  , setSavePolicySelector

  -- * Enum types
  , CKRecordSavePolicy(CKRecordSavePolicy)
  , pattern CKRecordSaveIfServerRecordUnchanged
  , pattern CKRecordSaveChangedKeys
  , pattern CKRecordSaveAllKeys

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
init_ :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id CKModifyRecordsOperation)
init_ ckModifyRecordsOperation =
  sendOwnedMessage ckModifyRecordsOperation initSelector

-- | @- initWithRecordsToSave:recordIDsToDelete:@
initWithRecordsToSave_recordIDsToDelete :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSArray records, IsNSArray recordIDs) => ckModifyRecordsOperation -> records -> recordIDs -> IO (Id CKModifyRecordsOperation)
initWithRecordsToSave_recordIDsToDelete ckModifyRecordsOperation records recordIDs =
  sendOwnedMessage ckModifyRecordsOperation initWithRecordsToSave_recordIDsToDeleteSelector (toNSArray records) (toNSArray recordIDs)

-- | @- recordsToSave@
recordsToSave :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id NSArray)
recordsToSave ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation recordsToSaveSelector

-- | @- setRecordsToSave:@
setRecordsToSave :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSArray value) => ckModifyRecordsOperation -> value -> IO ()
setRecordsToSave ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setRecordsToSaveSelector (toNSArray value)

-- | @- recordIDsToDelete@
recordIDsToDelete :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id NSArray)
recordIDsToDelete ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation recordIDsToDeleteSelector

-- | @- setRecordIDsToDelete:@
setRecordIDsToDelete :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSArray value) => ckModifyRecordsOperation -> value -> IO ()
setRecordIDsToDelete ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setRecordIDsToDeleteSelector (toNSArray value)

-- | Determines what data is sent to the server and whether the save should succeed even if the record on the server has changed.
--
-- :  The default value is @CKRecordSaveIfServerRecordUnchanged,@ which is the recommended value for regular use.  A @CKShare@ record is always treated as @CKRecordSaveIfServerRecordUnchanged,@ regardless of the @savePolicy@ specified.
--
-- ObjC selector: @- savePolicy@
savePolicy :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO CKRecordSavePolicy
savePolicy ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation savePolicySelector

-- | Determines what data is sent to the server and whether the save should succeed even if the record on the server has changed.
--
-- :  The default value is @CKRecordSaveIfServerRecordUnchanged,@ which is the recommended value for regular use.  A @CKShare@ record is always treated as @CKRecordSaveIfServerRecordUnchanged,@ regardless of the @savePolicy@ specified.
--
-- ObjC selector: @- setSavePolicy:@
setSavePolicy :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> CKRecordSavePolicy -> IO ()
setSavePolicy ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setSavePolicySelector value

-- | This property is kept by the server to identify the last known request from this client.  Multiple requests from the client with the same change token will be ignored by the server.
--
-- ObjC selector: @- clientChangeTokenData@
clientChangeTokenData :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Id NSData)
clientChangeTokenData ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation clientChangeTokenDataSelector

-- | This property is kept by the server to identify the last known request from this client.  Multiple requests from the client with the same change token will be ignored by the server.
--
-- ObjC selector: @- setClientChangeTokenData:@
setClientChangeTokenData :: (IsCKModifyRecordsOperation ckModifyRecordsOperation, IsNSData value) => ckModifyRecordsOperation -> value -> IO ()
setClientChangeTokenData ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setClientChangeTokenDataSelector (toNSData value)

-- | Determines whether the batch should fail atomically or not.
--
-- YES by default.  Server-side write atomicity is only enforced on zones that have @CKRecordZoneCapabilityAtomic.@  If @isAtomic@ is YES, client-side checks are enforced regardless of the zone's capabilities.  (For example, if a record is malformed, and cannot be sent to the server, the client will forcibly fail all other records-to-be-modified in that zone)
--
-- ObjC selector: @- atomic@
atomic :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO Bool
atomic ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation atomicSelector

-- | Determines whether the batch should fail atomically or not.
--
-- YES by default.  Server-side write atomicity is only enforced on zones that have @CKRecordZoneCapabilityAtomic.@  If @isAtomic@ is YES, client-side checks are enforced regardless of the zone's capabilities.  (For example, if a record is malformed, and cannot be sent to the server, the client will forcibly fail all other records-to-be-modified in that zone)
--
-- ObjC selector: @- setAtomic:@
setAtomic :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Bool -> IO ()
setAtomic ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setAtomicSelector value

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordProgressBlock@
perRecordProgressBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordProgressBlock ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation perRecordProgressBlockSelector

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordProgressBlock:@
setPerRecordProgressBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordProgressBlock ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setPerRecordProgressBlockSelector value

-- | Called on success or failure for each record.
--
-- Will not be invoked if @perRecordSaveBlock@ is set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordCompletionBlock@
perRecordCompletionBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordCompletionBlock ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation perRecordCompletionBlockSelector

-- | Called on success or failure for each record.
--
-- Will not be invoked if @perRecordSaveBlock@ is set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordCompletionBlock:@
setPerRecordCompletionBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordCompletionBlock ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setPerRecordCompletionBlockSelector value

-- | Called on success or failure of a record save
--
-- Following a successful record save, this callback will be invoked with a nonnull @record,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nil @record,@ and a nonnull @error@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordSaveBlock@
perRecordSaveBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordSaveBlock ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation perRecordSaveBlockSelector

-- | Called on success or failure of a record save
--
-- Following a successful record save, this callback will be invoked with a nonnull @record,@ and a nil @error.@  Following a save failure due to a per-item error (@CKErrorServerRecordChanged,@ for example), this callback will be invoked with a nil @record,@ and a nonnull @error@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordSaveBlock:@
setPerRecordSaveBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordSaveBlock ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setPerRecordSaveBlockSelector value

-- | Called on success or failure of a record deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordDeleteBlock@
perRecordDeleteBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> IO (Ptr ())
perRecordDeleteBlock ckModifyRecordsOperation =
  sendMessage ckModifyRecordsOperation perRecordDeleteBlockSelector

-- | Called on success or failure of a record deletion
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordDeleteBlock:@
setPerRecordDeleteBlock :: IsCKModifyRecordsOperation ckModifyRecordsOperation => ckModifyRecordsOperation -> Ptr () -> IO ()
setPerRecordDeleteBlock ckModifyRecordsOperation value =
  sendMessage ckModifyRecordsOperation setPerRecordDeleteBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKModifyRecordsOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordsToSave:recordIDsToDelete:@
initWithRecordsToSave_recordIDsToDeleteSelector :: Selector '[Id NSArray, Id NSArray] (Id CKModifyRecordsOperation)
initWithRecordsToSave_recordIDsToDeleteSelector = mkSelector "initWithRecordsToSave:recordIDsToDelete:"

-- | @Selector@ for @recordsToSave@
recordsToSaveSelector :: Selector '[] (Id NSArray)
recordsToSaveSelector = mkSelector "recordsToSave"

-- | @Selector@ for @setRecordsToSave:@
setRecordsToSaveSelector :: Selector '[Id NSArray] ()
setRecordsToSaveSelector = mkSelector "setRecordsToSave:"

-- | @Selector@ for @recordIDsToDelete@
recordIDsToDeleteSelector :: Selector '[] (Id NSArray)
recordIDsToDeleteSelector = mkSelector "recordIDsToDelete"

-- | @Selector@ for @setRecordIDsToDelete:@
setRecordIDsToDeleteSelector :: Selector '[Id NSArray] ()
setRecordIDsToDeleteSelector = mkSelector "setRecordIDsToDelete:"

-- | @Selector@ for @savePolicy@
savePolicySelector :: Selector '[] CKRecordSavePolicy
savePolicySelector = mkSelector "savePolicy"

-- | @Selector@ for @setSavePolicy:@
setSavePolicySelector :: Selector '[CKRecordSavePolicy] ()
setSavePolicySelector = mkSelector "setSavePolicy:"

-- | @Selector@ for @clientChangeTokenData@
clientChangeTokenDataSelector :: Selector '[] (Id NSData)
clientChangeTokenDataSelector = mkSelector "clientChangeTokenData"

-- | @Selector@ for @setClientChangeTokenData:@
setClientChangeTokenDataSelector :: Selector '[Id NSData] ()
setClientChangeTokenDataSelector = mkSelector "setClientChangeTokenData:"

-- | @Selector@ for @atomic@
atomicSelector :: Selector '[] Bool
atomicSelector = mkSelector "atomic"

-- | @Selector@ for @setAtomic:@
setAtomicSelector :: Selector '[Bool] ()
setAtomicSelector = mkSelector "setAtomic:"

-- | @Selector@ for @perRecordProgressBlock@
perRecordProgressBlockSelector :: Selector '[] (Ptr ())
perRecordProgressBlockSelector = mkSelector "perRecordProgressBlock"

-- | @Selector@ for @setPerRecordProgressBlock:@
setPerRecordProgressBlockSelector :: Selector '[Ptr ()] ()
setPerRecordProgressBlockSelector = mkSelector "setPerRecordProgressBlock:"

-- | @Selector@ for @perRecordCompletionBlock@
perRecordCompletionBlockSelector :: Selector '[] (Ptr ())
perRecordCompletionBlockSelector = mkSelector "perRecordCompletionBlock"

-- | @Selector@ for @setPerRecordCompletionBlock:@
setPerRecordCompletionBlockSelector :: Selector '[Ptr ()] ()
setPerRecordCompletionBlockSelector = mkSelector "setPerRecordCompletionBlock:"

-- | @Selector@ for @perRecordSaveBlock@
perRecordSaveBlockSelector :: Selector '[] (Ptr ())
perRecordSaveBlockSelector = mkSelector "perRecordSaveBlock"

-- | @Selector@ for @setPerRecordSaveBlock:@
setPerRecordSaveBlockSelector :: Selector '[Ptr ()] ()
setPerRecordSaveBlockSelector = mkSelector "setPerRecordSaveBlock:"

-- | @Selector@ for @perRecordDeleteBlock@
perRecordDeleteBlockSelector :: Selector '[] (Ptr ())
perRecordDeleteBlockSelector = mkSelector "perRecordDeleteBlock"

-- | @Selector@ for @setPerRecordDeleteBlock:@
setPerRecordDeleteBlockSelector :: Selector '[Ptr ()] ()
setPerRecordDeleteBlockSelector = mkSelector "setPerRecordDeleteBlock:"

