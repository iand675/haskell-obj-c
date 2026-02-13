{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKFetchDatabaseChangesOperation
--
-- This operation will fetch changes to record zones within a database
--
-- If a change anchor from a previous @CKFetchDatabaseChangesOperation@ is passed in, only the zones that have changed since that anchor will be returned.  This per-database @serverChangeToken@ is not to be confused with the per-recordZone @serverChangeToken@ from @CKFetchRecordZoneChangesOperation.@  If this is your first fetch or if you wish to re-fetch all zones, pass nil for the change token.  Change token are opaque tokens and clients should not infer any behavior based on their content.  @CKFetchDatabaseChangesOperation@ is supported in a @privateCloudDatabase@ and @sharedCloudDatabase@
--
-- Generated bindings for @CKFetchDatabaseChangesOperation@.
module ObjC.CloudKit.CKFetchDatabaseChangesOperation
  ( CKFetchDatabaseChangesOperation
  , IsCKFetchDatabaseChangesOperation(..)
  , init_
  , initWithPreviousServerChangeToken
  , previousServerChangeToken
  , setPreviousServerChangeToken
  , resultsLimit
  , setResultsLimit
  , fetchAllChanges
  , setFetchAllChanges
  , recordZoneWithIDChangedBlock
  , setRecordZoneWithIDChangedBlock
  , recordZoneWithIDWasDeletedBlock
  , setRecordZoneWithIDWasDeletedBlock
  , recordZoneWithIDWasPurgedBlock
  , setRecordZoneWithIDWasPurgedBlock
  , recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock
  , setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock
  , changeTokenUpdatedBlock
  , setChangeTokenUpdatedBlock
  , fetchDatabaseChangesCompletionBlock
  , setFetchDatabaseChangesCompletionBlock
  , changeTokenUpdatedBlockSelector
  , fetchAllChangesSelector
  , fetchDatabaseChangesCompletionBlockSelector
  , initSelector
  , initWithPreviousServerChangeTokenSelector
  , previousServerChangeTokenSelector
  , recordZoneWithIDChangedBlockSelector
  , recordZoneWithIDWasDeletedBlockSelector
  , recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector
  , recordZoneWithIDWasPurgedBlockSelector
  , resultsLimitSelector
  , setChangeTokenUpdatedBlockSelector
  , setFetchAllChangesSelector
  , setFetchDatabaseChangesCompletionBlockSelector
  , setPreviousServerChangeTokenSelector
  , setRecordZoneWithIDChangedBlockSelector
  , setRecordZoneWithIDWasDeletedBlockSelector
  , setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector
  , setRecordZoneWithIDWasPurgedBlockSelector
  , setResultsLimitSelector


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
init_ :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Id CKFetchDatabaseChangesOperation)
init_ ckFetchDatabaseChangesOperation =
  sendOwnedMessage ckFetchDatabaseChangesOperation initSelector

-- | @- initWithPreviousServerChangeToken:@
initWithPreviousServerChangeToken :: (IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation, IsCKServerChangeToken previousServerChangeToken) => ckFetchDatabaseChangesOperation -> previousServerChangeToken -> IO (Id CKFetchDatabaseChangesOperation)
initWithPreviousServerChangeToken ckFetchDatabaseChangesOperation previousServerChangeToken =
  sendOwnedMessage ckFetchDatabaseChangesOperation initWithPreviousServerChangeTokenSelector (toCKServerChangeToken previousServerChangeToken)

-- | @- previousServerChangeToken@
previousServerChangeToken :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Id CKServerChangeToken)
previousServerChangeToken ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation previousServerChangeTokenSelector

-- | @- setPreviousServerChangeToken:@
setPreviousServerChangeToken :: (IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation, IsCKServerChangeToken value) => ckFetchDatabaseChangesOperation -> value -> IO ()
setPreviousServerChangeToken ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setPreviousServerChangeTokenSelector (toCKServerChangeToken value)

-- | @- resultsLimit@
resultsLimit :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO CULong
resultsLimit ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation resultsLimitSelector

-- | @- setResultsLimit:@
setResultsLimit :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> CULong -> IO ()
setResultsLimit ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setResultsLimitSelector value

-- | When set to YES, this operation will send repeated requests to the server until all record zone changes have been fetched.
--
-- @changeTokenUpdatedBlock@ will be invoked periodically, to give clients an updated change token so that already-fetched record zone changes don't need to be re-fetched on a subsequent operation.  When set to NO, it is the responsibility of the caller to issue subsequent fetch-changes operations when moreComing is YES in a @fetchDatabaseChangesCompletionBlock@ invocation.  @fetchAllChanges@ is @YES@ by default  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  Blocks assigned to this operation may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchAllChanges@
fetchAllChanges :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO Bool
fetchAllChanges ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation fetchAllChangesSelector

-- | When set to YES, this operation will send repeated requests to the server until all record zone changes have been fetched.
--
-- @changeTokenUpdatedBlock@ will be invoked periodically, to give clients an updated change token so that already-fetched record zone changes don't need to be re-fetched on a subsequent operation.  When set to NO, it is the responsibility of the caller to issue subsequent fetch-changes operations when moreComing is YES in a @fetchDatabaseChangesCompletionBlock@ invocation.  @fetchAllChanges@ is @YES@ by default  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  Blocks assigned to this operation may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchAllChanges:@
setFetchAllChanges :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> Bool -> IO ()
setFetchAllChanges ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setFetchAllChangesSelector value

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordZoneWithIDChangedBlock@
recordZoneWithIDChangedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Ptr ())
recordZoneWithIDChangedBlock ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation recordZoneWithIDChangedBlockSelector

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordZoneWithIDChangedBlock:@
setRecordZoneWithIDChangedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> Ptr () -> IO ()
setRecordZoneWithIDChangedBlock ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setRecordZoneWithIDChangedBlockSelector value

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordZoneWithIDWasDeletedBlock@
recordZoneWithIDWasDeletedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Ptr ())
recordZoneWithIDWasDeletedBlock ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation recordZoneWithIDWasDeletedBlockSelector

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordZoneWithIDWasDeletedBlock:@
setRecordZoneWithIDWasDeletedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> Ptr () -> IO ()
setRecordZoneWithIDWasDeletedBlock ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setRecordZoneWithIDWasDeletedBlockSelector value

-- | If this block is set it will be called instead of @recordZoneWithIDWasDeletedBlock@ if the user deleted this zone via the iCloud storage UI.
--
-- This is an indication that the user wanted all data deleted, so local cached data should be wiped and not re-uploaded to the server.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordZoneWithIDWasPurgedBlock@
recordZoneWithIDWasPurgedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Ptr ())
recordZoneWithIDWasPurgedBlock ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation recordZoneWithIDWasPurgedBlockSelector

-- | If this block is set it will be called instead of @recordZoneWithIDWasDeletedBlock@ if the user deleted this zone via the iCloud storage UI.
--
-- This is an indication that the user wanted all data deleted, so local cached data should be wiped and not re-uploaded to the server.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordZoneWithIDWasPurgedBlock:@
setRecordZoneWithIDWasPurgedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> Ptr () -> IO ()
setRecordZoneWithIDWasPurgedBlock ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setRecordZoneWithIDWasPurgedBlockSelector value

-- | If this block is set it will be called instead of @recordZoneWithIDWasDeletedBlock@ if the user chose to reset all encrypted data for their account.
--
-- This is an indication that the user had to reset encrypted data during account recovery, so local cached data should be re-uploaded to the server to minimize data loss.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock@
recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Ptr ())
recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector

-- | If this block is set it will be called instead of @recordZoneWithIDWasDeletedBlock@ if the user chose to reset all encrypted data for their account.
--
-- This is an indication that the user had to reset encrypted data during account recovery, so local cached data should be re-uploaded to the server to minimize data loss.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock:@
setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> Ptr () -> IO ()
setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector value

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- changeTokenUpdatedBlock@
changeTokenUpdatedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Ptr ())
changeTokenUpdatedBlock ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation changeTokenUpdatedBlockSelector

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setChangeTokenUpdatedBlock:@
setChangeTokenUpdatedBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> Ptr () -> IO ()
setChangeTokenUpdatedBlock ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setChangeTokenUpdatedBlockSelector value

-- | This block is called when the operation completes.
--
-- Clients are responsible for saving the change token at the end of the operation and passing it in to the next call to @CKFetchDatabaseChangesOperation.@  If the server returns a @CKErrorChangeTokenExpired@ error, the @previousServerChangeToken@ value was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @previousServerChangeToken.@  If @moreComing@ is true then the server wasn't able to return all the changes in this response. Another @CKFetchDatabaseChangesOperation@ operation should be run with the @previousServerChangeToken@ token from this operation.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchDatabaseChangesCompletionBlock@
fetchDatabaseChangesCompletionBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> IO (Ptr ())
fetchDatabaseChangesCompletionBlock ckFetchDatabaseChangesOperation =
  sendMessage ckFetchDatabaseChangesOperation fetchDatabaseChangesCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- Clients are responsible for saving the change token at the end of the operation and passing it in to the next call to @CKFetchDatabaseChangesOperation.@  If the server returns a @CKErrorChangeTokenExpired@ error, the @previousServerChangeToken@ value was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @previousServerChangeToken.@  If @moreComing@ is true then the server wasn't able to return all the changes in this response. Another @CKFetchDatabaseChangesOperation@ operation should be run with the @previousServerChangeToken@ token from this operation.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchDatabaseChangesCompletionBlock:@
setFetchDatabaseChangesCompletionBlock :: IsCKFetchDatabaseChangesOperation ckFetchDatabaseChangesOperation => ckFetchDatabaseChangesOperation -> Ptr () -> IO ()
setFetchDatabaseChangesCompletionBlock ckFetchDatabaseChangesOperation value =
  sendMessage ckFetchDatabaseChangesOperation setFetchDatabaseChangesCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchDatabaseChangesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithPreviousServerChangeToken:@
initWithPreviousServerChangeTokenSelector :: Selector '[Id CKServerChangeToken] (Id CKFetchDatabaseChangesOperation)
initWithPreviousServerChangeTokenSelector = mkSelector "initWithPreviousServerChangeToken:"

-- | @Selector@ for @previousServerChangeToken@
previousServerChangeTokenSelector :: Selector '[] (Id CKServerChangeToken)
previousServerChangeTokenSelector = mkSelector "previousServerChangeToken"

-- | @Selector@ for @setPreviousServerChangeToken:@
setPreviousServerChangeTokenSelector :: Selector '[Id CKServerChangeToken] ()
setPreviousServerChangeTokenSelector = mkSelector "setPreviousServerChangeToken:"

-- | @Selector@ for @resultsLimit@
resultsLimitSelector :: Selector '[] CULong
resultsLimitSelector = mkSelector "resultsLimit"

-- | @Selector@ for @setResultsLimit:@
setResultsLimitSelector :: Selector '[CULong] ()
setResultsLimitSelector = mkSelector "setResultsLimit:"

-- | @Selector@ for @fetchAllChanges@
fetchAllChangesSelector :: Selector '[] Bool
fetchAllChangesSelector = mkSelector "fetchAllChanges"

-- | @Selector@ for @setFetchAllChanges:@
setFetchAllChangesSelector :: Selector '[Bool] ()
setFetchAllChangesSelector = mkSelector "setFetchAllChanges:"

-- | @Selector@ for @recordZoneWithIDChangedBlock@
recordZoneWithIDChangedBlockSelector :: Selector '[] (Ptr ())
recordZoneWithIDChangedBlockSelector = mkSelector "recordZoneWithIDChangedBlock"

-- | @Selector@ for @setRecordZoneWithIDChangedBlock:@
setRecordZoneWithIDChangedBlockSelector :: Selector '[Ptr ()] ()
setRecordZoneWithIDChangedBlockSelector = mkSelector "setRecordZoneWithIDChangedBlock:"

-- | @Selector@ for @recordZoneWithIDWasDeletedBlock@
recordZoneWithIDWasDeletedBlockSelector :: Selector '[] (Ptr ())
recordZoneWithIDWasDeletedBlockSelector = mkSelector "recordZoneWithIDWasDeletedBlock"

-- | @Selector@ for @setRecordZoneWithIDWasDeletedBlock:@
setRecordZoneWithIDWasDeletedBlockSelector :: Selector '[Ptr ()] ()
setRecordZoneWithIDWasDeletedBlockSelector = mkSelector "setRecordZoneWithIDWasDeletedBlock:"

-- | @Selector@ for @recordZoneWithIDWasPurgedBlock@
recordZoneWithIDWasPurgedBlockSelector :: Selector '[] (Ptr ())
recordZoneWithIDWasPurgedBlockSelector = mkSelector "recordZoneWithIDWasPurgedBlock"

-- | @Selector@ for @setRecordZoneWithIDWasPurgedBlock:@
setRecordZoneWithIDWasPurgedBlockSelector :: Selector '[Ptr ()] ()
setRecordZoneWithIDWasPurgedBlockSelector = mkSelector "setRecordZoneWithIDWasPurgedBlock:"

-- | @Selector@ for @recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock@
recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector :: Selector '[] (Ptr ())
recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector = mkSelector "recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock"

-- | @Selector@ for @setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock:@
setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector :: Selector '[Ptr ()] ()
setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlockSelector = mkSelector "setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock:"

-- | @Selector@ for @changeTokenUpdatedBlock@
changeTokenUpdatedBlockSelector :: Selector '[] (Ptr ())
changeTokenUpdatedBlockSelector = mkSelector "changeTokenUpdatedBlock"

-- | @Selector@ for @setChangeTokenUpdatedBlock:@
setChangeTokenUpdatedBlockSelector :: Selector '[Ptr ()] ()
setChangeTokenUpdatedBlockSelector = mkSelector "setChangeTokenUpdatedBlock:"

-- | @Selector@ for @fetchDatabaseChangesCompletionBlock@
fetchDatabaseChangesCompletionBlockSelector :: Selector '[] (Ptr ())
fetchDatabaseChangesCompletionBlockSelector = mkSelector "fetchDatabaseChangesCompletionBlock"

-- | @Selector@ for @setFetchDatabaseChangesCompletionBlock:@
setFetchDatabaseChangesCompletionBlockSelector :: Selector '[Ptr ()] ()
setFetchDatabaseChangesCompletionBlockSelector = mkSelector "setFetchDatabaseChangesCompletionBlock:"

