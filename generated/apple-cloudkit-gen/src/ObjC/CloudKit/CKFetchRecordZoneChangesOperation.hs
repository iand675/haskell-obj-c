{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This operation will fetch records changes across the given record zones
--
-- For each @previousServerChangeToken@ passed in with a @CKFetchRecordZoneChangesConfiguration,@ only records that have changed since that anchor will be fetched.  If this is your first fetch of a zone or if you wish to re-fetch all records within a zone, do not include a @previousServerChangeToken.@  Change tokens are opaque tokens and clients should not infer any behavior based on their content.
--
-- Generated bindings for @CKFetchRecordZoneChangesOperation@.
module ObjC.CloudKit.CKFetchRecordZoneChangesOperation
  ( CKFetchRecordZoneChangesOperation
  , IsCKFetchRecordZoneChangesOperation(..)
  , init_
  , initWithRecordZoneIDs_configurationsByRecordZoneID
  , initWithRecordZoneIDs_optionsByRecordZoneID
  , recordZoneIDs
  , setRecordZoneIDs
  , configurationsByRecordZoneID
  , setConfigurationsByRecordZoneID
  , fetchAllChanges
  , setFetchAllChanges
  , recordChangedBlock
  , setRecordChangedBlock
  , recordWasChangedBlock
  , setRecordWasChangedBlock
  , recordWithIDWasDeletedBlock
  , setRecordWithIDWasDeletedBlock
  , recordZoneChangeTokensUpdatedBlock
  , setRecordZoneChangeTokensUpdatedBlock
  , recordZoneFetchCompletionBlock
  , setRecordZoneFetchCompletionBlock
  , fetchRecordZoneChangesCompletionBlock
  , setFetchRecordZoneChangesCompletionBlock
  , optionsByRecordZoneID
  , setOptionsByRecordZoneID
  , configurationsByRecordZoneIDSelector
  , fetchAllChangesSelector
  , fetchRecordZoneChangesCompletionBlockSelector
  , initSelector
  , initWithRecordZoneIDs_configurationsByRecordZoneIDSelector
  , initWithRecordZoneIDs_optionsByRecordZoneIDSelector
  , optionsByRecordZoneIDSelector
  , recordChangedBlockSelector
  , recordWasChangedBlockSelector
  , recordWithIDWasDeletedBlockSelector
  , recordZoneChangeTokensUpdatedBlockSelector
  , recordZoneFetchCompletionBlockSelector
  , recordZoneIDsSelector
  , setConfigurationsByRecordZoneIDSelector
  , setFetchAllChangesSelector
  , setFetchRecordZoneChangesCompletionBlockSelector
  , setOptionsByRecordZoneIDSelector
  , setRecordChangedBlockSelector
  , setRecordWasChangedBlockSelector
  , setRecordWithIDWasDeletedBlockSelector
  , setRecordZoneChangeTokensUpdatedBlockSelector
  , setRecordZoneFetchCompletionBlockSelector
  , setRecordZoneIDsSelector


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
init_ :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id CKFetchRecordZoneChangesOperation)
init_ ckFetchRecordZoneChangesOperation =
  sendOwnedMessage ckFetchRecordZoneChangesOperation initSelector

-- | @- initWithRecordZoneIDs:configurationsByRecordZoneID:@
initWithRecordZoneIDs_configurationsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSArray recordZoneIDs, IsNSDictionary configurationsByRecordZoneID) => ckFetchRecordZoneChangesOperation -> recordZoneIDs -> configurationsByRecordZoneID -> IO (Id CKFetchRecordZoneChangesOperation)
initWithRecordZoneIDs_configurationsByRecordZoneID ckFetchRecordZoneChangesOperation recordZoneIDs configurationsByRecordZoneID =
  sendOwnedMessage ckFetchRecordZoneChangesOperation initWithRecordZoneIDs_configurationsByRecordZoneIDSelector (toNSArray recordZoneIDs) (toNSDictionary configurationsByRecordZoneID)

-- | @- initWithRecordZoneIDs:optionsByRecordZoneID:@
initWithRecordZoneIDs_optionsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSArray recordZoneIDs, IsNSDictionary optionsByRecordZoneID) => ckFetchRecordZoneChangesOperation -> recordZoneIDs -> optionsByRecordZoneID -> IO (Id CKFetchRecordZoneChangesOperation)
initWithRecordZoneIDs_optionsByRecordZoneID ckFetchRecordZoneChangesOperation recordZoneIDs optionsByRecordZoneID =
  sendOwnedMessage ckFetchRecordZoneChangesOperation initWithRecordZoneIDs_optionsByRecordZoneIDSelector (toNSArray recordZoneIDs) (toNSDictionary optionsByRecordZoneID)

-- | @- recordZoneIDs@
recordZoneIDs :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id NSArray)
recordZoneIDs ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation recordZoneIDsSelector

-- | @- setRecordZoneIDs:@
setRecordZoneIDs :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSArray value) => ckFetchRecordZoneChangesOperation -> value -> IO ()
setRecordZoneIDs ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setRecordZoneIDsSelector (toNSArray value)

-- | @- configurationsByRecordZoneID@
configurationsByRecordZoneID :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id NSDictionary)
configurationsByRecordZoneID ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation configurationsByRecordZoneIDSelector

-- | @- setConfigurationsByRecordZoneID:@
setConfigurationsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSDictionary value) => ckFetchRecordZoneChangesOperation -> value -> IO ()
setConfigurationsByRecordZoneID ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setConfigurationsByRecordZoneIDSelector (toNSDictionary value)

-- | Determines if the operation should fetch all changes from the server before completing.
--
-- When set to YES, this operation will send repeated requests to the server until all record changes have been fetched. @recordZoneChangeTokensUpdatedBlock@ will be invoked periodically, to give clients an updated change token so that already-fetched record changes don't need to be re-fetched on a subsequent operation. @recordZoneFetchCompletionBlock@ will only be called once and @moreComing@ will always be NO.
--
-- When set to NO, it is the responsibility of the caller to issue subsequent fetch-changes operations when @moreComing@ is YES in a @recordZoneFetchCompletionBlock@ invocation.
--
-- @fetchAllChanges@ is YES by default
--
-- ObjC selector: @- fetchAllChanges@
fetchAllChanges :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO Bool
fetchAllChanges ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation fetchAllChangesSelector

-- | Determines if the operation should fetch all changes from the server before completing.
--
-- When set to YES, this operation will send repeated requests to the server until all record changes have been fetched. @recordZoneChangeTokensUpdatedBlock@ will be invoked periodically, to give clients an updated change token so that already-fetched record changes don't need to be re-fetched on a subsequent operation. @recordZoneFetchCompletionBlock@ will only be called once and @moreComing@ will always be NO.
--
-- When set to NO, it is the responsibility of the caller to issue subsequent fetch-changes operations when @moreComing@ is YES in a @recordZoneFetchCompletionBlock@ invocation.
--
-- @fetchAllChanges@ is YES by default
--
-- ObjC selector: @- setFetchAllChanges:@
setFetchAllChanges :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Bool -> IO ()
setFetchAllChanges ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setFetchAllChangesSelector value

-- | If the replacement callback @recordWasChangedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordChangedBlock@
recordChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordChangedBlock ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation recordChangedBlockSelector

-- | If the replacement callback @recordWasChangedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordChangedBlock:@
setRecordChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordChangedBlock ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setRecordChangedBlockSelector value

-- | If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordWasChangedBlock@
recordWasChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordWasChangedBlock ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation recordWasChangedBlockSelector

-- | If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordWasChangedBlock:@
setRecordWasChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordWasChangedBlock ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setRecordWasChangedBlockSelector value

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordWithIDWasDeletedBlock ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation recordWithIDWasDeletedBlockSelector

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordWithIDWasDeletedBlock ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setRecordWithIDWasDeletedBlockSelector value

-- | Clients are responsible for saving this per-recordZone @serverChangeToken@ and passing it in to the next call to @CKFetchRecordZoneChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in this block so that already fetched records don't need to be re-downloaded on a subsequent operation.  @recordZoneChangeTokensUpdatedBlock@ will not be called after the last batch of changes in a zone; the @recordZoneFetchCompletionBlock@ block will be called instead.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ issued on this zone is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @serverChangeToken@ used for this record zone when initting this operation was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @serverChangeToken.@  @recordZoneChangeTokensUpdatedBlock@ will not be called if @fetchAllChanges@ is NO.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordZoneChangeTokensUpdatedBlock@
recordZoneChangeTokensUpdatedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordZoneChangeTokensUpdatedBlock ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation recordZoneChangeTokensUpdatedBlockSelector

-- | Clients are responsible for saving this per-recordZone @serverChangeToken@ and passing it in to the next call to @CKFetchRecordZoneChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in this block so that already fetched records don't need to be re-downloaded on a subsequent operation.  @recordZoneChangeTokensUpdatedBlock@ will not be called after the last batch of changes in a zone; the @recordZoneFetchCompletionBlock@ block will be called instead.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ issued on this zone is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @serverChangeToken@ used for this record zone when initting this operation was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @serverChangeToken.@  @recordZoneChangeTokensUpdatedBlock@ will not be called if @fetchAllChanges@ is NO.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordZoneChangeTokensUpdatedBlock:@
setRecordZoneChangeTokensUpdatedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordZoneChangeTokensUpdatedBlock ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setRecordZoneChangeTokensUpdatedBlockSelector value

-- | @- recordZoneFetchCompletionBlock@
recordZoneFetchCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordZoneFetchCompletionBlock ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation recordZoneFetchCompletionBlockSelector

-- | @- setRecordZoneFetchCompletionBlock:@
setRecordZoneFetchCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordZoneFetchCompletionBlock ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setRecordZoneFetchCompletionBlockSelector value

-- | This block is called when the operation completes.
--
-- @serverChangeToken-s@ previously returned via a @recordZoneChangeTokensUpdatedBlock@ or @recordZoneFetchCompletionBlock@ invocation, along with the record changes that preceded it, are valid even if there is a subsequent @operationError@  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of recordIDs and zoneIDs to errors keyed off of @CKPartialErrorsByItemIDKey.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchRecordZoneChangesCompletionBlock@
fetchRecordZoneChangesCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
fetchRecordZoneChangesCompletionBlock ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation fetchRecordZoneChangesCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- @serverChangeToken-s@ previously returned via a @recordZoneChangeTokensUpdatedBlock@ or @recordZoneFetchCompletionBlock@ invocation, along with the record changes that preceded it, are valid even if there is a subsequent @operationError@  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of recordIDs and zoneIDs to errors keyed off of @CKPartialErrorsByItemIDKey.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchRecordZoneChangesCompletionBlock:@
setFetchRecordZoneChangesCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setFetchRecordZoneChangesCompletionBlock ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setFetchRecordZoneChangesCompletionBlockSelector value

-- | @- optionsByRecordZoneID@
optionsByRecordZoneID :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id NSDictionary)
optionsByRecordZoneID ckFetchRecordZoneChangesOperation =
  sendMessage ckFetchRecordZoneChangesOperation optionsByRecordZoneIDSelector

-- | @- setOptionsByRecordZoneID:@
setOptionsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSDictionary value) => ckFetchRecordZoneChangesOperation -> value -> IO ()
setOptionsByRecordZoneID ckFetchRecordZoneChangesOperation value =
  sendMessage ckFetchRecordZoneChangesOperation setOptionsByRecordZoneIDSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchRecordZoneChangesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZoneIDs:configurationsByRecordZoneID:@
initWithRecordZoneIDs_configurationsByRecordZoneIDSelector :: Selector '[Id NSArray, Id NSDictionary] (Id CKFetchRecordZoneChangesOperation)
initWithRecordZoneIDs_configurationsByRecordZoneIDSelector = mkSelector "initWithRecordZoneIDs:configurationsByRecordZoneID:"

-- | @Selector@ for @initWithRecordZoneIDs:optionsByRecordZoneID:@
initWithRecordZoneIDs_optionsByRecordZoneIDSelector :: Selector '[Id NSArray, Id NSDictionary] (Id CKFetchRecordZoneChangesOperation)
initWithRecordZoneIDs_optionsByRecordZoneIDSelector = mkSelector "initWithRecordZoneIDs:optionsByRecordZoneID:"

-- | @Selector@ for @recordZoneIDs@
recordZoneIDsSelector :: Selector '[] (Id NSArray)
recordZoneIDsSelector = mkSelector "recordZoneIDs"

-- | @Selector@ for @setRecordZoneIDs:@
setRecordZoneIDsSelector :: Selector '[Id NSArray] ()
setRecordZoneIDsSelector = mkSelector "setRecordZoneIDs:"

-- | @Selector@ for @configurationsByRecordZoneID@
configurationsByRecordZoneIDSelector :: Selector '[] (Id NSDictionary)
configurationsByRecordZoneIDSelector = mkSelector "configurationsByRecordZoneID"

-- | @Selector@ for @setConfigurationsByRecordZoneID:@
setConfigurationsByRecordZoneIDSelector :: Selector '[Id NSDictionary] ()
setConfigurationsByRecordZoneIDSelector = mkSelector "setConfigurationsByRecordZoneID:"

-- | @Selector@ for @fetchAllChanges@
fetchAllChangesSelector :: Selector '[] Bool
fetchAllChangesSelector = mkSelector "fetchAllChanges"

-- | @Selector@ for @setFetchAllChanges:@
setFetchAllChangesSelector :: Selector '[Bool] ()
setFetchAllChangesSelector = mkSelector "setFetchAllChanges:"

-- | @Selector@ for @recordChangedBlock@
recordChangedBlockSelector :: Selector '[] (Ptr ())
recordChangedBlockSelector = mkSelector "recordChangedBlock"

-- | @Selector@ for @setRecordChangedBlock:@
setRecordChangedBlockSelector :: Selector '[Ptr ()] ()
setRecordChangedBlockSelector = mkSelector "setRecordChangedBlock:"

-- | @Selector@ for @recordWasChangedBlock@
recordWasChangedBlockSelector :: Selector '[] (Ptr ())
recordWasChangedBlockSelector = mkSelector "recordWasChangedBlock"

-- | @Selector@ for @setRecordWasChangedBlock:@
setRecordWasChangedBlockSelector :: Selector '[Ptr ()] ()
setRecordWasChangedBlockSelector = mkSelector "setRecordWasChangedBlock:"

-- | @Selector@ for @recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlockSelector :: Selector '[] (Ptr ())
recordWithIDWasDeletedBlockSelector = mkSelector "recordWithIDWasDeletedBlock"

-- | @Selector@ for @setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlockSelector :: Selector '[Ptr ()] ()
setRecordWithIDWasDeletedBlockSelector = mkSelector "setRecordWithIDWasDeletedBlock:"

-- | @Selector@ for @recordZoneChangeTokensUpdatedBlock@
recordZoneChangeTokensUpdatedBlockSelector :: Selector '[] (Ptr ())
recordZoneChangeTokensUpdatedBlockSelector = mkSelector "recordZoneChangeTokensUpdatedBlock"

-- | @Selector@ for @setRecordZoneChangeTokensUpdatedBlock:@
setRecordZoneChangeTokensUpdatedBlockSelector :: Selector '[Ptr ()] ()
setRecordZoneChangeTokensUpdatedBlockSelector = mkSelector "setRecordZoneChangeTokensUpdatedBlock:"

-- | @Selector@ for @recordZoneFetchCompletionBlock@
recordZoneFetchCompletionBlockSelector :: Selector '[] (Ptr ())
recordZoneFetchCompletionBlockSelector = mkSelector "recordZoneFetchCompletionBlock"

-- | @Selector@ for @setRecordZoneFetchCompletionBlock:@
setRecordZoneFetchCompletionBlockSelector :: Selector '[Ptr ()] ()
setRecordZoneFetchCompletionBlockSelector = mkSelector "setRecordZoneFetchCompletionBlock:"

-- | @Selector@ for @fetchRecordZoneChangesCompletionBlock@
fetchRecordZoneChangesCompletionBlockSelector :: Selector '[] (Ptr ())
fetchRecordZoneChangesCompletionBlockSelector = mkSelector "fetchRecordZoneChangesCompletionBlock"

-- | @Selector@ for @setFetchRecordZoneChangesCompletionBlock:@
setFetchRecordZoneChangesCompletionBlockSelector :: Selector '[Ptr ()] ()
setFetchRecordZoneChangesCompletionBlockSelector = mkSelector "setFetchRecordZoneChangesCompletionBlock:"

-- | @Selector@ for @optionsByRecordZoneID@
optionsByRecordZoneIDSelector :: Selector '[] (Id NSDictionary)
optionsByRecordZoneIDSelector = mkSelector "optionsByRecordZoneID"

-- | @Selector@ for @setOptionsByRecordZoneID:@
setOptionsByRecordZoneIDSelector :: Selector '[Id NSDictionary] ()
setOptionsByRecordZoneIDSelector = mkSelector "setOptionsByRecordZoneID:"

