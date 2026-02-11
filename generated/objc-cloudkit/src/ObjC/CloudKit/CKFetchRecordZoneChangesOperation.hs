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
  , initSelector
  , initWithRecordZoneIDs_configurationsByRecordZoneIDSelector
  , initWithRecordZoneIDs_optionsByRecordZoneIDSelector
  , recordZoneIDsSelector
  , setRecordZoneIDsSelector
  , configurationsByRecordZoneIDSelector
  , setConfigurationsByRecordZoneIDSelector
  , fetchAllChangesSelector
  , setFetchAllChangesSelector
  , recordChangedBlockSelector
  , setRecordChangedBlockSelector
  , recordWasChangedBlockSelector
  , setRecordWasChangedBlockSelector
  , recordWithIDWasDeletedBlockSelector
  , setRecordWithIDWasDeletedBlockSelector
  , recordZoneChangeTokensUpdatedBlockSelector
  , setRecordZoneChangeTokensUpdatedBlockSelector
  , recordZoneFetchCompletionBlockSelector
  , setRecordZoneFetchCompletionBlockSelector
  , fetchRecordZoneChangesCompletionBlockSelector
  , setFetchRecordZoneChangesCompletionBlockSelector
  , optionsByRecordZoneIDSelector
  , setOptionsByRecordZoneIDSelector


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
init_ :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id CKFetchRecordZoneChangesOperation)
init_ ckFetchRecordZoneChangesOperation  =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRecordZoneIDs:configurationsByRecordZoneID:@
initWithRecordZoneIDs_configurationsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSArray recordZoneIDs, IsNSDictionary configurationsByRecordZoneID) => ckFetchRecordZoneChangesOperation -> recordZoneIDs -> configurationsByRecordZoneID -> IO (Id CKFetchRecordZoneChangesOperation)
initWithRecordZoneIDs_configurationsByRecordZoneID ckFetchRecordZoneChangesOperation  recordZoneIDs configurationsByRecordZoneID =
withObjCPtr recordZoneIDs $ \raw_recordZoneIDs ->
  withObjCPtr configurationsByRecordZoneID $ \raw_configurationsByRecordZoneID ->
      sendMsg ckFetchRecordZoneChangesOperation (mkSelector "initWithRecordZoneIDs:configurationsByRecordZoneID:") (retPtr retVoid) [argPtr (castPtr raw_recordZoneIDs :: Ptr ()), argPtr (castPtr raw_configurationsByRecordZoneID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecordZoneIDs:optionsByRecordZoneID:@
initWithRecordZoneIDs_optionsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSArray recordZoneIDs, IsNSDictionary optionsByRecordZoneID) => ckFetchRecordZoneChangesOperation -> recordZoneIDs -> optionsByRecordZoneID -> IO (Id CKFetchRecordZoneChangesOperation)
initWithRecordZoneIDs_optionsByRecordZoneID ckFetchRecordZoneChangesOperation  recordZoneIDs optionsByRecordZoneID =
withObjCPtr recordZoneIDs $ \raw_recordZoneIDs ->
  withObjCPtr optionsByRecordZoneID $ \raw_optionsByRecordZoneID ->
      sendMsg ckFetchRecordZoneChangesOperation (mkSelector "initWithRecordZoneIDs:optionsByRecordZoneID:") (retPtr retVoid) [argPtr (castPtr raw_recordZoneIDs :: Ptr ()), argPtr (castPtr raw_optionsByRecordZoneID :: Ptr ())] >>= ownedObject . castPtr

-- | @- recordZoneIDs@
recordZoneIDs :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id NSArray)
recordZoneIDs ckFetchRecordZoneChangesOperation  =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "recordZoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordZoneIDs:@
setRecordZoneIDs :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSArray value) => ckFetchRecordZoneChangesOperation -> value -> IO ()
setRecordZoneIDs ckFetchRecordZoneChangesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setRecordZoneIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- configurationsByRecordZoneID@
configurationsByRecordZoneID :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id NSDictionary)
configurationsByRecordZoneID ckFetchRecordZoneChangesOperation  =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "configurationsByRecordZoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConfigurationsByRecordZoneID:@
setConfigurationsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSDictionary value) => ckFetchRecordZoneChangesOperation -> value -> IO ()
setConfigurationsByRecordZoneID ckFetchRecordZoneChangesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setConfigurationsByRecordZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
fetchAllChanges ckFetchRecordZoneChangesOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckFetchRecordZoneChangesOperation (mkSelector "fetchAllChanges") retCULong []

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
setFetchAllChanges ckFetchRecordZoneChangesOperation  value =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setFetchAllChanges:") retVoid [argCULong (if value then 1 else 0)]

-- | If the replacement callback @recordWasChangedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordChangedBlock@
recordChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordChangedBlock ckFetchRecordZoneChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordZoneChangesOperation (mkSelector "recordChangedBlock") (retPtr retVoid) []

-- | If the replacement callback @recordWasChangedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordChangedBlock:@
setRecordChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordChangedBlock ckFetchRecordZoneChangesOperation  value =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setRecordChangedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordWasChangedBlock@
recordWasChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordWasChangedBlock ckFetchRecordZoneChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordZoneChangesOperation (mkSelector "recordWasChangedBlock") (retPtr retVoid) []

-- | If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordWasChangedBlock:@
setRecordWasChangedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordWasChangedBlock ckFetchRecordZoneChangesOperation  value =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setRecordWasChangedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordWithIDWasDeletedBlock ckFetchRecordZoneChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordZoneChangesOperation (mkSelector "recordWithIDWasDeletedBlock") (retPtr retVoid) []

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordWithIDWasDeletedBlock ckFetchRecordZoneChangesOperation  value =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setRecordWithIDWasDeletedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Clients are responsible for saving this per-recordZone @serverChangeToken@ and passing it in to the next call to @CKFetchRecordZoneChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in this block so that already fetched records don't need to be re-downloaded on a subsequent operation.  @recordZoneChangeTokensUpdatedBlock@ will not be called after the last batch of changes in a zone; the @recordZoneFetchCompletionBlock@ block will be called instead.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ issued on this zone is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @serverChangeToken@ used for this record zone when initting this operation was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @serverChangeToken.@  @recordZoneChangeTokensUpdatedBlock@ will not be called if @fetchAllChanges@ is NO.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordZoneChangeTokensUpdatedBlock@
recordZoneChangeTokensUpdatedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordZoneChangeTokensUpdatedBlock ckFetchRecordZoneChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordZoneChangesOperation (mkSelector "recordZoneChangeTokensUpdatedBlock") (retPtr retVoid) []

-- | Clients are responsible for saving this per-recordZone @serverChangeToken@ and passing it in to the next call to @CKFetchRecordZoneChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in this block so that already fetched records don't need to be re-downloaded on a subsequent operation.  @recordZoneChangeTokensUpdatedBlock@ will not be called after the last batch of changes in a zone; the @recordZoneFetchCompletionBlock@ block will be called instead.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ issued on this zone is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @serverChangeToken@ used for this record zone when initting this operation was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @serverChangeToken.@  @recordZoneChangeTokensUpdatedBlock@ will not be called if @fetchAllChanges@ is NO.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordZoneChangeTokensUpdatedBlock:@
setRecordZoneChangeTokensUpdatedBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordZoneChangeTokensUpdatedBlock ckFetchRecordZoneChangesOperation  value =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setRecordZoneChangeTokensUpdatedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- recordZoneFetchCompletionBlock@
recordZoneFetchCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
recordZoneFetchCompletionBlock ckFetchRecordZoneChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordZoneChangesOperation (mkSelector "recordZoneFetchCompletionBlock") (retPtr retVoid) []

-- | @- setRecordZoneFetchCompletionBlock:@
setRecordZoneFetchCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setRecordZoneFetchCompletionBlock ckFetchRecordZoneChangesOperation  value =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setRecordZoneFetchCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | This block is called when the operation completes.
--
-- @serverChangeToken-s@ previously returned via a @recordZoneChangeTokensUpdatedBlock@ or @recordZoneFetchCompletionBlock@ invocation, along with the record changes that preceded it, are valid even if there is a subsequent @operationError@  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of recordIDs and zoneIDs to errors keyed off of @CKPartialErrorsByItemIDKey.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchRecordZoneChangesCompletionBlock@
fetchRecordZoneChangesCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Ptr ())
fetchRecordZoneChangesCompletionBlock ckFetchRecordZoneChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordZoneChangesOperation (mkSelector "fetchRecordZoneChangesCompletionBlock") (retPtr retVoid) []

-- | This block is called when the operation completes.
--
-- @serverChangeToken-s@ previously returned via a @recordZoneChangeTokensUpdatedBlock@ or @recordZoneFetchCompletionBlock@ invocation, along with the record changes that preceded it, are valid even if there is a subsequent @operationError@  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of recordIDs and zoneIDs to errors keyed off of @CKPartialErrorsByItemIDKey.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchRecordZoneChangesCompletionBlock:@
setFetchRecordZoneChangesCompletionBlock :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> Ptr () -> IO ()
setFetchRecordZoneChangesCompletionBlock ckFetchRecordZoneChangesOperation  value =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setFetchRecordZoneChangesCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- optionsByRecordZoneID@
optionsByRecordZoneID :: IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation => ckFetchRecordZoneChangesOperation -> IO (Id NSDictionary)
optionsByRecordZoneID ckFetchRecordZoneChangesOperation  =
  sendMsg ckFetchRecordZoneChangesOperation (mkSelector "optionsByRecordZoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOptionsByRecordZoneID:@
setOptionsByRecordZoneID :: (IsCKFetchRecordZoneChangesOperation ckFetchRecordZoneChangesOperation, IsNSDictionary value) => ckFetchRecordZoneChangesOperation -> value -> IO ()
setOptionsByRecordZoneID ckFetchRecordZoneChangesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZoneChangesOperation (mkSelector "setOptionsByRecordZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZoneIDs:configurationsByRecordZoneID:@
initWithRecordZoneIDs_configurationsByRecordZoneIDSelector :: Selector
initWithRecordZoneIDs_configurationsByRecordZoneIDSelector = mkSelector "initWithRecordZoneIDs:configurationsByRecordZoneID:"

-- | @Selector@ for @initWithRecordZoneIDs:optionsByRecordZoneID:@
initWithRecordZoneIDs_optionsByRecordZoneIDSelector :: Selector
initWithRecordZoneIDs_optionsByRecordZoneIDSelector = mkSelector "initWithRecordZoneIDs:optionsByRecordZoneID:"

-- | @Selector@ for @recordZoneIDs@
recordZoneIDsSelector :: Selector
recordZoneIDsSelector = mkSelector "recordZoneIDs"

-- | @Selector@ for @setRecordZoneIDs:@
setRecordZoneIDsSelector :: Selector
setRecordZoneIDsSelector = mkSelector "setRecordZoneIDs:"

-- | @Selector@ for @configurationsByRecordZoneID@
configurationsByRecordZoneIDSelector :: Selector
configurationsByRecordZoneIDSelector = mkSelector "configurationsByRecordZoneID"

-- | @Selector@ for @setConfigurationsByRecordZoneID:@
setConfigurationsByRecordZoneIDSelector :: Selector
setConfigurationsByRecordZoneIDSelector = mkSelector "setConfigurationsByRecordZoneID:"

-- | @Selector@ for @fetchAllChanges@
fetchAllChangesSelector :: Selector
fetchAllChangesSelector = mkSelector "fetchAllChanges"

-- | @Selector@ for @setFetchAllChanges:@
setFetchAllChangesSelector :: Selector
setFetchAllChangesSelector = mkSelector "setFetchAllChanges:"

-- | @Selector@ for @recordChangedBlock@
recordChangedBlockSelector :: Selector
recordChangedBlockSelector = mkSelector "recordChangedBlock"

-- | @Selector@ for @setRecordChangedBlock:@
setRecordChangedBlockSelector :: Selector
setRecordChangedBlockSelector = mkSelector "setRecordChangedBlock:"

-- | @Selector@ for @recordWasChangedBlock@
recordWasChangedBlockSelector :: Selector
recordWasChangedBlockSelector = mkSelector "recordWasChangedBlock"

-- | @Selector@ for @setRecordWasChangedBlock:@
setRecordWasChangedBlockSelector :: Selector
setRecordWasChangedBlockSelector = mkSelector "setRecordWasChangedBlock:"

-- | @Selector@ for @recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlockSelector :: Selector
recordWithIDWasDeletedBlockSelector = mkSelector "recordWithIDWasDeletedBlock"

-- | @Selector@ for @setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlockSelector :: Selector
setRecordWithIDWasDeletedBlockSelector = mkSelector "setRecordWithIDWasDeletedBlock:"

-- | @Selector@ for @recordZoneChangeTokensUpdatedBlock@
recordZoneChangeTokensUpdatedBlockSelector :: Selector
recordZoneChangeTokensUpdatedBlockSelector = mkSelector "recordZoneChangeTokensUpdatedBlock"

-- | @Selector@ for @setRecordZoneChangeTokensUpdatedBlock:@
setRecordZoneChangeTokensUpdatedBlockSelector :: Selector
setRecordZoneChangeTokensUpdatedBlockSelector = mkSelector "setRecordZoneChangeTokensUpdatedBlock:"

-- | @Selector@ for @recordZoneFetchCompletionBlock@
recordZoneFetchCompletionBlockSelector :: Selector
recordZoneFetchCompletionBlockSelector = mkSelector "recordZoneFetchCompletionBlock"

-- | @Selector@ for @setRecordZoneFetchCompletionBlock:@
setRecordZoneFetchCompletionBlockSelector :: Selector
setRecordZoneFetchCompletionBlockSelector = mkSelector "setRecordZoneFetchCompletionBlock:"

-- | @Selector@ for @fetchRecordZoneChangesCompletionBlock@
fetchRecordZoneChangesCompletionBlockSelector :: Selector
fetchRecordZoneChangesCompletionBlockSelector = mkSelector "fetchRecordZoneChangesCompletionBlock"

-- | @Selector@ for @setFetchRecordZoneChangesCompletionBlock:@
setFetchRecordZoneChangesCompletionBlockSelector :: Selector
setFetchRecordZoneChangesCompletionBlockSelector = mkSelector "setFetchRecordZoneChangesCompletionBlock:"

-- | @Selector@ for @optionsByRecordZoneID@
optionsByRecordZoneIDSelector :: Selector
optionsByRecordZoneIDSelector = mkSelector "optionsByRecordZoneID"

-- | @Selector@ for @setOptionsByRecordZoneID:@
setOptionsByRecordZoneIDSelector :: Selector
setOptionsByRecordZoneIDSelector = mkSelector "setOptionsByRecordZoneID:"

