{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKFetchRecordsOperation@.
module ObjC.CloudKit.CKFetchRecordsOperation
  ( CKFetchRecordsOperation
  , IsCKFetchRecordsOperation(..)
  , init_
  , initWithRecordIDs
  , fetchCurrentUserRecordOperation
  , recordIDs
  , setRecordIDs
  , desiredKeys
  , setDesiredKeys
  , perRecordProgressBlock
  , setPerRecordProgressBlock
  , perRecordCompletionBlock
  , setPerRecordCompletionBlock
  , desiredKeysSelector
  , fetchCurrentUserRecordOperationSelector
  , initSelector
  , initWithRecordIDsSelector
  , perRecordCompletionBlockSelector
  , perRecordProgressBlockSelector
  , recordIDsSelector
  , setDesiredKeysSelector
  , setPerRecordCompletionBlockSelector
  , setPerRecordProgressBlockSelector
  , setRecordIDsSelector


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
init_ :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Id CKFetchRecordsOperation)
init_ ckFetchRecordsOperation =
  sendOwnedMessage ckFetchRecordsOperation initSelector

-- | @- initWithRecordIDs:@
initWithRecordIDs :: (IsCKFetchRecordsOperation ckFetchRecordsOperation, IsNSArray recordIDs) => ckFetchRecordsOperation -> recordIDs -> IO (Id CKFetchRecordsOperation)
initWithRecordIDs ckFetchRecordsOperation recordIDs =
  sendOwnedMessage ckFetchRecordsOperation initWithRecordIDsSelector (toNSArray recordIDs)

-- | @+ fetchCurrentUserRecordOperation@
fetchCurrentUserRecordOperation :: IO (Id CKFetchRecordsOperation)
fetchCurrentUserRecordOperation  =
  do
    cls' <- getRequiredClass "CKFetchRecordsOperation"
    sendClassMessage cls' fetchCurrentUserRecordOperationSelector

-- | @- recordIDs@
recordIDs :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Id NSArray)
recordIDs ckFetchRecordsOperation =
  sendMessage ckFetchRecordsOperation recordIDsSelector

-- | @- setRecordIDs:@
setRecordIDs :: (IsCKFetchRecordsOperation ckFetchRecordsOperation, IsNSArray value) => ckFetchRecordsOperation -> value -> IO ()
setRecordIDs ckFetchRecordsOperation value =
  sendMessage ckFetchRecordsOperation setRecordIDsSelector (toNSArray value)

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Id NSArray)
desiredKeys ckFetchRecordsOperation =
  sendMessage ckFetchRecordsOperation desiredKeysSelector

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordsOperation ckFetchRecordsOperation, IsNSArray value) => ckFetchRecordsOperation -> value -> IO ()
setDesiredKeys ckFetchRecordsOperation value =
  sendMessage ckFetchRecordsOperation setDesiredKeysSelector (toNSArray value)

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordProgressBlock@
perRecordProgressBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Ptr ())
perRecordProgressBlock ckFetchRecordsOperation =
  sendMessage ckFetchRecordsOperation perRecordProgressBlockSelector

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordProgressBlock:@
setPerRecordProgressBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> Ptr () -> IO ()
setPerRecordProgressBlock ckFetchRecordsOperation value =
  sendMessage ckFetchRecordsOperation setPerRecordProgressBlockSelector value

-- | Called on success or failure for each record.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordCompletionBlock@
perRecordCompletionBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Ptr ())
perRecordCompletionBlock ckFetchRecordsOperation =
  sendMessage ckFetchRecordsOperation perRecordCompletionBlockSelector

-- | Called on success or failure for each record.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordCompletionBlock:@
setPerRecordCompletionBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> Ptr () -> IO ()
setPerRecordCompletionBlock ckFetchRecordsOperation value =
  sendMessage ckFetchRecordsOperation setPerRecordCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchRecordsOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordIDs:@
initWithRecordIDsSelector :: Selector '[Id NSArray] (Id CKFetchRecordsOperation)
initWithRecordIDsSelector = mkSelector "initWithRecordIDs:"

-- | @Selector@ for @fetchCurrentUserRecordOperation@
fetchCurrentUserRecordOperationSelector :: Selector '[] (Id CKFetchRecordsOperation)
fetchCurrentUserRecordOperationSelector = mkSelector "fetchCurrentUserRecordOperation"

-- | @Selector@ for @recordIDs@
recordIDsSelector :: Selector '[] (Id NSArray)
recordIDsSelector = mkSelector "recordIDs"

-- | @Selector@ for @setRecordIDs:@
setRecordIDsSelector :: Selector '[Id NSArray] ()
setRecordIDsSelector = mkSelector "setRecordIDs:"

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector '[] (Id NSArray)
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector '[Id NSArray] ()
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

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

