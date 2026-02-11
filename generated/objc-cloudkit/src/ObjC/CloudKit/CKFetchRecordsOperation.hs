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
  , initSelector
  , initWithRecordIDsSelector
  , fetchCurrentUserRecordOperationSelector
  , recordIDsSelector
  , setRecordIDsSelector
  , desiredKeysSelector
  , setDesiredKeysSelector
  , perRecordProgressBlockSelector
  , setPerRecordProgressBlockSelector
  , perRecordCompletionBlockSelector
  , setPerRecordCompletionBlockSelector


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
init_ :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Id CKFetchRecordsOperation)
init_ ckFetchRecordsOperation  =
  sendMsg ckFetchRecordsOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRecordIDs:@
initWithRecordIDs :: (IsCKFetchRecordsOperation ckFetchRecordsOperation, IsNSArray recordIDs) => ckFetchRecordsOperation -> recordIDs -> IO (Id CKFetchRecordsOperation)
initWithRecordIDs ckFetchRecordsOperation  recordIDs =
withObjCPtr recordIDs $ \raw_recordIDs ->
    sendMsg ckFetchRecordsOperation (mkSelector "initWithRecordIDs:") (retPtr retVoid) [argPtr (castPtr raw_recordIDs :: Ptr ())] >>= ownedObject . castPtr

-- | @+ fetchCurrentUserRecordOperation@
fetchCurrentUserRecordOperation :: IO (Id CKFetchRecordsOperation)
fetchCurrentUserRecordOperation  =
  do
    cls' <- getRequiredClass "CKFetchRecordsOperation"
    sendClassMsg cls' (mkSelector "fetchCurrentUserRecordOperation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recordIDs@
recordIDs :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Id NSArray)
recordIDs ckFetchRecordsOperation  =
  sendMsg ckFetchRecordsOperation (mkSelector "recordIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordIDs:@
setRecordIDs :: (IsCKFetchRecordsOperation ckFetchRecordsOperation, IsNSArray value) => ckFetchRecordsOperation -> value -> IO ()
setRecordIDs ckFetchRecordsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordsOperation (mkSelector "setRecordIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Id NSArray)
desiredKeys ckFetchRecordsOperation  =
  sendMsg ckFetchRecordsOperation (mkSelector "desiredKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordsOperation ckFetchRecordsOperation, IsNSArray value) => ckFetchRecordsOperation -> value -> IO ()
setDesiredKeys ckFetchRecordsOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordsOperation (mkSelector "setDesiredKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordProgressBlock@
perRecordProgressBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Ptr ())
perRecordProgressBlock ckFetchRecordsOperation  =
  fmap castPtr $ sendMsg ckFetchRecordsOperation (mkSelector "perRecordProgressBlock") (retPtr retVoid) []

-- | Indicates the progress for each record.
--
-- This method is called at least once with a progress of 1.0 for every record. Intermediate progress is only reported for records that contain assets.  It is possible for progress to regress when a retry is automatically triggered.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordProgressBlock:@
setPerRecordProgressBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> Ptr () -> IO ()
setPerRecordProgressBlock ckFetchRecordsOperation  value =
  sendMsg ckFetchRecordsOperation (mkSelector "setPerRecordProgressBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Called on success or failure for each record.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perRecordCompletionBlock@
perRecordCompletionBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> IO (Ptr ())
perRecordCompletionBlock ckFetchRecordsOperation  =
  fmap castPtr $ sendMsg ckFetchRecordsOperation (mkSelector "perRecordCompletionBlock") (retPtr retVoid) []

-- | Called on success or failure for each record.
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerRecordCompletionBlock:@
setPerRecordCompletionBlock :: IsCKFetchRecordsOperation ckFetchRecordsOperation => ckFetchRecordsOperation -> Ptr () -> IO ()
setPerRecordCompletionBlock ckFetchRecordsOperation  value =
  sendMsg ckFetchRecordsOperation (mkSelector "setPerRecordCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordIDs:@
initWithRecordIDsSelector :: Selector
initWithRecordIDsSelector = mkSelector "initWithRecordIDs:"

-- | @Selector@ for @fetchCurrentUserRecordOperation@
fetchCurrentUserRecordOperationSelector :: Selector
fetchCurrentUserRecordOperationSelector = mkSelector "fetchCurrentUserRecordOperation"

-- | @Selector@ for @recordIDs@
recordIDsSelector :: Selector
recordIDsSelector = mkSelector "recordIDs"

-- | @Selector@ for @setRecordIDs:@
setRecordIDsSelector :: Selector
setRecordIDsSelector = mkSelector "setRecordIDs:"

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

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

