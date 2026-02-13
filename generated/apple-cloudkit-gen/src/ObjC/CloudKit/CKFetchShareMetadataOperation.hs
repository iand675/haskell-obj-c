{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKFetchShareMetadataOperation
--
-- Fetch the @CKShareMetadata@ for a share URL.
--
-- Since you can't know what container this share is in before you fetch its metadata, you may run this operation in any container you have access to
--
-- Generated bindings for @CKFetchShareMetadataOperation@.
module ObjC.CloudKit.CKFetchShareMetadataOperation
  ( CKFetchShareMetadataOperation
  , IsCKFetchShareMetadataOperation(..)
  , init_
  , initWithShareURLs
  , shareURLs
  , setShareURLs
  , shouldFetchRootRecord
  , setShouldFetchRootRecord
  , rootRecordDesiredKeys
  , setRootRecordDesiredKeys
  , perShareMetadataBlock
  , setPerShareMetadataBlock
  , fetchShareMetadataCompletionBlock
  , setFetchShareMetadataCompletionBlock
  , fetchShareMetadataCompletionBlockSelector
  , initSelector
  , initWithShareURLsSelector
  , perShareMetadataBlockSelector
  , rootRecordDesiredKeysSelector
  , setFetchShareMetadataCompletionBlockSelector
  , setPerShareMetadataBlockSelector
  , setRootRecordDesiredKeysSelector
  , setShareURLsSelector
  , setShouldFetchRootRecordSelector
  , shareURLsSelector
  , shouldFetchRootRecordSelector


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
init_ :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Id CKFetchShareMetadataOperation)
init_ ckFetchShareMetadataOperation =
  sendOwnedMessage ckFetchShareMetadataOperation initSelector

-- | @- initWithShareURLs:@
initWithShareURLs :: (IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation, IsNSArray shareURLs) => ckFetchShareMetadataOperation -> shareURLs -> IO (Id CKFetchShareMetadataOperation)
initWithShareURLs ckFetchShareMetadataOperation shareURLs =
  sendOwnedMessage ckFetchShareMetadataOperation initWithShareURLsSelector (toNSArray shareURLs)

-- | @- shareURLs@
shareURLs :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Id NSArray)
shareURLs ckFetchShareMetadataOperation =
  sendMessage ckFetchShareMetadataOperation shareURLsSelector

-- | @- setShareURLs:@
setShareURLs :: (IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation, IsNSArray value) => ckFetchShareMetadataOperation -> value -> IO ()
setShareURLs ckFetchShareMetadataOperation value =
  sendMessage ckFetchShareMetadataOperation setShareURLsSelector (toNSArray value)

-- | If set to YES, the resulting @CKShareMetadata@ will have a @rootRecord@ object filled out.
--
-- Defaults to @NO.@  The resulting @CKShareMetadata@ will have a @rootRecordID@ property regardless of the value of this property.
--
-- ObjC selector: @- shouldFetchRootRecord@
shouldFetchRootRecord :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO Bool
shouldFetchRootRecord ckFetchShareMetadataOperation =
  sendMessage ckFetchShareMetadataOperation shouldFetchRootRecordSelector

-- | If set to YES, the resulting @CKShareMetadata@ will have a @rootRecord@ object filled out.
--
-- Defaults to @NO.@  The resulting @CKShareMetadata@ will have a @rootRecordID@ property regardless of the value of this property.
--
-- ObjC selector: @- setShouldFetchRootRecord:@
setShouldFetchRootRecord :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> Bool -> IO ()
setShouldFetchRootRecord ckFetchShareMetadataOperation value =
  sendMessage ckFetchShareMetadataOperation setShouldFetchRootRecordSelector value

-- | Declares which user-defined keys should be fetched and added to the resulting @rootRecord.@
--
-- Only consulted if @shouldFetchRootRecord@ is @YES.@  If nil, declares the entire root record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- rootRecordDesiredKeys@
rootRecordDesiredKeys :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Id NSArray)
rootRecordDesiredKeys ckFetchShareMetadataOperation =
  sendMessage ckFetchShareMetadataOperation rootRecordDesiredKeysSelector

-- | Declares which user-defined keys should be fetched and added to the resulting @rootRecord.@
--
-- Only consulted if @shouldFetchRootRecord@ is @YES.@  If nil, declares the entire root record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setRootRecordDesiredKeys:@
setRootRecordDesiredKeys :: (IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation, IsNSArray value) => ckFetchShareMetadataOperation -> value -> IO ()
setRootRecordDesiredKeys ckFetchShareMetadataOperation value =
  sendMessage ckFetchShareMetadataOperation setRootRecordDesiredKeysSelector (toNSArray value)

-- | Called once for each share URL that the server processed
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perShareMetadataBlock@
perShareMetadataBlock :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Ptr ())
perShareMetadataBlock ckFetchShareMetadataOperation =
  sendMessage ckFetchShareMetadataOperation perShareMetadataBlockSelector

-- | Called once for each share URL that the server processed
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerShareMetadataBlock:@
setPerShareMetadataBlock :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> Ptr () -> IO ()
setPerShareMetadataBlock ckFetchShareMetadataOperation value =
  sendMessage ckFetchShareMetadataOperation setPerShareMetadataBlockSelector value

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of shareURLs to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @perShareMetadataBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchShareMetadataCompletionBlock@
fetchShareMetadataCompletionBlock :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Ptr ())
fetchShareMetadataCompletionBlock ckFetchShareMetadataOperation =
  sendMessage ckFetchShareMetadataOperation fetchShareMetadataCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of shareURLs to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @perShareMetadataBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchShareMetadataCompletionBlock:@
setFetchShareMetadataCompletionBlock :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> Ptr () -> IO ()
setFetchShareMetadataCompletionBlock ckFetchShareMetadataOperation value =
  sendMessage ckFetchShareMetadataOperation setFetchShareMetadataCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchShareMetadataOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithShareURLs:@
initWithShareURLsSelector :: Selector '[Id NSArray] (Id CKFetchShareMetadataOperation)
initWithShareURLsSelector = mkSelector "initWithShareURLs:"

-- | @Selector@ for @shareURLs@
shareURLsSelector :: Selector '[] (Id NSArray)
shareURLsSelector = mkSelector "shareURLs"

-- | @Selector@ for @setShareURLs:@
setShareURLsSelector :: Selector '[Id NSArray] ()
setShareURLsSelector = mkSelector "setShareURLs:"

-- | @Selector@ for @shouldFetchRootRecord@
shouldFetchRootRecordSelector :: Selector '[] Bool
shouldFetchRootRecordSelector = mkSelector "shouldFetchRootRecord"

-- | @Selector@ for @setShouldFetchRootRecord:@
setShouldFetchRootRecordSelector :: Selector '[Bool] ()
setShouldFetchRootRecordSelector = mkSelector "setShouldFetchRootRecord:"

-- | @Selector@ for @rootRecordDesiredKeys@
rootRecordDesiredKeysSelector :: Selector '[] (Id NSArray)
rootRecordDesiredKeysSelector = mkSelector "rootRecordDesiredKeys"

-- | @Selector@ for @setRootRecordDesiredKeys:@
setRootRecordDesiredKeysSelector :: Selector '[Id NSArray] ()
setRootRecordDesiredKeysSelector = mkSelector "setRootRecordDesiredKeys:"

-- | @Selector@ for @perShareMetadataBlock@
perShareMetadataBlockSelector :: Selector '[] (Ptr ())
perShareMetadataBlockSelector = mkSelector "perShareMetadataBlock"

-- | @Selector@ for @setPerShareMetadataBlock:@
setPerShareMetadataBlockSelector :: Selector '[Ptr ()] ()
setPerShareMetadataBlockSelector = mkSelector "setPerShareMetadataBlock:"

-- | @Selector@ for @fetchShareMetadataCompletionBlock@
fetchShareMetadataCompletionBlockSelector :: Selector '[] (Ptr ())
fetchShareMetadataCompletionBlockSelector = mkSelector "fetchShareMetadataCompletionBlock"

-- | @Selector@ for @setFetchShareMetadataCompletionBlock:@
setFetchShareMetadataCompletionBlockSelector :: Selector '[Ptr ()] ()
setFetchShareMetadataCompletionBlockSelector = mkSelector "setFetchShareMetadataCompletionBlock:"

