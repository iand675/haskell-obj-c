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
  , initSelector
  , initWithShareURLsSelector
  , shareURLsSelector
  , setShareURLsSelector
  , shouldFetchRootRecordSelector
  , setShouldFetchRootRecordSelector
  , rootRecordDesiredKeysSelector
  , setRootRecordDesiredKeysSelector
  , perShareMetadataBlockSelector
  , setPerShareMetadataBlockSelector
  , fetchShareMetadataCompletionBlockSelector
  , setFetchShareMetadataCompletionBlockSelector


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
init_ :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Id CKFetchShareMetadataOperation)
init_ ckFetchShareMetadataOperation  =
  sendMsg ckFetchShareMetadataOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithShareURLs:@
initWithShareURLs :: (IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation, IsNSArray shareURLs) => ckFetchShareMetadataOperation -> shareURLs -> IO (Id CKFetchShareMetadataOperation)
initWithShareURLs ckFetchShareMetadataOperation  shareURLs =
withObjCPtr shareURLs $ \raw_shareURLs ->
    sendMsg ckFetchShareMetadataOperation (mkSelector "initWithShareURLs:") (retPtr retVoid) [argPtr (castPtr raw_shareURLs :: Ptr ())] >>= ownedObject . castPtr

-- | @- shareURLs@
shareURLs :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Id NSArray)
shareURLs ckFetchShareMetadataOperation  =
  sendMsg ckFetchShareMetadataOperation (mkSelector "shareURLs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShareURLs:@
setShareURLs :: (IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation, IsNSArray value) => ckFetchShareMetadataOperation -> value -> IO ()
setShareURLs ckFetchShareMetadataOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchShareMetadataOperation (mkSelector "setShareURLs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If set to YES, the resulting @CKShareMetadata@ will have a @rootRecord@ object filled out.
--
-- Defaults to @NO.@  The resulting @CKShareMetadata@ will have a @rootRecordID@ property regardless of the value of this property.
--
-- ObjC selector: @- shouldFetchRootRecord@
shouldFetchRootRecord :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO Bool
shouldFetchRootRecord ckFetchShareMetadataOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckFetchShareMetadataOperation (mkSelector "shouldFetchRootRecord") retCULong []

-- | If set to YES, the resulting @CKShareMetadata@ will have a @rootRecord@ object filled out.
--
-- Defaults to @NO.@  The resulting @CKShareMetadata@ will have a @rootRecordID@ property regardless of the value of this property.
--
-- ObjC selector: @- setShouldFetchRootRecord:@
setShouldFetchRootRecord :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> Bool -> IO ()
setShouldFetchRootRecord ckFetchShareMetadataOperation  value =
  sendMsg ckFetchShareMetadataOperation (mkSelector "setShouldFetchRootRecord:") retVoid [argCULong (if value then 1 else 0)]

-- | Declares which user-defined keys should be fetched and added to the resulting @rootRecord.@
--
-- Only consulted if @shouldFetchRootRecord@ is @YES.@  If nil, declares the entire root record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- rootRecordDesiredKeys@
rootRecordDesiredKeys :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Id NSArray)
rootRecordDesiredKeys ckFetchShareMetadataOperation  =
  sendMsg ckFetchShareMetadataOperation (mkSelector "rootRecordDesiredKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Declares which user-defined keys should be fetched and added to the resulting @rootRecord.@
--
-- Only consulted if @shouldFetchRootRecord@ is @YES.@  If nil, declares the entire root record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setRootRecordDesiredKeys:@
setRootRecordDesiredKeys :: (IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation, IsNSArray value) => ckFetchShareMetadataOperation -> value -> IO ()
setRootRecordDesiredKeys ckFetchShareMetadataOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchShareMetadataOperation (mkSelector "setRootRecordDesiredKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Called once for each share URL that the server processed
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- perShareMetadataBlock@
perShareMetadataBlock :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> IO (Ptr ())
perShareMetadataBlock ckFetchShareMetadataOperation  =
  fmap castPtr $ sendMsg ckFetchShareMetadataOperation (mkSelector "perShareMetadataBlock") (retPtr retVoid) []

-- | Called once for each share URL that the server processed
--
-- Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setPerShareMetadataBlock:@
setPerShareMetadataBlock :: IsCKFetchShareMetadataOperation ckFetchShareMetadataOperation => ckFetchShareMetadataOperation -> Ptr () -> IO ()
setPerShareMetadataBlock ckFetchShareMetadataOperation  value =
  sendMsg ckFetchShareMetadataOperation (mkSelector "setPerShareMetadataBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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
fetchShareMetadataCompletionBlock ckFetchShareMetadataOperation  =
  fmap castPtr $ sendMsg ckFetchShareMetadataOperation (mkSelector "fetchShareMetadataCompletionBlock") (retPtr retVoid) []

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
setFetchShareMetadataCompletionBlock ckFetchShareMetadataOperation  value =
  sendMsg ckFetchShareMetadataOperation (mkSelector "setFetchShareMetadataCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithShareURLs:@
initWithShareURLsSelector :: Selector
initWithShareURLsSelector = mkSelector "initWithShareURLs:"

-- | @Selector@ for @shareURLs@
shareURLsSelector :: Selector
shareURLsSelector = mkSelector "shareURLs"

-- | @Selector@ for @setShareURLs:@
setShareURLsSelector :: Selector
setShareURLsSelector = mkSelector "setShareURLs:"

-- | @Selector@ for @shouldFetchRootRecord@
shouldFetchRootRecordSelector :: Selector
shouldFetchRootRecordSelector = mkSelector "shouldFetchRootRecord"

-- | @Selector@ for @setShouldFetchRootRecord:@
setShouldFetchRootRecordSelector :: Selector
setShouldFetchRootRecordSelector = mkSelector "setShouldFetchRootRecord:"

-- | @Selector@ for @rootRecordDesiredKeys@
rootRecordDesiredKeysSelector :: Selector
rootRecordDesiredKeysSelector = mkSelector "rootRecordDesiredKeys"

-- | @Selector@ for @setRootRecordDesiredKeys:@
setRootRecordDesiredKeysSelector :: Selector
setRootRecordDesiredKeysSelector = mkSelector "setRootRecordDesiredKeys:"

-- | @Selector@ for @perShareMetadataBlock@
perShareMetadataBlockSelector :: Selector
perShareMetadataBlockSelector = mkSelector "perShareMetadataBlock"

-- | @Selector@ for @setPerShareMetadataBlock:@
setPerShareMetadataBlockSelector :: Selector
setPerShareMetadataBlockSelector = mkSelector "setPerShareMetadataBlock:"

-- | @Selector@ for @fetchShareMetadataCompletionBlock@
fetchShareMetadataCompletionBlockSelector :: Selector
fetchShareMetadataCompletionBlockSelector = mkSelector "fetchShareMetadataCompletionBlock"

-- | @Selector@ for @setFetchShareMetadataCompletionBlock:@
setFetchShareMetadataCompletionBlockSelector :: Selector
setFetchShareMetadataCompletionBlockSelector = mkSelector "setFetchShareMetadataCompletionBlock:"

