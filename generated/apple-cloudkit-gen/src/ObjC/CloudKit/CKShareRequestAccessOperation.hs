{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKShareRequestAccessOperation@.
module ObjC.CloudKit.CKShareRequestAccessOperation
  ( CKShareRequestAccessOperation
  , IsCKShareRequestAccessOperation(..)
  , init_
  , initWithShareURLs
  , shareURLs
  , setShareURLs
  , perShareAccessRequestCompletionBlock
  , setPerShareAccessRequestCompletionBlock
  , shareRequestAccessCompletionBlock
  , setShareRequestAccessCompletionBlock
  , initSelector
  , initWithShareURLsSelector
  , perShareAccessRequestCompletionBlockSelector
  , setPerShareAccessRequestCompletionBlockSelector
  , setShareRequestAccessCompletionBlockSelector
  , setShareURLsSelector
  , shareRequestAccessCompletionBlockSelector
  , shareURLsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new, empty share request access operation.
--
-- ObjC selector: @- init@
init_ :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Id CKShareRequestAccessOperation)
init_ ckShareRequestAccessOperation =
  sendOwnedMessage ckShareRequestAccessOperation initSelector

-- | Creates a share request access operation configured with specified share URLs.
--
-- - Parameter shareURLs: An array of @NSURL@ objects representing the shares to request access to. - Returns: A configured ``CKShareRequestAccessOperation`` instance.
--
-- ObjC selector: @- initWithShareURLs:@
initWithShareURLs :: (IsCKShareRequestAccessOperation ckShareRequestAccessOperation, IsNSArray shareURLs) => ckShareRequestAccessOperation -> shareURLs -> IO (Id CKShareRequestAccessOperation)
initWithShareURLs ckShareRequestAccessOperation shareURLs =
  sendOwnedMessage ckShareRequestAccessOperation initWithShareURLsSelector (toNSArray shareURLs)

-- | The URLs of the shares to request access to.
--
-- Include multiple URLs to request access to multiple shares simultaneously. The server processes each URL independently.
--
-- ObjC selector: @- shareURLs@
shareURLs :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Id NSArray)
shareURLs ckShareRequestAccessOperation =
  sendMessage ckShareRequestAccessOperation shareURLsSelector

-- | The URLs of the shares to request access to.
--
-- Include multiple URLs to request access to multiple shares simultaneously. The server processes each URL independently.
--
-- ObjC selector: @- setShareURLs:@
setShareURLs :: (IsCKShareRequestAccessOperation ckShareRequestAccessOperation, IsNSArray value) => ckShareRequestAccessOperation -> value -> IO ()
setShareURLs ckShareRequestAccessOperation value =
  sendMessage ckShareRequestAccessOperation setShareURLsSelector (toNSArray value)

-- | A completion block called once for each processed share URL.
--
-- The server does not disclose share existence to protect user privacy.
--
-- - Parameters:   - shareURL: The URL of the share that was processed.   - shareRequestAccessError: An error describing why the access request failed, or @nil@ if successful.
--
-- ObjC selector: @- perShareAccessRequestCompletionBlock@
perShareAccessRequestCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Ptr ())
perShareAccessRequestCompletionBlock ckShareRequestAccessOperation =
  sendMessage ckShareRequestAccessOperation perShareAccessRequestCompletionBlockSelector

-- | A completion block called once for each processed share URL.
--
-- The server does not disclose share existence to protect user privacy.
--
-- - Parameters:   - shareURL: The URL of the share that was processed.   - shareRequestAccessError: An error describing why the access request failed, or @nil@ if successful.
--
-- ObjC selector: @- setPerShareAccessRequestCompletionBlock:@
setPerShareAccessRequestCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> Ptr () -> IO ()
setPerShareAccessRequestCompletionBlock ckShareRequestAccessOperation value =
  sendMessage ckShareRequestAccessOperation setPerShareAccessRequestCompletionBlockSelector value

-- | A completion block called when the entire operation finishes.
--
-- - Parameter operationError: An error describing the overall operation failure, or @nil@ if successful.
--
-- If @operationError@ is @CKErrorPartialFailure@, the @userInfo@ dictionary contains detailed errors for each share under ``CKPartialErrorsByItemIDKey``.
--
-- ObjC selector: @- shareRequestAccessCompletionBlock@
shareRequestAccessCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Ptr ())
shareRequestAccessCompletionBlock ckShareRequestAccessOperation =
  sendMessage ckShareRequestAccessOperation shareRequestAccessCompletionBlockSelector

-- | A completion block called when the entire operation finishes.
--
-- - Parameter operationError: An error describing the overall operation failure, or @nil@ if successful.
--
-- If @operationError@ is @CKErrorPartialFailure@, the @userInfo@ dictionary contains detailed errors for each share under ``CKPartialErrorsByItemIDKey``.
--
-- ObjC selector: @- setShareRequestAccessCompletionBlock:@
setShareRequestAccessCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> Ptr () -> IO ()
setShareRequestAccessCompletionBlock ckShareRequestAccessOperation value =
  sendMessage ckShareRequestAccessOperation setShareRequestAccessCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKShareRequestAccessOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithShareURLs:@
initWithShareURLsSelector :: Selector '[Id NSArray] (Id CKShareRequestAccessOperation)
initWithShareURLsSelector = mkSelector "initWithShareURLs:"

-- | @Selector@ for @shareURLs@
shareURLsSelector :: Selector '[] (Id NSArray)
shareURLsSelector = mkSelector "shareURLs"

-- | @Selector@ for @setShareURLs:@
setShareURLsSelector :: Selector '[Id NSArray] ()
setShareURLsSelector = mkSelector "setShareURLs:"

-- | @Selector@ for @perShareAccessRequestCompletionBlock@
perShareAccessRequestCompletionBlockSelector :: Selector '[] (Ptr ())
perShareAccessRequestCompletionBlockSelector = mkSelector "perShareAccessRequestCompletionBlock"

-- | @Selector@ for @setPerShareAccessRequestCompletionBlock:@
setPerShareAccessRequestCompletionBlockSelector :: Selector '[Ptr ()] ()
setPerShareAccessRequestCompletionBlockSelector = mkSelector "setPerShareAccessRequestCompletionBlock:"

-- | @Selector@ for @shareRequestAccessCompletionBlock@
shareRequestAccessCompletionBlockSelector :: Selector '[] (Ptr ())
shareRequestAccessCompletionBlockSelector = mkSelector "shareRequestAccessCompletionBlock"

-- | @Selector@ for @setShareRequestAccessCompletionBlock:@
setShareRequestAccessCompletionBlockSelector :: Selector '[Ptr ()] ()
setShareRequestAccessCompletionBlockSelector = mkSelector "setShareRequestAccessCompletionBlock:"

