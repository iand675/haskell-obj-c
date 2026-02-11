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
  , shareURLsSelector
  , setShareURLsSelector
  , perShareAccessRequestCompletionBlockSelector
  , setPerShareAccessRequestCompletionBlockSelector
  , shareRequestAccessCompletionBlockSelector
  , setShareRequestAccessCompletionBlockSelector


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

-- | Creates a new, empty share request access operation.
--
-- ObjC selector: @- init@
init_ :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Id CKShareRequestAccessOperation)
init_ ckShareRequestAccessOperation  =
  sendMsg ckShareRequestAccessOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a share request access operation configured with specified share URLs.
--
-- - Parameter shareURLs: An array of @NSURL@ objects representing the shares to request access to. - Returns: A configured ``CKShareRequestAccessOperation`` instance.
--
-- ObjC selector: @- initWithShareURLs:@
initWithShareURLs :: (IsCKShareRequestAccessOperation ckShareRequestAccessOperation, IsNSArray shareURLs) => ckShareRequestAccessOperation -> shareURLs -> IO (Id CKShareRequestAccessOperation)
initWithShareURLs ckShareRequestAccessOperation  shareURLs =
withObjCPtr shareURLs $ \raw_shareURLs ->
    sendMsg ckShareRequestAccessOperation (mkSelector "initWithShareURLs:") (retPtr retVoid) [argPtr (castPtr raw_shareURLs :: Ptr ())] >>= ownedObject . castPtr

-- | The URLs of the shares to request access to.
--
-- Include multiple URLs to request access to multiple shares simultaneously. The server processes each URL independently.
--
-- ObjC selector: @- shareURLs@
shareURLs :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Id NSArray)
shareURLs ckShareRequestAccessOperation  =
  sendMsg ckShareRequestAccessOperation (mkSelector "shareURLs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URLs of the shares to request access to.
--
-- Include multiple URLs to request access to multiple shares simultaneously. The server processes each URL independently.
--
-- ObjC selector: @- setShareURLs:@
setShareURLs :: (IsCKShareRequestAccessOperation ckShareRequestAccessOperation, IsNSArray value) => ckShareRequestAccessOperation -> value -> IO ()
setShareURLs ckShareRequestAccessOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckShareRequestAccessOperation (mkSelector "setShareURLs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A completion block called once for each processed share URL.
--
-- The server does not disclose share existence to protect user privacy.
--
-- - Parameters:   - shareURL: The URL of the share that was processed.   - shareRequestAccessError: An error describing why the access request failed, or @nil@ if successful.
--
-- ObjC selector: @- perShareAccessRequestCompletionBlock@
perShareAccessRequestCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Ptr ())
perShareAccessRequestCompletionBlock ckShareRequestAccessOperation  =
  fmap castPtr $ sendMsg ckShareRequestAccessOperation (mkSelector "perShareAccessRequestCompletionBlock") (retPtr retVoid) []

-- | A completion block called once for each processed share URL.
--
-- The server does not disclose share existence to protect user privacy.
--
-- - Parameters:   - shareURL: The URL of the share that was processed.   - shareRequestAccessError: An error describing why the access request failed, or @nil@ if successful.
--
-- ObjC selector: @- setPerShareAccessRequestCompletionBlock:@
setPerShareAccessRequestCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> Ptr () -> IO ()
setPerShareAccessRequestCompletionBlock ckShareRequestAccessOperation  value =
  sendMsg ckShareRequestAccessOperation (mkSelector "setPerShareAccessRequestCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | A completion block called when the entire operation finishes.
--
-- - Parameter operationError: An error describing the overall operation failure, or @nil@ if successful.
--
-- If @operationError@ is @CKErrorPartialFailure@, the @userInfo@ dictionary contains detailed errors for each share under ``CKPartialErrorsByItemIDKey``.
--
-- ObjC selector: @- shareRequestAccessCompletionBlock@
shareRequestAccessCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> IO (Ptr ())
shareRequestAccessCompletionBlock ckShareRequestAccessOperation  =
  fmap castPtr $ sendMsg ckShareRequestAccessOperation (mkSelector "shareRequestAccessCompletionBlock") (retPtr retVoid) []

-- | A completion block called when the entire operation finishes.
--
-- - Parameter operationError: An error describing the overall operation failure, or @nil@ if successful.
--
-- If @operationError@ is @CKErrorPartialFailure@, the @userInfo@ dictionary contains detailed errors for each share under ``CKPartialErrorsByItemIDKey``.
--
-- ObjC selector: @- setShareRequestAccessCompletionBlock:@
setShareRequestAccessCompletionBlock :: IsCKShareRequestAccessOperation ckShareRequestAccessOperation => ckShareRequestAccessOperation -> Ptr () -> IO ()
setShareRequestAccessCompletionBlock ckShareRequestAccessOperation  value =
  sendMsg ckShareRequestAccessOperation (mkSelector "setShareRequestAccessCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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

-- | @Selector@ for @perShareAccessRequestCompletionBlock@
perShareAccessRequestCompletionBlockSelector :: Selector
perShareAccessRequestCompletionBlockSelector = mkSelector "perShareAccessRequestCompletionBlock"

-- | @Selector@ for @setPerShareAccessRequestCompletionBlock:@
setPerShareAccessRequestCompletionBlockSelector :: Selector
setPerShareAccessRequestCompletionBlockSelector = mkSelector "setPerShareAccessRequestCompletionBlock:"

-- | @Selector@ for @shareRequestAccessCompletionBlock@
shareRequestAccessCompletionBlockSelector :: Selector
shareRequestAccessCompletionBlockSelector = mkSelector "shareRequestAccessCompletionBlock"

-- | @Selector@ for @setShareRequestAccessCompletionBlock:@
setShareRequestAccessCompletionBlockSelector :: Selector
setShareRequestAccessCompletionBlockSelector = mkSelector "setShareRequestAccessCompletionBlock:"

