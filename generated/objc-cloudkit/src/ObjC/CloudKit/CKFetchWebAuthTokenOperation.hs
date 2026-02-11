{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKFetchWebAuthTokenOperation
--
-- This operation will fetch a web auth token given an API token obtained from the CloudKit Dashboard for your container
--
-- Generated bindings for @CKFetchWebAuthTokenOperation@.
module ObjC.CloudKit.CKFetchWebAuthTokenOperation
  ( CKFetchWebAuthTokenOperation
  , IsCKFetchWebAuthTokenOperation(..)
  , init_
  , initWithAPIToken
  , apiToken
  , setAPIToken
  , fetchWebAuthTokenCompletionBlock
  , setFetchWebAuthTokenCompletionBlock
  , initSelector
  , initWithAPITokenSelector
  , apiTokenSelector
  , setAPITokenSelector
  , fetchWebAuthTokenCompletionBlockSelector
  , setFetchWebAuthTokenCompletionBlockSelector


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
init_ :: IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation => ckFetchWebAuthTokenOperation -> IO (Id CKFetchWebAuthTokenOperation)
init_ ckFetchWebAuthTokenOperation  =
  sendMsg ckFetchWebAuthTokenOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithAPIToken:@
initWithAPIToken :: (IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation, IsNSString apiToken) => ckFetchWebAuthTokenOperation -> apiToken -> IO (Id CKFetchWebAuthTokenOperation)
initWithAPIToken ckFetchWebAuthTokenOperation  apiToken =
withObjCPtr apiToken $ \raw_apiToken ->
    sendMsg ckFetchWebAuthTokenOperation (mkSelector "initWithAPIToken:") (retPtr retVoid) [argPtr (castPtr raw_apiToken :: Ptr ())] >>= ownedObject . castPtr

-- | APIToken is expected to be set before you begin this operation.
--
-- ObjC selector: @- APIToken@
apiToken :: IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation => ckFetchWebAuthTokenOperation -> IO (Id NSString)
apiToken ckFetchWebAuthTokenOperation  =
  sendMsg ckFetchWebAuthTokenOperation (mkSelector "APIToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | APIToken is expected to be set before you begin this operation.
--
-- ObjC selector: @- setAPIToken:@
setAPIToken :: (IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation, IsNSString value) => ckFetchWebAuthTokenOperation -> value -> IO ()
setAPIToken ckFetchWebAuthTokenOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchWebAuthTokenOperation (mkSelector "setAPIToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchWebAuthTokenCompletionBlock@
fetchWebAuthTokenCompletionBlock :: IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation => ckFetchWebAuthTokenOperation -> IO (Ptr ())
fetchWebAuthTokenCompletionBlock ckFetchWebAuthTokenOperation  =
  fmap castPtr $ sendMsg ckFetchWebAuthTokenOperation (mkSelector "fetchWebAuthTokenCompletionBlock") (retPtr retVoid) []

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchWebAuthTokenCompletionBlock:@
setFetchWebAuthTokenCompletionBlock :: IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation => ckFetchWebAuthTokenOperation -> Ptr () -> IO ()
setFetchWebAuthTokenCompletionBlock ckFetchWebAuthTokenOperation  value =
  sendMsg ckFetchWebAuthTokenOperation (mkSelector "setFetchWebAuthTokenCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAPIToken:@
initWithAPITokenSelector :: Selector
initWithAPITokenSelector = mkSelector "initWithAPIToken:"

-- | @Selector@ for @APIToken@
apiTokenSelector :: Selector
apiTokenSelector = mkSelector "APIToken"

-- | @Selector@ for @setAPIToken:@
setAPITokenSelector :: Selector
setAPITokenSelector = mkSelector "setAPIToken:"

-- | @Selector@ for @fetchWebAuthTokenCompletionBlock@
fetchWebAuthTokenCompletionBlockSelector :: Selector
fetchWebAuthTokenCompletionBlockSelector = mkSelector "fetchWebAuthTokenCompletionBlock"

-- | @Selector@ for @setFetchWebAuthTokenCompletionBlock:@
setFetchWebAuthTokenCompletionBlockSelector :: Selector
setFetchWebAuthTokenCompletionBlockSelector = mkSelector "setFetchWebAuthTokenCompletionBlock:"

