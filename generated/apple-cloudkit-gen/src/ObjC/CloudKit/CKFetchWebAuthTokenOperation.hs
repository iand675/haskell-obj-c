{-# LANGUAGE DataKinds #-}
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
  , apiTokenSelector
  , fetchWebAuthTokenCompletionBlockSelector
  , initSelector
  , initWithAPITokenSelector
  , setAPITokenSelector
  , setFetchWebAuthTokenCompletionBlockSelector


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
init_ :: IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation => ckFetchWebAuthTokenOperation -> IO (Id CKFetchWebAuthTokenOperation)
init_ ckFetchWebAuthTokenOperation =
  sendOwnedMessage ckFetchWebAuthTokenOperation initSelector

-- | @- initWithAPIToken:@
initWithAPIToken :: (IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation, IsNSString apiToken) => ckFetchWebAuthTokenOperation -> apiToken -> IO (Id CKFetchWebAuthTokenOperation)
initWithAPIToken ckFetchWebAuthTokenOperation apiToken =
  sendOwnedMessage ckFetchWebAuthTokenOperation initWithAPITokenSelector (toNSString apiToken)

-- | APIToken is expected to be set before you begin this operation.
--
-- ObjC selector: @- APIToken@
apiToken :: IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation => ckFetchWebAuthTokenOperation -> IO (Id NSString)
apiToken ckFetchWebAuthTokenOperation =
  sendMessage ckFetchWebAuthTokenOperation apiTokenSelector

-- | APIToken is expected to be set before you begin this operation.
--
-- ObjC selector: @- setAPIToken:@
setAPIToken :: (IsCKFetchWebAuthTokenOperation ckFetchWebAuthTokenOperation, IsNSString value) => ckFetchWebAuthTokenOperation -> value -> IO ()
setAPIToken ckFetchWebAuthTokenOperation value =
  sendMessage ckFetchWebAuthTokenOperation setAPITokenSelector (toNSString value)

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
fetchWebAuthTokenCompletionBlock ckFetchWebAuthTokenOperation =
  sendMessage ckFetchWebAuthTokenOperation fetchWebAuthTokenCompletionBlockSelector

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
setFetchWebAuthTokenCompletionBlock ckFetchWebAuthTokenOperation value =
  sendMessage ckFetchWebAuthTokenOperation setFetchWebAuthTokenCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchWebAuthTokenOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAPIToken:@
initWithAPITokenSelector :: Selector '[Id NSString] (Id CKFetchWebAuthTokenOperation)
initWithAPITokenSelector = mkSelector "initWithAPIToken:"

-- | @Selector@ for @APIToken@
apiTokenSelector :: Selector '[] (Id NSString)
apiTokenSelector = mkSelector "APIToken"

-- | @Selector@ for @setAPIToken:@
setAPITokenSelector :: Selector '[Id NSString] ()
setAPITokenSelector = mkSelector "setAPIToken:"

-- | @Selector@ for @fetchWebAuthTokenCompletionBlock@
fetchWebAuthTokenCompletionBlockSelector :: Selector '[] (Ptr ())
fetchWebAuthTokenCompletionBlockSelector = mkSelector "fetchWebAuthTokenCompletionBlock"

-- | @Selector@ for @setFetchWebAuthTokenCompletionBlock:@
setFetchWebAuthTokenCompletionBlockSelector :: Selector '[Ptr ()] ()
setFetchWebAuthTokenCompletionBlockSelector = mkSelector "setFetchWebAuthTokenCompletionBlock:"

