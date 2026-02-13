{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A VSAccountManager instance coordinates access to a subscriber's account.
--
-- Generated bindings for @VSAccountManager@.
module ObjC.VideoSubscriberAccount.VSAccountManager
  ( VSAccountManager
  , IsVSAccountManager(..)
  , checkAccessStatusWithOptions_completionHandler
  , enqueueAccountMetadataRequest_completionHandler
  , delegate
  , setDelegate
  , checkAccessStatusWithOptions_completionHandlerSelector
  , delegateSelector
  , enqueueAccountMetadataRequest_completionHandlerSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Determine the state of the application's access to the user's subscription information.
--
-- @options@ — The only currently supported option key is VSCheckAccessOptionPrompt.
--
-- @completionHandler@ — A block to be called when the request finishes.  It will always be called exactly once.  It may be called before the method call returns.  It may be called on any queue.
--
-- @accessStatus@ — The current state the application's access to the user's subscription information.
--
-- @error@ — If the user did not grant access to the app, this will contain an error describing the result of the operation.
--
-- ObjC selector: @- checkAccessStatusWithOptions:completionHandler:@
checkAccessStatusWithOptions_completionHandler :: (IsVSAccountManager vsAccountManager, IsNSDictionary options) => vsAccountManager -> options -> Ptr () -> IO ()
checkAccessStatusWithOptions_completionHandler vsAccountManager options completionHandler =
  sendMessage vsAccountManager checkAccessStatusWithOptions_completionHandlerSelector (toNSDictionary options) completionHandler

-- | Begins requesting information about the subscriber's account.
--
-- @request@ — This identifies what specific information the app wants to know.
--
-- @completionHandler@ — A block to be called when the request finishes.  It will always be called exactly once.  It may be called before the method call returns.  It may be called on any queue.
--
-- @metadata@ — If the request finished successfully, this will contain information about the subscriber's account.
--
-- @error@ — If the request did not finish successfully, this will contain an error describing the result of the operation.
--
-- A result object that may be used to cancel the in-flight request.  Cancellation is advisory, and does not guarantee that the request will finish immediately.
--
-- ObjC selector: @- enqueueAccountMetadataRequest:completionHandler:@
enqueueAccountMetadataRequest_completionHandler :: (IsVSAccountManager vsAccountManager, IsVSAccountMetadataRequest request) => vsAccountManager -> request -> Ptr () -> IO (Id VSAccountManagerResult)
enqueueAccountMetadataRequest_completionHandler vsAccountManager request completionHandler =
  sendMessage vsAccountManager enqueueAccountMetadataRequest_completionHandlerSelector (toVSAccountMetadataRequest request) completionHandler

-- | An object that can help the account manager by presenting and dismissing view controllers when needed, and deciding whether to allow authentication with the selected provider. Some requests may fail if a delegate is not provided.  For example, an account metadata request may require a delegate if it allows interruption.
--
-- ObjC selector: @- delegate@
delegate :: IsVSAccountManager vsAccountManager => vsAccountManager -> IO RawId
delegate vsAccountManager =
  sendMessage vsAccountManager delegateSelector

-- | An object that can help the account manager by presenting and dismissing view controllers when needed, and deciding whether to allow authentication with the selected provider. Some requests may fail if a delegate is not provided.  For example, an account metadata request may require a delegate if it allows interruption.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsVSAccountManager vsAccountManager => vsAccountManager -> RawId -> IO ()
setDelegate vsAccountManager value =
  sendMessage vsAccountManager setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkAccessStatusWithOptions:completionHandler:@
checkAccessStatusWithOptions_completionHandlerSelector :: Selector '[Id NSDictionary, Ptr ()] ()
checkAccessStatusWithOptions_completionHandlerSelector = mkSelector "checkAccessStatusWithOptions:completionHandler:"

-- | @Selector@ for @enqueueAccountMetadataRequest:completionHandler:@
enqueueAccountMetadataRequest_completionHandlerSelector :: Selector '[Id VSAccountMetadataRequest, Ptr ()] (Id VSAccountManagerResult)
enqueueAccountMetadataRequest_completionHandlerSelector = mkSelector "enqueueAccountMetadataRequest:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

