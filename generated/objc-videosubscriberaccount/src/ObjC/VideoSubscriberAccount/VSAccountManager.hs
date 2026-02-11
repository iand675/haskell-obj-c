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
  , checkAccessStatusWithOptions_completionHandlerSelector
  , enqueueAccountMetadataRequest_completionHandlerSelector


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
checkAccessStatusWithOptions_completionHandler vsAccountManager  options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vsAccountManager (mkSelector "checkAccessStatusWithOptions:completionHandler:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
enqueueAccountMetadataRequest_completionHandler vsAccountManager  request completionHandler =
withObjCPtr request $ \raw_request ->
    sendMsg vsAccountManager (mkSelector "enqueueAccountMetadataRequest:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkAccessStatusWithOptions:completionHandler:@
checkAccessStatusWithOptions_completionHandlerSelector :: Selector
checkAccessStatusWithOptions_completionHandlerSelector = mkSelector "checkAccessStatusWithOptions:completionHandler:"

-- | @Selector@ for @enqueueAccountMetadataRequest:completionHandler:@
enqueueAccountMetadataRequest_completionHandlerSelector :: Selector
enqueueAccountMetadataRequest_completionHandlerSelector = mkSelector "enqueueAccountMetadataRequest:completionHandler:"

