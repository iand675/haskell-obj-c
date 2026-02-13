{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Used to request information from an identity document stored as a Wallet pass.
--
-- Generated bindings for @PKIdentityAuthorizationController@.
module ObjC.PassKit.PKIdentityAuthorizationController
  ( PKIdentityAuthorizationController
  , IsPKIdentityAuthorizationController(..)
  , checkCanRequestDocument_completion
  , requestDocument_completion
  , cancelRequest
  , cancelRequestSelector
  , checkCanRequestDocument_completionSelector
  , requestDocument_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Determines if a document can be requested, taking into account the entitlement of the calling process as well as the state of this device.
--
-- ObjC selector: @- checkCanRequestDocument:completion:@
checkCanRequestDocument_completion :: IsPKIdentityAuthorizationController pkIdentityAuthorizationController => pkIdentityAuthorizationController -> RawId -> Ptr () -> IO ()
checkCanRequestDocument_completion pkIdentityAuthorizationController descriptor completion =
  sendMessage pkIdentityAuthorizationController checkCanRequestDocument_completionSelector descriptor completion

-- | Requests identity document information from the user. The user will be prompted to approve the request before any data is released. If the user approves, the document will be returned through the completion handler. If the user does not approve, PKIdentityErrorUserCancelled will be returned through the completion handler. If the request is cancelled by the calling app through cancelRequest, PKIdentityErrorAppCancelled will be returned. Only one request can be in progress at a time, otherwise PKIdentityErrorRequestAlreadyInProgress will be returned.
--
-- ObjC selector: @- requestDocument:completion:@
requestDocument_completion :: (IsPKIdentityAuthorizationController pkIdentityAuthorizationController, IsPKIdentityRequest request) => pkIdentityAuthorizationController -> request -> Ptr () -> IO ()
requestDocument_completion pkIdentityAuthorizationController request completion =
  sendMessage pkIdentityAuthorizationController requestDocument_completionSelector (toPKIdentityRequest request) completion

-- | If there is a request in progress through requestDocument, this will cancel that request if possible. If the request is cancelled, PKIdentityErrorAppCancelled will be returned in the requestDocument:completion: completion handler. Cancellation is not guaranteed; even if this method is called, it is possible that requestDocument:completion: will return a document response if a response was already in flight.
--
-- ObjC selector: @- cancelRequest@
cancelRequest :: IsPKIdentityAuthorizationController pkIdentityAuthorizationController => pkIdentityAuthorizationController -> IO ()
cancelRequest pkIdentityAuthorizationController =
  sendMessage pkIdentityAuthorizationController cancelRequestSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @checkCanRequestDocument:completion:@
checkCanRequestDocument_completionSelector :: Selector '[RawId, Ptr ()] ()
checkCanRequestDocument_completionSelector = mkSelector "checkCanRequestDocument:completion:"

-- | @Selector@ for @requestDocument:completion:@
requestDocument_completionSelector :: Selector '[Id PKIdentityRequest, Ptr ()] ()
requestDocument_completionSelector = mkSelector "requestDocument:completion:"

-- | @Selector@ for @cancelRequest@
cancelRequestSelector :: Selector '[] ()
cancelRequestSelector = mkSelector "cancelRequest"

