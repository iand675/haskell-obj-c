{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationOpenIDRequest@.
module ObjC.AuthenticationServices.ASAuthorizationOpenIDRequest
  ( ASAuthorizationOpenIDRequest
  , IsASAuthorizationOpenIDRequest(..)
  , requestedScopes
  , setRequestedScopes
  , state
  , setState
  , nonce
  , setNonce
  , requestedOperation
  , setRequestedOperation
  , requestedScopesSelector
  , setRequestedScopesSelector
  , stateSelector
  , setStateSelector
  , nonceSelector
  , setNonceSelector
  , requestedOperationSelector
  , setRequestedOperationSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The contact information to be requested from the user.  Only scopes for which this app was authorized for will be returned.
--
-- ObjC selector: @- requestedScopes@
requestedScopes :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSArray)
requestedScopes asAuthorizationOpenIDRequest  =
  sendMsg asAuthorizationOpenIDRequest (mkSelector "requestedScopes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The contact information to be requested from the user.  Only scopes for which this app was authorized for will be returned.
--
-- ObjC selector: @- setRequestedScopes:@
setRequestedScopes :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSArray value) => asAuthorizationOpenIDRequest -> value -> IO ()
setRequestedScopes asAuthorizationOpenIDRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationOpenIDRequest (mkSelector "setRequestedScopes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | State to be passed to the identity provider.  This value will be returned as a part of successful ASAuthorization response.
--
-- Note: The state size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- state@
state :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSString)
state asAuthorizationOpenIDRequest  =
  sendMsg asAuthorizationOpenIDRequest (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | State to be passed to the identity provider.  This value will be returned as a part of successful ASAuthorization response.
--
-- Note: The state size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- setState:@
setState :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSString value) => asAuthorizationOpenIDRequest -> value -> IO ()
setState asAuthorizationOpenIDRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationOpenIDRequest (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Nonce to be passed to the identity provider.  This value can be verified with the identity token provided as a part of successful ASAuthorization response.
--
-- Note: The nonce size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- nonce@
nonce :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSString)
nonce asAuthorizationOpenIDRequest  =
  sendMsg asAuthorizationOpenIDRequest (mkSelector "nonce") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Nonce to be passed to the identity provider.  This value can be verified with the identity token provided as a part of successful ASAuthorization response.
--
-- Note: The nonce size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- setNonce:@
setNonce :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSString value) => asAuthorizationOpenIDRequest -> value -> IO ()
setNonce asAuthorizationOpenIDRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationOpenIDRequest (mkSelector "setNonce:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Operation to be executed by the request. The ASAuthorizationOperationImplicit operation interpretation depends on the credential provider implementation.
--
-- ObjC selector: @- requestedOperation@
requestedOperation :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSString)
requestedOperation asAuthorizationOpenIDRequest  =
  sendMsg asAuthorizationOpenIDRequest (mkSelector "requestedOperation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Operation to be executed by the request. The ASAuthorizationOperationImplicit operation interpretation depends on the credential provider implementation.
--
-- ObjC selector: @- setRequestedOperation:@
setRequestedOperation :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSString value) => asAuthorizationOpenIDRequest -> value -> IO ()
setRequestedOperation asAuthorizationOpenIDRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationOpenIDRequest (mkSelector "setRequestedOperation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestedScopes@
requestedScopesSelector :: Selector
requestedScopesSelector = mkSelector "requestedScopes"

-- | @Selector@ for @setRequestedScopes:@
setRequestedScopesSelector :: Selector
setRequestedScopesSelector = mkSelector "setRequestedScopes:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @nonce@
nonceSelector :: Selector
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @requestedOperation@
requestedOperationSelector :: Selector
requestedOperationSelector = mkSelector "requestedOperation"

-- | @Selector@ for @setRequestedOperation:@
setRequestedOperationSelector :: Selector
setRequestedOperationSelector = mkSelector "setRequestedOperation:"

