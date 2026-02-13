{-# LANGUAGE DataKinds #-}
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
  , nonceSelector
  , requestedOperationSelector
  , requestedScopesSelector
  , setNonceSelector
  , setRequestedOperationSelector
  , setRequestedScopesSelector
  , setStateSelector
  , stateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The contact information to be requested from the user.  Only scopes for which this app was authorized for will be returned.
--
-- ObjC selector: @- requestedScopes@
requestedScopes :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSArray)
requestedScopes asAuthorizationOpenIDRequest =
  sendMessage asAuthorizationOpenIDRequest requestedScopesSelector

-- | The contact information to be requested from the user.  Only scopes for which this app was authorized for will be returned.
--
-- ObjC selector: @- setRequestedScopes:@
setRequestedScopes :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSArray value) => asAuthorizationOpenIDRequest -> value -> IO ()
setRequestedScopes asAuthorizationOpenIDRequest value =
  sendMessage asAuthorizationOpenIDRequest setRequestedScopesSelector (toNSArray value)

-- | State to be passed to the identity provider.  This value will be returned as a part of successful ASAuthorization response.
--
-- Note: The state size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- state@
state :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSString)
state asAuthorizationOpenIDRequest =
  sendMessage asAuthorizationOpenIDRequest stateSelector

-- | State to be passed to the identity provider.  This value will be returned as a part of successful ASAuthorization response.
--
-- Note: The state size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- setState:@
setState :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSString value) => asAuthorizationOpenIDRequest -> value -> IO ()
setState asAuthorizationOpenIDRequest value =
  sendMessage asAuthorizationOpenIDRequest setStateSelector (toNSString value)

-- | Nonce to be passed to the identity provider.  This value can be verified with the identity token provided as a part of successful ASAuthorization response.
--
-- Note: The nonce size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- nonce@
nonce :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSString)
nonce asAuthorizationOpenIDRequest =
  sendMessage asAuthorizationOpenIDRequest nonceSelector

-- | Nonce to be passed to the identity provider.  This value can be verified with the identity token provided as a part of successful ASAuthorization response.
--
-- Note: The nonce size may depend on the actual technology used and an error might be returned by the request execution.
--
-- ObjC selector: @- setNonce:@
setNonce :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSString value) => asAuthorizationOpenIDRequest -> value -> IO ()
setNonce asAuthorizationOpenIDRequest value =
  sendMessage asAuthorizationOpenIDRequest setNonceSelector (toNSString value)

-- | Operation to be executed by the request. The ASAuthorizationOperationImplicit operation interpretation depends on the credential provider implementation.
--
-- ObjC selector: @- requestedOperation@
requestedOperation :: IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest => asAuthorizationOpenIDRequest -> IO (Id NSString)
requestedOperation asAuthorizationOpenIDRequest =
  sendMessage asAuthorizationOpenIDRequest requestedOperationSelector

-- | Operation to be executed by the request. The ASAuthorizationOperationImplicit operation interpretation depends on the credential provider implementation.
--
-- ObjC selector: @- setRequestedOperation:@
setRequestedOperation :: (IsASAuthorizationOpenIDRequest asAuthorizationOpenIDRequest, IsNSString value) => asAuthorizationOpenIDRequest -> value -> IO ()
setRequestedOperation asAuthorizationOpenIDRequest value =
  sendMessage asAuthorizationOpenIDRequest setRequestedOperationSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestedScopes@
requestedScopesSelector :: Selector '[] (Id NSArray)
requestedScopesSelector = mkSelector "requestedScopes"

-- | @Selector@ for @setRequestedScopes:@
setRequestedScopesSelector :: Selector '[Id NSArray] ()
setRequestedScopesSelector = mkSelector "setRequestedScopes:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSString)
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[Id NSString] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @nonce@
nonceSelector :: Selector '[] (Id NSString)
nonceSelector = mkSelector "nonce"

-- | @Selector@ for @setNonce:@
setNonceSelector :: Selector '[Id NSString] ()
setNonceSelector = mkSelector "setNonce:"

-- | @Selector@ for @requestedOperation@
requestedOperationSelector :: Selector '[] (Id NSString)
requestedOperationSelector = mkSelector "requestedOperation"

-- | @Selector@ for @setRequestedOperation:@
setRequestedOperationSelector :: Selector '[Id NSString] ()
setRequestedOperationSelector = mkSelector "setRequestedOperation:"

