{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSingleSignOnCredential@.
module ObjC.AuthenticationServices.ASAuthorizationSingleSignOnCredential
  ( ASAuthorizationSingleSignOnCredential
  , IsASAuthorizationSingleSignOnCredential(..)
  , new
  , init_
  , state
  , accessToken
  , identityToken
  , authorizedScopes
  , authenticatedResponse
  , newSelector
  , initSelector
  , stateSelector
  , accessTokenSelector
  , identityTokenSelector
  , authorizedScopesSelector
  , authenticatedResponseSelector


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

-- | @+ new@
new :: IO (Id ASAuthorizationSingleSignOnCredential)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSingleSignOnCredential"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id ASAuthorizationSingleSignOnCredential)
init_ asAuthorizationSingleSignOnCredential  =
  sendMsg asAuthorizationSingleSignOnCredential (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A state returned from the AuthenticationServices extension.
--
-- ObjC selector: @- state@
state :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSString)
state asAuthorizationSingleSignOnCredential  =
  sendMsg asAuthorizationSingleSignOnCredential (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An access token used to access other systems with the authorized scopes.
--
-- ObjC selector: @- accessToken@
accessToken :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSData)
accessToken asAuthorizationSingleSignOnCredential  =
  sendMsg asAuthorizationSingleSignOnCredential (mkSelector "accessToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A JSON Web Token (JWT) used to communicate information about the identity of the user in a secure way to the app.
--
-- ObjC selector: @- identityToken@
identityToken :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSData)
identityToken asAuthorizationSingleSignOnCredential  =
  sendMsg asAuthorizationSingleSignOnCredential (mkSelector "identityToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This value will contain a list of scopes for which the user provided authorization.  These may contain a subset of the requested scopes on
--
-- See: ASAuthorizationOpenIDRequest.  The application should query this value to identify which scopes were returned as it maybe different from ones requested.
--
-- ObjC selector: @- authorizedScopes@
authorizedScopes :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSArray)
authorizedScopes asAuthorizationSingleSignOnCredential  =
  sendMsg asAuthorizationSingleSignOnCredential (mkSelector "authorizedScopes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The complete AuthenticationServices extension response with the additional outputs used by the specific technology used by the Authorization Server instance and AuthenticationServices Extension.
--
-- Note: for some operations all properties can be null and the response will indicate just successful result of the operation.
--
-- ObjC selector: @- authenticatedResponse@
authenticatedResponse :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSHTTPURLResponse)
authenticatedResponse asAuthorizationSingleSignOnCredential  =
  sendMsg asAuthorizationSingleSignOnCredential (mkSelector "authenticatedResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @accessToken@
accessTokenSelector :: Selector
accessTokenSelector = mkSelector "accessToken"

-- | @Selector@ for @identityToken@
identityTokenSelector :: Selector
identityTokenSelector = mkSelector "identityToken"

-- | @Selector@ for @authorizedScopes@
authorizedScopesSelector :: Selector
authorizedScopesSelector = mkSelector "authorizedScopes"

-- | @Selector@ for @authenticatedResponse@
authenticatedResponseSelector :: Selector
authenticatedResponseSelector = mkSelector "authenticatedResponse"

