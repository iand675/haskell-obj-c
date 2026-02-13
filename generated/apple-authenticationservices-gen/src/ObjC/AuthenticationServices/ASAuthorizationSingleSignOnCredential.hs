{-# LANGUAGE DataKinds #-}
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
  , privateKeys
  , accessTokenSelector
  , authenticatedResponseSelector
  , authorizedScopesSelector
  , identityTokenSelector
  , initSelector
  , newSelector
  , privateKeysSelector
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

-- | @+ new@
new :: IO (Id ASAuthorizationSingleSignOnCredential)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSingleSignOnCredential"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id ASAuthorizationSingleSignOnCredential)
init_ asAuthorizationSingleSignOnCredential =
  sendOwnedMessage asAuthorizationSingleSignOnCredential initSelector

-- | A state returned from the AuthenticationServices extension.
--
-- ObjC selector: @- state@
state :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSString)
state asAuthorizationSingleSignOnCredential =
  sendMessage asAuthorizationSingleSignOnCredential stateSelector

-- | An access token used to access other systems with the authorized scopes.
--
-- ObjC selector: @- accessToken@
accessToken :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSData)
accessToken asAuthorizationSingleSignOnCredential =
  sendMessage asAuthorizationSingleSignOnCredential accessTokenSelector

-- | A JSON Web Token (JWT) used to communicate information about the identity of the user in a secure way to the app.
--
-- ObjC selector: @- identityToken@
identityToken :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSData)
identityToken asAuthorizationSingleSignOnCredential =
  sendMessage asAuthorizationSingleSignOnCredential identityTokenSelector

-- | This value will contain a list of scopes for which the user provided authorization.  These may contain a subset of the requested scopes on
--
-- See: ASAuthorizationOpenIDRequest.  The application should query this value to identify which scopes were returned as it maybe different from ones requested.
--
-- ObjC selector: @- authorizedScopes@
authorizedScopes :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSArray)
authorizedScopes asAuthorizationSingleSignOnCredential =
  sendMessage asAuthorizationSingleSignOnCredential authorizedScopesSelector

-- | The complete AuthenticationServices extension response with the additional outputs used by the specific technology used by the Authorization Server instance and AuthenticationServices Extension.
--
-- Note: for some operations all properties can be null and the response will indicate just successful result of the operation.
--
-- ObjC selector: @- authenticatedResponse@
authenticatedResponse :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSHTTPURLResponse)
authenticatedResponse asAuthorizationSingleSignOnCredential =
  sendMessage asAuthorizationSingleSignOnCredential authenticatedResponseSelector

-- | Private SecKeys returned from the AuthenticationServices extension.
--
-- ObjC selector: @- privateKeys@
privateKeys :: IsASAuthorizationSingleSignOnCredential asAuthorizationSingleSignOnCredential => asAuthorizationSingleSignOnCredential -> IO (Id NSArray)
privateKeys asAuthorizationSingleSignOnCredential =
  sendMessage asAuthorizationSingleSignOnCredential privateKeysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationSingleSignOnCredential)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationSingleSignOnCredential)
initSelector = mkSelector "init"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSString)
stateSelector = mkSelector "state"

-- | @Selector@ for @accessToken@
accessTokenSelector :: Selector '[] (Id NSData)
accessTokenSelector = mkSelector "accessToken"

-- | @Selector@ for @identityToken@
identityTokenSelector :: Selector '[] (Id NSData)
identityTokenSelector = mkSelector "identityToken"

-- | @Selector@ for @authorizedScopes@
authorizedScopesSelector :: Selector '[] (Id NSArray)
authorizedScopesSelector = mkSelector "authorizedScopes"

-- | @Selector@ for @authenticatedResponse@
authenticatedResponseSelector :: Selector '[] (Id NSHTTPURLResponse)
authenticatedResponseSelector = mkSelector "authenticatedResponse"

-- | @Selector@ for @privateKeys@
privateKeysSelector :: Selector '[] (Id NSArray)
privateKeysSelector = mkSelector "privateKeys"

