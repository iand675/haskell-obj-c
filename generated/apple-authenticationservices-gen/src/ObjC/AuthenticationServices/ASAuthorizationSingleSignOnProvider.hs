{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSingleSignOnProvider@.
module ObjC.AuthenticationServices.ASAuthorizationSingleSignOnProvider
  ( ASAuthorizationSingleSignOnProvider
  , IsASAuthorizationSingleSignOnProvider(..)
  , authorizationProviderWithIdentityProviderURL
  , createRequest
  , new
  , init_
  , url
  , canPerformAuthorization
  , authorizationProviderWithIdentityProviderURLSelector
  , canPerformAuthorizationSelector
  , createRequestSelector
  , initSelector
  , newSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | To get the right extension the identity provider main URL has to be provided. The URL is even part of the extension using assosiated domains mechanism or can be configured by MDM profile.
--
-- ObjC selector: @+ authorizationProviderWithIdentityProviderURL:@
authorizationProviderWithIdentityProviderURL :: IsNSURL url => url -> IO (Id ASAuthorizationSingleSignOnProvider)
authorizationProviderWithIdentityProviderURL url =
  do
    cls' <- getRequiredClass "ASAuthorizationSingleSignOnProvider"
    sendClassMessage cls' authorizationProviderWithIdentityProviderURLSelector (toNSURL url)

-- | @- createRequest@
createRequest :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO (Id ASAuthorizationSingleSignOnRequest)
createRequest asAuthorizationSingleSignOnProvider =
  sendMessage asAuthorizationSingleSignOnProvider createRequestSelector

-- | @+ new@
new :: IO (Id ASAuthorizationSingleSignOnProvider)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSingleSignOnProvider"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO (Id ASAuthorizationSingleSignOnProvider)
init_ asAuthorizationSingleSignOnProvider =
  sendOwnedMessage asAuthorizationSingleSignOnProvider initSelector

-- | @- url@
url :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO (Id NSURL)
url asAuthorizationSingleSignOnProvider =
  sendMessage asAuthorizationSingleSignOnProvider urlSelector

-- | Returns YES if the configured provider is capable of performing authorization within a given configuration.
--
-- ObjC selector: @- canPerformAuthorization@
canPerformAuthorization :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO Bool
canPerformAuthorization asAuthorizationSingleSignOnProvider =
  sendMessage asAuthorizationSingleSignOnProvider canPerformAuthorizationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationProviderWithIdentityProviderURL:@
authorizationProviderWithIdentityProviderURLSelector :: Selector '[Id NSURL] (Id ASAuthorizationSingleSignOnProvider)
authorizationProviderWithIdentityProviderURLSelector = mkSelector "authorizationProviderWithIdentityProviderURL:"

-- | @Selector@ for @createRequest@
createRequestSelector :: Selector '[] (Id ASAuthorizationSingleSignOnRequest)
createRequestSelector = mkSelector "createRequest"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationSingleSignOnProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationSingleSignOnProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @canPerformAuthorization@
canPerformAuthorizationSelector :: Selector '[] Bool
canPerformAuthorizationSelector = mkSelector "canPerformAuthorization"

