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
  , createRequestSelector
  , newSelector
  , initSelector
  , urlSelector
  , canPerformAuthorizationSelector


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

-- | To get the right extension the identity provider main URL has to be provided. The URL is even part of the extension using assosiated domains mechanism or can be configured by MDM profile.
--
-- ObjC selector: @+ authorizationProviderWithIdentityProviderURL:@
authorizationProviderWithIdentityProviderURL :: IsNSURL url => url -> IO (Id ASAuthorizationSingleSignOnProvider)
authorizationProviderWithIdentityProviderURL url =
  do
    cls' <- getRequiredClass "ASAuthorizationSingleSignOnProvider"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "authorizationProviderWithIdentityProviderURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- createRequest@
createRequest :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO (Id ASAuthorizationSingleSignOnRequest)
createRequest asAuthorizationSingleSignOnProvider  =
  sendMsg asAuthorizationSingleSignOnProvider (mkSelector "createRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationSingleSignOnProvider)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSingleSignOnProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO (Id ASAuthorizationSingleSignOnProvider)
init_ asAuthorizationSingleSignOnProvider  =
  sendMsg asAuthorizationSingleSignOnProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- url@
url :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO (Id NSURL)
url asAuthorizationSingleSignOnProvider  =
  sendMsg asAuthorizationSingleSignOnProvider (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns YES if the configured provider is capable of performing authorization within a given configuration.
--
-- ObjC selector: @- canPerformAuthorization@
canPerformAuthorization :: IsASAuthorizationSingleSignOnProvider asAuthorizationSingleSignOnProvider => asAuthorizationSingleSignOnProvider -> IO Bool
canPerformAuthorization asAuthorizationSingleSignOnProvider  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationSingleSignOnProvider (mkSelector "canPerformAuthorization") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationProviderWithIdentityProviderURL:@
authorizationProviderWithIdentityProviderURLSelector :: Selector
authorizationProviderWithIdentityProviderURLSelector = mkSelector "authorizationProviderWithIdentityProviderURL:"

-- | @Selector@ for @createRequest@
createRequestSelector :: Selector
createRequestSelector = mkSelector "createRequest"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @canPerformAuthorization@
canPerformAuthorizationSelector :: Selector
canPerformAuthorizationSelector = mkSelector "canPerformAuthorization"

