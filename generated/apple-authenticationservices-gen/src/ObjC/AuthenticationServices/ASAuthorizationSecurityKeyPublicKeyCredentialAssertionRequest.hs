{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest@.
module ObjC.AuthenticationServices.ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest
  ( ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest
  , IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest(..)
  , allowedCredentials
  , setAllowedCredentials
  , appID
  , setAppID
  , allowedCredentialsSelector
  , appIDSelector
  , setAllowedCredentialsSelector
  , setAppIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A list of descriptors indicating credentials that may be used to sign in. If this is non-empty, only credentials matching the provided descriptors can be used when authenticating.
--
-- ObjC selector: @- allowedCredentials@
allowedCredentials :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> IO (Id NSArray)
allowedCredentials asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest allowedCredentialsSelector

-- | A list of descriptors indicating credentials that may be used to sign in. If this is non-empty, only credentials matching the provided descriptors can be used when authenticating.
--
-- ObjC selector: @- setAllowedCredentials:@
setAllowedCredentials :: (IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> value -> IO ()
setAllowedCredentials asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest value =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest setAllowedCredentialsSelector (toNSArray value)

-- | Use this value to request the appid WebAuthn extension. This can only be requested by web browsers.
--
-- ObjC selector: @- appID@
appID :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> IO (Id NSString)
appID asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest appIDSelector

-- | Use this value to request the appid WebAuthn extension. This can only be requested by web browsers.
--
-- ObjC selector: @- setAppID:@
setAppID :: (IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest, IsNSString value) => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> value -> IO ()
setAppID asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest value =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest setAppIDSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowedCredentials@
allowedCredentialsSelector :: Selector '[] (Id NSArray)
allowedCredentialsSelector = mkSelector "allowedCredentials"

-- | @Selector@ for @setAllowedCredentials:@
setAllowedCredentialsSelector :: Selector '[Id NSArray] ()
setAllowedCredentialsSelector = mkSelector "setAllowedCredentials:"

-- | @Selector@ for @appID@
appIDSelector :: Selector '[] (Id NSString)
appIDSelector = mkSelector "appID"

-- | @Selector@ for @setAppID:@
setAppIDSelector :: Selector '[Id NSString] ()
setAppIDSelector = mkSelector "setAppID:"

