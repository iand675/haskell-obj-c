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
  , setAllowedCredentialsSelector
  , appIDSelector
  , setAppIDSelector


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

-- | A list of descriptors indicating credentials that may be used to sign in. If this is non-empty, only credentials matching the provided descriptors can be used when authenticating.
--
-- ObjC selector: @- allowedCredentials@
allowedCredentials :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> IO (Id NSArray)
allowedCredentials asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest (mkSelector "allowedCredentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of descriptors indicating credentials that may be used to sign in. If this is non-empty, only credentials matching the provided descriptors can be used when authenticating.
--
-- ObjC selector: @- setAllowedCredentials:@
setAllowedCredentials :: (IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> value -> IO ()
setAllowedCredentials asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest (mkSelector "setAllowedCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Use this value to request the appid WebAuthn extension. This can only be requested by web browsers.
--
-- ObjC selector: @- appID@
appID :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> IO (Id NSString)
appID asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest (mkSelector "appID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use this value to request the appid WebAuthn extension. This can only be requested by web browsers.
--
-- ObjC selector: @- setAppID:@
setAppID :: (IsASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest, IsNSString value) => asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest -> value -> IO ()
setAppID asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest (mkSelector "setAppID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowedCredentials@
allowedCredentialsSelector :: Selector
allowedCredentialsSelector = mkSelector "allowedCredentials"

-- | @Selector@ for @setAllowedCredentials:@
setAllowedCredentialsSelector :: Selector
setAllowedCredentialsSelector = mkSelector "setAllowedCredentials:"

-- | @Selector@ for @appID@
appIDSelector :: Selector
appIDSelector = mkSelector "appID"

-- | @Selector@ for @setAppID:@
setAppIDSelector :: Selector
setAppIDSelector = mkSelector "setAppID:"

