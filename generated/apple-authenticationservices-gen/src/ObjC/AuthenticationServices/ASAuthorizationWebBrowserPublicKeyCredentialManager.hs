{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationWebBrowserPublicKeyCredentialManager@.
module ObjC.AuthenticationServices.ASAuthorizationWebBrowserPublicKeyCredentialManager
  ( ASAuthorizationWebBrowserPublicKeyCredentialManager
  , IsASAuthorizationWebBrowserPublicKeyCredentialManager(..)
  , init_
  , requestAuthorizationForPublicKeyCredentials
  , isDeviceConfiguredForPasskeys
  , authorizationStateForPlatformCredentials
  , authorizationStateForPlatformCredentialsSelector
  , initSelector
  , isDeviceConfiguredForPasskeysSelector
  , requestAuthorizationForPublicKeyCredentialsSelector

  -- * Enum types
  , ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState(ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState)
  , pattern ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateAuthorized
  , pattern ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateDenied
  , pattern ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateNotDetermined

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASAuthorizationWebBrowserPublicKeyCredentialManager asAuthorizationWebBrowserPublicKeyCredentialManager => asAuthorizationWebBrowserPublicKeyCredentialManager -> IO (Id ASAuthorizationWebBrowserPublicKeyCredentialManager)
init_ asAuthorizationWebBrowserPublicKeyCredentialManager =
  sendOwnedMessage asAuthorizationWebBrowserPublicKeyCredentialManager initSelector

-- | @- requestAuthorizationForPublicKeyCredentials:@
requestAuthorizationForPublicKeyCredentials :: IsASAuthorizationWebBrowserPublicKeyCredentialManager asAuthorizationWebBrowserPublicKeyCredentialManager => asAuthorizationWebBrowserPublicKeyCredentialManager -> Ptr () -> IO ()
requestAuthorizationForPublicKeyCredentials asAuthorizationWebBrowserPublicKeyCredentialManager completionHandler =
  sendMessage asAuthorizationWebBrowserPublicKeyCredentialManager requestAuthorizationForPublicKeyCredentialsSelector completionHandler

-- | @+ isDeviceConfiguredForPasskeys@
isDeviceConfiguredForPasskeys :: IO Bool
isDeviceConfiguredForPasskeys  =
  do
    cls' <- getRequiredClass "ASAuthorizationWebBrowserPublicKeyCredentialManager"
    sendClassMessage cls' isDeviceConfiguredForPasskeysSelector

-- | @- authorizationStateForPlatformCredentials@
authorizationStateForPlatformCredentials :: IsASAuthorizationWebBrowserPublicKeyCredentialManager asAuthorizationWebBrowserPublicKeyCredentialManager => asAuthorizationWebBrowserPublicKeyCredentialManager -> IO ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState
authorizationStateForPlatformCredentials asAuthorizationWebBrowserPublicKeyCredentialManager =
  sendMessage asAuthorizationWebBrowserPublicKeyCredentialManager authorizationStateForPlatformCredentialsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationWebBrowserPublicKeyCredentialManager)
initSelector = mkSelector "init"

-- | @Selector@ for @requestAuthorizationForPublicKeyCredentials:@
requestAuthorizationForPublicKeyCredentialsSelector :: Selector '[Ptr ()] ()
requestAuthorizationForPublicKeyCredentialsSelector = mkSelector "requestAuthorizationForPublicKeyCredentials:"

-- | @Selector@ for @isDeviceConfiguredForPasskeys@
isDeviceConfiguredForPasskeysSelector :: Selector '[] Bool
isDeviceConfiguredForPasskeysSelector = mkSelector "isDeviceConfiguredForPasskeys"

-- | @Selector@ for @authorizationStateForPlatformCredentials@
authorizationStateForPlatformCredentialsSelector :: Selector '[] ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState
authorizationStateForPlatformCredentialsSelector = mkSelector "authorizationStateForPlatformCredentials"

