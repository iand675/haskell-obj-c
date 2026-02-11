{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , requestAuthorizationForPublicKeyCredentialsSelector
  , isDeviceConfiguredForPasskeysSelector
  , authorizationStateForPlatformCredentialsSelector

  -- * Enum types
  , ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState(ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState)
  , pattern ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateAuthorized
  , pattern ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateDenied
  , pattern ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateNotDetermined

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
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASAuthorizationWebBrowserPublicKeyCredentialManager asAuthorizationWebBrowserPublicKeyCredentialManager => asAuthorizationWebBrowserPublicKeyCredentialManager -> IO (Id ASAuthorizationWebBrowserPublicKeyCredentialManager)
init_ asAuthorizationWebBrowserPublicKeyCredentialManager  =
  sendMsg asAuthorizationWebBrowserPublicKeyCredentialManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- requestAuthorizationForPublicKeyCredentials:@
requestAuthorizationForPublicKeyCredentials :: IsASAuthorizationWebBrowserPublicKeyCredentialManager asAuthorizationWebBrowserPublicKeyCredentialManager => asAuthorizationWebBrowserPublicKeyCredentialManager -> Ptr () -> IO ()
requestAuthorizationForPublicKeyCredentials asAuthorizationWebBrowserPublicKeyCredentialManager  completionHandler =
  sendMsg asAuthorizationWebBrowserPublicKeyCredentialManager (mkSelector "requestAuthorizationForPublicKeyCredentials:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ isDeviceConfiguredForPasskeys@
isDeviceConfiguredForPasskeys :: IO Bool
isDeviceConfiguredForPasskeys  =
  do
    cls' <- getRequiredClass "ASAuthorizationWebBrowserPublicKeyCredentialManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isDeviceConfiguredForPasskeys") retCULong []

-- | @- authorizationStateForPlatformCredentials@
authorizationStateForPlatformCredentials :: IsASAuthorizationWebBrowserPublicKeyCredentialManager asAuthorizationWebBrowserPublicKeyCredentialManager => asAuthorizationWebBrowserPublicKeyCredentialManager -> IO ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState
authorizationStateForPlatformCredentials asAuthorizationWebBrowserPublicKeyCredentialManager  =
  fmap (coerce :: CLong -> ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState) $ sendMsg asAuthorizationWebBrowserPublicKeyCredentialManager (mkSelector "authorizationStateForPlatformCredentials") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @requestAuthorizationForPublicKeyCredentials:@
requestAuthorizationForPublicKeyCredentialsSelector :: Selector
requestAuthorizationForPublicKeyCredentialsSelector = mkSelector "requestAuthorizationForPublicKeyCredentials:"

-- | @Selector@ for @isDeviceConfiguredForPasskeys@
isDeviceConfiguredForPasskeysSelector :: Selector
isDeviceConfiguredForPasskeysSelector = mkSelector "isDeviceConfiguredForPasskeys"

-- | @Selector@ for @authorizationStateForPlatformCredentials@
authorizationStateForPlatformCredentialsSelector :: Selector
authorizationStateForPlatformCredentialsSelector = mkSelector "authorizationStateForPlatformCredentials"

