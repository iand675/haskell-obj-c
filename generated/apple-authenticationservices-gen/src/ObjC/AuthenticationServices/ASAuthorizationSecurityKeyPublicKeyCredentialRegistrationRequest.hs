{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest@.
module ObjC.AuthenticationServices.ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest
  ( ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest
  , IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest(..)
  , new
  , init_
  , credentialParameters
  , setCredentialParameters
  , excludedCredentials
  , setExcludedCredentials
  , residentKeyPreference
  , setResidentKeyPreference
  , credentialParametersSelector
  , excludedCredentialsSelector
  , initSelector
  , newSelector
  , residentKeyPreferenceSelector
  , setCredentialParametersSelector
  , setExcludedCredentialsSelector
  , setResidentKeyPreferenceSelector


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
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
init_ asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest =
  sendOwnedMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest initSelector

-- | A list of parameters for the new credential which are supported by the Relying Party. The authenticator should choose from these parameters when creating the credential.
--
-- ObjC selector: @- credentialParameters@
credentialParameters :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id NSArray)
credentialParameters asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest credentialParametersSelector

-- | A list of parameters for the new credential which are supported by the Relying Party. The authenticator should choose from these parameters when creating the credential.
--
-- ObjC selector: @- setCredentialParameters:@
setCredentialParameters :: (IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> value -> IO ()
setCredentialParameters asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest value =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest setCredentialParametersSelector (toNSArray value)

-- | A list of descriptors indicating credentials which must not already exist on the authenticator. If a credential already exists on the authenticator which matches one or more of these descriptors, a new credential will not be created and authentication will fail.
--
-- ObjC selector: @- excludedCredentials@
excludedCredentials :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id NSArray)
excludedCredentials asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest excludedCredentialsSelector

-- | A list of descriptors indicating credentials which must not already exist on the authenticator. If a credential already exists on the authenticator which matches one or more of these descriptors, a new credential will not be created and authentication will fail.
--
-- ObjC selector: @- setExcludedCredentials:@
setExcludedCredentials :: (IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> value -> IO ()
setExcludedCredentials asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest value =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest setExcludedCredentialsSelector (toNSArray value)

-- | A preference whether the authenticator should store the private key of the newly created credential.
--
-- ObjC selector: @- residentKeyPreference@
residentKeyPreference :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id NSString)
residentKeyPreference asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest residentKeyPreferenceSelector

-- | A preference whether the authenticator should store the private key of the newly created credential.
--
-- ObjC selector: @- setResidentKeyPreference:@
setResidentKeyPreference :: (IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest, IsNSString value) => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> value -> IO ()
setResidentKeyPreference asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest value =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest setResidentKeyPreferenceSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @credentialParameters@
credentialParametersSelector :: Selector '[] (Id NSArray)
credentialParametersSelector = mkSelector "credentialParameters"

-- | @Selector@ for @setCredentialParameters:@
setCredentialParametersSelector :: Selector '[Id NSArray] ()
setCredentialParametersSelector = mkSelector "setCredentialParameters:"

-- | @Selector@ for @excludedCredentials@
excludedCredentialsSelector :: Selector '[] (Id NSArray)
excludedCredentialsSelector = mkSelector "excludedCredentials"

-- | @Selector@ for @setExcludedCredentials:@
setExcludedCredentialsSelector :: Selector '[Id NSArray] ()
setExcludedCredentialsSelector = mkSelector "setExcludedCredentials:"

-- | @Selector@ for @residentKeyPreference@
residentKeyPreferenceSelector :: Selector '[] (Id NSString)
residentKeyPreferenceSelector = mkSelector "residentKeyPreference"

-- | @Selector@ for @setResidentKeyPreference:@
setResidentKeyPreferenceSelector :: Selector '[Id NSString] ()
setResidentKeyPreferenceSelector = mkSelector "setResidentKeyPreference:"

