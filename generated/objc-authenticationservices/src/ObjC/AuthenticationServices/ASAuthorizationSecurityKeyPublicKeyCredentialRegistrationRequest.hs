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
  , newSelector
  , initSelector
  , credentialParametersSelector
  , setCredentialParametersSelector
  , excludedCredentialsSelector
  , setExcludedCredentialsSelector
  , residentKeyPreferenceSelector
  , setResidentKeyPreferenceSelector


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
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
init_ asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A list of parameters for the new credential which are supported by the Relying Party. The authenticator should choose from these parameters when creating the credential.
--
-- ObjC selector: @- credentialParameters@
credentialParameters :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id NSArray)
credentialParameters asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (mkSelector "credentialParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of parameters for the new credential which are supported by the Relying Party. The authenticator should choose from these parameters when creating the credential.
--
-- ObjC selector: @- setCredentialParameters:@
setCredentialParameters :: (IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> value -> IO ()
setCredentialParameters asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (mkSelector "setCredentialParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A list of descriptors indicating credentials which must not already exist on the authenticator. If a credential already exists on the authenticator which matches one or more of these descriptors, a new credential will not be created and authentication will fail.
--
-- ObjC selector: @- excludedCredentials@
excludedCredentials :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id NSArray)
excludedCredentials asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (mkSelector "excludedCredentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of descriptors indicating credentials which must not already exist on the authenticator. If a credential already exists on the authenticator which matches one or more of these descriptors, a new credential will not be created and authentication will fail.
--
-- ObjC selector: @- setExcludedCredentials:@
setExcludedCredentials :: (IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> value -> IO ()
setExcludedCredentials asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (mkSelector "setExcludedCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A preference whether the authenticator should store the private key of the newly created credential.
--
-- ObjC selector: @- residentKeyPreference@
residentKeyPreference :: IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> IO (Id NSString)
residentKeyPreference asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (mkSelector "residentKeyPreference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A preference whether the authenticator should store the private key of the newly created credential.
--
-- ObjC selector: @- setResidentKeyPreference:@
setResidentKeyPreference :: (IsASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest, IsNSString value) => asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest -> value -> IO ()
setResidentKeyPreference asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest (mkSelector "setResidentKeyPreference:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @credentialParameters@
credentialParametersSelector :: Selector
credentialParametersSelector = mkSelector "credentialParameters"

-- | @Selector@ for @setCredentialParameters:@
setCredentialParametersSelector :: Selector
setCredentialParametersSelector = mkSelector "setCredentialParameters:"

-- | @Selector@ for @excludedCredentials@
excludedCredentialsSelector :: Selector
excludedCredentialsSelector = mkSelector "excludedCredentials"

-- | @Selector@ for @setExcludedCredentials:@
setExcludedCredentialsSelector :: Selector
setExcludedCredentialsSelector = mkSelector "setExcludedCredentials:"

-- | @Selector@ for @residentKeyPreference@
residentKeyPreferenceSelector :: Selector
residentKeyPreferenceSelector = mkSelector "residentKeyPreference"

-- | @Selector@ for @setResidentKeyPreference:@
setResidentKeyPreferenceSelector :: Selector
setResidentKeyPreferenceSelector = mkSelector "setResidentKeyPreference:"

