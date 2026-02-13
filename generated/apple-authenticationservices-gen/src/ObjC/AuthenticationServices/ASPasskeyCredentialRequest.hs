{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates a passkey assertion request made to a credential provider extension.
--
-- Generated bindings for @ASPasskeyCredentialRequest@.
module ObjC.AuthenticationServices.ASPasskeyCredentialRequest
  ( ASPasskeyCredentialRequest
  , IsASPasskeyCredentialRequest(..)
  , init_
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInput
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInput
  , requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms
  , clientDataHash
  , userVerificationPreference
  , setUserVerificationPreference
  , supportedAlgorithms
  , excludedCredentials
  , assertionExtensionInput
  , registrationExtensionInput
  , assertionExtensionInputSelector
  , clientDataHashSelector
  , excludedCredentialsSelector
  , initSelector
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInputSelector
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInputSelector
  , registrationExtensionInputSelector
  , requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector
  , setUserVerificationPreferenceSelector
  , supportedAlgorithmsSelector
  , userVerificationPreferenceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id ASPasskeyCredentialRequest)
init_ asPasskeyCredentialRequest =
  sendOwnedMessage asPasskeyCredentialRequest initSelector

-- | Initializes an instance of ASPasskeyCredentialRequest.
--
-- @credentialIdentity@ — credential identity to used for this request.
--
-- @clientDataHash@ — the client data to be signed for this assertion request.
--
-- @userVerificationPreference@ — user verification preference setting of this assertion request.
--
-- ObjC selector: @- initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms :: (IsASPasskeyCredentialRequest asPasskeyCredentialRequest, IsASPasskeyCredentialIdentity credentialIdentity, IsNSData clientDataHash, IsNSString userVerificationPreference, IsNSArray supportedAlgorithms) => asPasskeyCredentialRequest -> credentialIdentity -> clientDataHash -> userVerificationPreference -> supportedAlgorithms -> IO (Id ASPasskeyCredentialRequest)
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms asPasskeyCredentialRequest credentialIdentity clientDataHash userVerificationPreference supportedAlgorithms =
  sendOwnedMessage asPasskeyCredentialRequest initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector (toASPasskeyCredentialIdentity credentialIdentity) (toNSData clientDataHash) (toNSString userVerificationPreference) (toNSArray supportedAlgorithms)

-- | Initializes an instance of ASPasskeyCredentialRequest.
--
-- @credentialIdentity@ — credential identity to used for this request.
--
-- @clientDataHash@ — the client data to be signed for this assertion request.
--
-- @userVerificationPreference@ — user verification preference setting of this assertion request.
--
-- @supportedAlgorithms@ — the set of support algorithms for the credential's key.
--
-- @assertionExtensionInput@ — input for any requested passkey extensions.
--
-- ObjC selector: @- initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:assertionExtensionInput:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInput :: (IsASPasskeyCredentialRequest asPasskeyCredentialRequest, IsASPasskeyCredentialIdentity credentialIdentity, IsNSData clientDataHash, IsNSString userVerificationPreference, IsNSArray supportedAlgorithms, IsASPasskeyAssertionCredentialExtensionInput assertionExtensionInput) => asPasskeyCredentialRequest -> credentialIdentity -> clientDataHash -> userVerificationPreference -> supportedAlgorithms -> assertionExtensionInput -> IO (Id ASPasskeyCredentialRequest)
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInput asPasskeyCredentialRequest credentialIdentity clientDataHash userVerificationPreference supportedAlgorithms assertionExtensionInput =
  sendOwnedMessage asPasskeyCredentialRequest initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInputSelector (toASPasskeyCredentialIdentity credentialIdentity) (toNSData clientDataHash) (toNSString userVerificationPreference) (toNSArray supportedAlgorithms) (toASPasskeyAssertionCredentialExtensionInput assertionExtensionInput)

-- | Initializes an instance of ASPasskeyCredentialRequest.
--
-- @credentialIdentity@ — credential identity to used for this request.
--
-- @clientDataHash@ — the client data to be signed for this assertion request.
--
-- @userVerificationPreference@ — user verification preference setting of this assertion request.
--
-- @supportedAlgorithms@ — the set of support algorithms for the credential's key.
--
-- @registrationExtensionInput@ — input for any requested passkey extensions.
--
-- ObjC selector: @- initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:registrationExtensionInput:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInput :: (IsASPasskeyCredentialRequest asPasskeyCredentialRequest, IsASPasskeyCredentialIdentity credentialIdentity, IsNSData clientDataHash, IsNSString userVerificationPreference, IsNSArray supportedAlgorithms, IsASPasskeyRegistrationCredentialExtensionInput registrationExtensionInput) => asPasskeyCredentialRequest -> credentialIdentity -> clientDataHash -> userVerificationPreference -> supportedAlgorithms -> registrationExtensionInput -> IO (Id ASPasskeyCredentialRequest)
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInput asPasskeyCredentialRequest credentialIdentity clientDataHash userVerificationPreference supportedAlgorithms registrationExtensionInput =
  sendOwnedMessage asPasskeyCredentialRequest initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInputSelector (toASPasskeyCredentialIdentity credentialIdentity) (toNSData clientDataHash) (toNSString userVerificationPreference) (toNSArray supportedAlgorithms) (toASPasskeyRegistrationCredentialExtensionInput registrationExtensionInput)

-- | Creates and initializes an instance of ASPasskeyCredentialRequest.
--
-- @credentialIdentity@ — credential identity to used for this request.
--
-- @clientDataHash@ — the client data to be signed for this assertion request.
--
-- @userVerificationPreference@ — user verification preference setting of this assertion request.
--
-- ObjC selector: @+ requestWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:@
requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms :: (IsASPasskeyCredentialIdentity credentialIdentity, IsNSData clientDataHash, IsNSString userVerificationPreference, IsNSArray supportedAlgorithms) => credentialIdentity -> clientDataHash -> userVerificationPreference -> supportedAlgorithms -> IO (Id ASPasskeyCredentialRequest)
requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms credentialIdentity clientDataHash userVerificationPreference supportedAlgorithms =
  do
    cls' <- getRequiredClass "ASPasskeyCredentialRequest"
    sendClassMessage cls' requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector (toASPasskeyCredentialIdentity credentialIdentity) (toNSData clientDataHash) (toNSString userVerificationPreference) (toNSArray supportedAlgorithms)

-- | Hash of client data for credential provider to sign as part of the assertion/registration operation.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id NSData)
clientDataHash asPasskeyCredentialRequest =
  sendMessage asPasskeyCredentialRequest clientDataHashSelector

-- | A preference for whether the authenticator should attempt to verify that it is being used by its owner, such as through a PIN or biometrics.
--
-- ObjC selector: @- userVerificationPreference@
userVerificationPreference :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id NSString)
userVerificationPreference asPasskeyCredentialRequest =
  sendMessage asPasskeyCredentialRequest userVerificationPreferenceSelector

-- | A preference for whether the authenticator should attempt to verify that it is being used by its owner, such as through a PIN or biometrics.
--
-- ObjC selector: @- setUserVerificationPreference:@
setUserVerificationPreference :: (IsASPasskeyCredentialRequest asPasskeyCredentialRequest, IsNSString value) => asPasskeyCredentialRequest -> value -> IO ()
setUserVerificationPreference asPasskeyCredentialRequest value =
  sendMessage asPasskeyCredentialRequest setUserVerificationPreferenceSelector (toNSString value)

-- | A list of signing algorithms supported by the relying party. Will be empty for assertion requests.
--
-- ObjC selector: @- supportedAlgorithms@
supportedAlgorithms :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id NSArray)
supportedAlgorithms asPasskeyCredentialRequest =
  sendMessage asPasskeyCredentialRequest supportedAlgorithmsSelector

-- | @- excludedCredentials@
excludedCredentials :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id NSArray)
excludedCredentials asPasskeyCredentialRequest =
  sendMessage asPasskeyCredentialRequest excludedCredentialsSelector

-- | Inputs for WebAuthn extensions used for passkey assertion. Will be nil for registration requests.
--
-- ObjC selector: @- assertionExtensionInput@
assertionExtensionInput :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id ASPasskeyAssertionCredentialExtensionInput)
assertionExtensionInput asPasskeyCredentialRequest =
  sendMessage asPasskeyCredentialRequest assertionExtensionInputSelector

-- | Inputs for WebAuthn extensions used for passkey registration. Will be nil for assertion requests.
--
-- ObjC selector: @- registrationExtensionInput@
registrationExtensionInput :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id ASPasskeyRegistrationCredentialExtensionInput)
registrationExtensionInput asPasskeyCredentialRequest =
  sendMessage asPasskeyCredentialRequest registrationExtensionInputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPasskeyCredentialRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector :: Selector '[Id ASPasskeyCredentialIdentity, Id NSData, Id NSString, Id NSArray] (Id ASPasskeyCredentialRequest)
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector = mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:"

-- | @Selector@ for @initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:assertionExtensionInput:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInputSelector :: Selector '[Id ASPasskeyCredentialIdentity, Id NSData, Id NSString, Id NSArray, Id ASPasskeyAssertionCredentialExtensionInput] (Id ASPasskeyCredentialRequest)
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInputSelector = mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:assertionExtensionInput:"

-- | @Selector@ for @initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:registrationExtensionInput:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInputSelector :: Selector '[Id ASPasskeyCredentialIdentity, Id NSData, Id NSString, Id NSArray, Id ASPasskeyRegistrationCredentialExtensionInput] (Id ASPasskeyCredentialRequest)
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInputSelector = mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:registrationExtensionInput:"

-- | @Selector@ for @requestWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:@
requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector :: Selector '[Id ASPasskeyCredentialIdentity, Id NSData, Id NSString, Id NSArray] (Id ASPasskeyCredentialRequest)
requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector = mkSelector "requestWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector '[] (Id NSData)
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @userVerificationPreference@
userVerificationPreferenceSelector :: Selector '[] (Id NSString)
userVerificationPreferenceSelector = mkSelector "userVerificationPreference"

-- | @Selector@ for @setUserVerificationPreference:@
setUserVerificationPreferenceSelector :: Selector '[Id NSString] ()
setUserVerificationPreferenceSelector = mkSelector "setUserVerificationPreference:"

-- | @Selector@ for @supportedAlgorithms@
supportedAlgorithmsSelector :: Selector '[] (Id NSArray)
supportedAlgorithmsSelector = mkSelector "supportedAlgorithms"

-- | @Selector@ for @excludedCredentials@
excludedCredentialsSelector :: Selector '[] (Id NSArray)
excludedCredentialsSelector = mkSelector "excludedCredentials"

-- | @Selector@ for @assertionExtensionInput@
assertionExtensionInputSelector :: Selector '[] (Id ASPasskeyAssertionCredentialExtensionInput)
assertionExtensionInputSelector = mkSelector "assertionExtensionInput"

-- | @Selector@ for @registrationExtensionInput@
registrationExtensionInputSelector :: Selector '[] (Id ASPasskeyRegistrationCredentialExtensionInput)
registrationExtensionInputSelector = mkSelector "registrationExtensionInput"

