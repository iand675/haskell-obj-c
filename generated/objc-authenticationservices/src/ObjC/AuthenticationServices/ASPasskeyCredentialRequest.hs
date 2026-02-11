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
  , excludedCredentials
  , initSelector
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInputSelector
  , initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInputSelector
  , requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector
  , clientDataHashSelector
  , userVerificationPreferenceSelector
  , setUserVerificationPreferenceSelector
  , excludedCredentialsSelector


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

-- | @- init@
init_ :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id ASPasskeyCredentialRequest)
init_ asPasskeyCredentialRequest  =
  sendMsg asPasskeyCredentialRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms asPasskeyCredentialRequest  credentialIdentity clientDataHash userVerificationPreference supportedAlgorithms =
withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
  withObjCPtr clientDataHash $ \raw_clientDataHash ->
    withObjCPtr userVerificationPreference $ \raw_userVerificationPreference ->
      withObjCPtr supportedAlgorithms $ \raw_supportedAlgorithms ->
          sendMsg asPasskeyCredentialRequest (mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentity :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_userVerificationPreference :: Ptr ()), argPtr (castPtr raw_supportedAlgorithms :: Ptr ())] >>= ownedObject . castPtr

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
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInput asPasskeyCredentialRequest  credentialIdentity clientDataHash userVerificationPreference supportedAlgorithms assertionExtensionInput =
withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
  withObjCPtr clientDataHash $ \raw_clientDataHash ->
    withObjCPtr userVerificationPreference $ \raw_userVerificationPreference ->
      withObjCPtr supportedAlgorithms $ \raw_supportedAlgorithms ->
        withObjCPtr assertionExtensionInput $ \raw_assertionExtensionInput ->
            sendMsg asPasskeyCredentialRequest (mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:assertionExtensionInput:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentity :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_userVerificationPreference :: Ptr ()), argPtr (castPtr raw_supportedAlgorithms :: Ptr ()), argPtr (castPtr raw_assertionExtensionInput :: Ptr ())] >>= ownedObject . castPtr

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
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInput asPasskeyCredentialRequest  credentialIdentity clientDataHash userVerificationPreference supportedAlgorithms registrationExtensionInput =
withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
  withObjCPtr clientDataHash $ \raw_clientDataHash ->
    withObjCPtr userVerificationPreference $ \raw_userVerificationPreference ->
      withObjCPtr supportedAlgorithms $ \raw_supportedAlgorithms ->
        withObjCPtr registrationExtensionInput $ \raw_registrationExtensionInput ->
            sendMsg asPasskeyCredentialRequest (mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:registrationExtensionInput:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentity :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_userVerificationPreference :: Ptr ()), argPtr (castPtr raw_supportedAlgorithms :: Ptr ()), argPtr (castPtr raw_registrationExtensionInput :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
      withObjCPtr clientDataHash $ \raw_clientDataHash ->
        withObjCPtr userVerificationPreference $ \raw_userVerificationPreference ->
          withObjCPtr supportedAlgorithms $ \raw_supportedAlgorithms ->
            sendClassMsg cls' (mkSelector "requestWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentity :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_userVerificationPreference :: Ptr ()), argPtr (castPtr raw_supportedAlgorithms :: Ptr ())] >>= retainedObject . castPtr

-- | Hash of client data for credential provider to sign as part of the assertion/registration operation.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id NSData)
clientDataHash asPasskeyCredentialRequest  =
  sendMsg asPasskeyCredentialRequest (mkSelector "clientDataHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A preference for whether the authenticator should attempt to verify that it is being used by its owner, such as through a PIN or biometrics.
--
-- ObjC selector: @- userVerificationPreference@
userVerificationPreference :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id NSString)
userVerificationPreference asPasskeyCredentialRequest  =
  sendMsg asPasskeyCredentialRequest (mkSelector "userVerificationPreference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A preference for whether the authenticator should attempt to verify that it is being used by its owner, such as through a PIN or biometrics.
--
-- ObjC selector: @- setUserVerificationPreference:@
setUserVerificationPreference :: (IsASPasskeyCredentialRequest asPasskeyCredentialRequest, IsNSString value) => asPasskeyCredentialRequest -> value -> IO ()
setUserVerificationPreference asPasskeyCredentialRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg asPasskeyCredentialRequest (mkSelector "setUserVerificationPreference:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- excludedCredentials@
excludedCredentials :: IsASPasskeyCredentialRequest asPasskeyCredentialRequest => asPasskeyCredentialRequest -> IO (Id NSArray)
excludedCredentials asPasskeyCredentialRequest  =
  sendMsg asPasskeyCredentialRequest (mkSelector "excludedCredentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector :: Selector
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector = mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:"

-- | @Selector@ for @initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:assertionExtensionInput:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInputSelector :: Selector
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_assertionExtensionInputSelector = mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:assertionExtensionInput:"

-- | @Selector@ for @initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:registrationExtensionInput:@
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInputSelector :: Selector
initWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithms_registrationExtensionInputSelector = mkSelector "initWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:registrationExtensionInput:"

-- | @Selector@ for @requestWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:@
requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector :: Selector
requestWithCredentialIdentity_clientDataHash_userVerificationPreference_supportedAlgorithmsSelector = mkSelector "requestWithCredentialIdentity:clientDataHash:userVerificationPreference:supportedAlgorithms:"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @userVerificationPreference@
userVerificationPreferenceSelector :: Selector
userVerificationPreferenceSelector = mkSelector "userVerificationPreference"

-- | @Selector@ for @setUserVerificationPreference:@
setUserVerificationPreferenceSelector :: Selector
setUserVerificationPreferenceSelector = mkSelector "setUserVerificationPreference:"

-- | @Selector@ for @excludedCredentials@
excludedCredentialsSelector :: Selector
excludedCredentialsSelector = mkSelector "excludedCredentials"

