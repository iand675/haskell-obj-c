{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that holds various parameters related to a passkey credential request.  This class is provided by the system to the credential provider extension when there is an active passkey request as part of  -[ASCredentialProviderViewController prepareCredentialListForServiceIdentifiers:requestParameters:] and should be used  to construct a passkey credential response using the item selected by the user from the extension's UI.
--
-- Generated bindings for @ASPasskeyCredentialRequestParameters@.
module ObjC.AuthenticationServices.ASPasskeyCredentialRequestParameters
  ( ASPasskeyCredentialRequestParameters
  , IsASPasskeyCredentialRequestParameters(..)
  , init_
  , relyingPartyIdentifier
  , clientDataHash
  , userVerificationPreference
  , allowedCredentials
  , extensionInput
  , allowedCredentialsSelector
  , clientDataHashSelector
  , extensionInputSelector
  , initSelector
  , relyingPartyIdentifierSelector
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
init_ :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id ASPasskeyCredentialRequestParameters)
init_ asPasskeyCredentialRequestParameters =
  sendOwnedMessage asPasskeyCredentialRequestParameters initSelector

-- | The relying party identifier for this request.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSString)
relyingPartyIdentifier asPasskeyCredentialRequestParameters =
  sendMessage asPasskeyCredentialRequestParameters relyingPartyIdentifierSelector

-- | Hash of client data for credential provider to sign as part of the operation.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSData)
clientDataHash asPasskeyCredentialRequestParameters =
  sendMessage asPasskeyCredentialRequestParameters clientDataHashSelector

-- | A preference for whether the authenticator should attempt to verify that it is being used by its owner, such as through a PIN or biometrics.
--
-- ObjC selector: @- userVerificationPreference@
userVerificationPreference :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSString)
userVerificationPreference asPasskeyCredentialRequestParameters =
  sendMessage asPasskeyCredentialRequestParameters userVerificationPreferenceSelector

-- | A list of allowed credential IDs for this request. An empty list means all credentials are allowed.
--
-- ObjC selector: @- allowedCredentials@
allowedCredentials :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSArray)
allowedCredentials asPasskeyCredentialRequestParameters =
  sendMessage asPasskeyCredentialRequestParameters allowedCredentialsSelector

-- | Inputs for WebAuthn extensions used for passkey assertion.
--
-- ObjC selector: @- extensionInput@
extensionInput :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id ASPasskeyAssertionCredentialExtensionInput)
extensionInput asPasskeyCredentialRequestParameters =
  sendMessage asPasskeyCredentialRequestParameters extensionInputSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPasskeyCredentialRequestParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector '[] (Id NSString)
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector '[] (Id NSData)
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @userVerificationPreference@
userVerificationPreferenceSelector :: Selector '[] (Id NSString)
userVerificationPreferenceSelector = mkSelector "userVerificationPreference"

-- | @Selector@ for @allowedCredentials@
allowedCredentialsSelector :: Selector '[] (Id NSArray)
allowedCredentialsSelector = mkSelector "allowedCredentials"

-- | @Selector@ for @extensionInput@
extensionInputSelector :: Selector '[] (Id ASPasskeyAssertionCredentialExtensionInput)
extensionInputSelector = mkSelector "extensionInput"

