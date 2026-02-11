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
  , initSelector
  , relyingPartyIdentifierSelector
  , clientDataHashSelector
  , userVerificationPreferenceSelector
  , allowedCredentialsSelector


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
init_ :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id ASPasskeyCredentialRequestParameters)
init_ asPasskeyCredentialRequestParameters  =
  sendMsg asPasskeyCredentialRequestParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The relying party identifier for this request.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSString)
relyingPartyIdentifier asPasskeyCredentialRequestParameters  =
  sendMsg asPasskeyCredentialRequestParameters (mkSelector "relyingPartyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Hash of client data for credential provider to sign as part of the operation.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSData)
clientDataHash asPasskeyCredentialRequestParameters  =
  sendMsg asPasskeyCredentialRequestParameters (mkSelector "clientDataHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A preference for whether the authenticator should attempt to verify that it is being used by its owner, such as through a PIN or biometrics.
--
-- ObjC selector: @- userVerificationPreference@
userVerificationPreference :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSString)
userVerificationPreference asPasskeyCredentialRequestParameters  =
  sendMsg asPasskeyCredentialRequestParameters (mkSelector "userVerificationPreference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of allowed credential IDs for this request. An empty list means all credentials are allowed.
--
-- ObjC selector: @- allowedCredentials@
allowedCredentials :: IsASPasskeyCredentialRequestParameters asPasskeyCredentialRequestParameters => asPasskeyCredentialRequestParameters -> IO (Id NSArray)
allowedCredentials asPasskeyCredentialRequestParameters  =
  sendMsg asPasskeyCredentialRequestParameters (mkSelector "allowedCredentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @userVerificationPreference@
userVerificationPreferenceSelector :: Selector
userVerificationPreferenceSelector = mkSelector "userVerificationPreference"

-- | @Selector@ for @allowedCredentials@
allowedCredentialsSelector :: Selector
allowedCredentialsSelector = mkSelector "allowedCredentials"

