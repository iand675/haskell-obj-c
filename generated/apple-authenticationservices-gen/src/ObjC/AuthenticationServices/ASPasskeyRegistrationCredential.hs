{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates a passkey registration response created by a credential provider extension.
--
-- Generated bindings for @ASPasskeyRegistrationCredential@.
module ObjC.AuthenticationServices.ASPasskeyRegistrationCredential
  ( ASPasskeyRegistrationCredential
  , IsASPasskeyRegistrationCredential(..)
  , initWithRelyingParty_clientDataHash_credentialID_attestationObject
  , initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutput
  , credentialWithRelyingParty_clientDataHash_credentialID_attestationObject
  , relyingParty
  , clientDataHash
  , credentialID
  , attestationObject
  , extensionOutput
  , setExtensionOutput
  , attestationObjectSelector
  , clientDataHashSelector
  , credentialIDSelector
  , credentialWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector
  , extensionOutputSelector
  , initWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector
  , initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutputSelector
  , relyingPartySelector
  , setExtensionOutputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes an ASPasskeyRegistrationCredential object.
--
-- @relyingParty@ — The relying party identifier associated with this passkey.
--
-- @clientDataHash@ — The JSON encoded client data for this registration result.
--
-- @credentialID@ — The unique identifier for this passkey.
--
-- @attestationObject@ — The attestation object for this passkey registration result.
--
-- ObjC selector: @- initWithRelyingParty:clientDataHash:credentialID:attestationObject:@
initWithRelyingParty_clientDataHash_credentialID_attestationObject :: (IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential, IsNSString relyingParty, IsNSData clientDataHash, IsNSData credentialID, IsNSData attestationObject) => asPasskeyRegistrationCredential -> relyingParty -> clientDataHash -> credentialID -> attestationObject -> IO (Id ASPasskeyRegistrationCredential)
initWithRelyingParty_clientDataHash_credentialID_attestationObject asPasskeyRegistrationCredential relyingParty clientDataHash credentialID attestationObject =
  sendOwnedMessage asPasskeyRegistrationCredential initWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector (toNSString relyingParty) (toNSData clientDataHash) (toNSData credentialID) (toNSData attestationObject)

-- | Initializes an ASPasskeyRegistrationCredential object.
--
-- @relyingParty@ — The relying party identifier associated with this passkey.
--
-- @clientDataHash@ — The JSON encoded client data for this registration result.
--
-- @credentialID@ — The unique identifier for this passkey.
--
-- @attestationObject@ — The attestation object for this passkey registration result.
--
-- @extensionOutput@ — The output of WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- initWithRelyingParty:clientDataHash:credentialID:attestationObject:extensionOutput:@
initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutput :: (IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential, IsNSString relyingParty, IsNSData clientDataHash, IsNSData credentialID, IsNSData attestationObject, IsASPasskeyRegistrationCredentialExtensionOutput extensionOutput) => asPasskeyRegistrationCredential -> relyingParty -> clientDataHash -> credentialID -> attestationObject -> extensionOutput -> IO (Id ASPasskeyRegistrationCredential)
initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutput asPasskeyRegistrationCredential relyingParty clientDataHash credentialID attestationObject extensionOutput =
  sendOwnedMessage asPasskeyRegistrationCredential initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutputSelector (toNSString relyingParty) (toNSData clientDataHash) (toNSData credentialID) (toNSData attestationObject) (toASPasskeyRegistrationCredentialExtensionOutput extensionOutput)

-- | Creates and initializes an ASPasskeyRegistrationCredential object.
--
-- @relyingParty@ — The relying party identifier associated with this passkey.
--
-- @clientDataHash@ — The JSON encoded client data for this registration result.
--
-- @credentialID@ — The unique identifier for this passkey.
--
-- @attestationObject@ — The attestation object for this passkey registration result.
--
-- ObjC selector: @+ credentialWithRelyingParty:clientDataHash:credentialID:attestationObject:@
credentialWithRelyingParty_clientDataHash_credentialID_attestationObject :: (IsNSString relyingParty, IsNSData clientDataHash, IsNSData credentialID, IsNSData attestationObject) => relyingParty -> clientDataHash -> credentialID -> attestationObject -> IO (Id ASPasskeyRegistrationCredential)
credentialWithRelyingParty_clientDataHash_credentialID_attestationObject relyingParty clientDataHash credentialID attestationObject =
  do
    cls' <- getRequiredClass "ASPasskeyRegistrationCredential"
    sendClassMessage cls' credentialWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector (toNSString relyingParty) (toNSData clientDataHash) (toNSData credentialID) (toNSData attestationObject)

-- | The relying party identifier associated with this passkey.
--
-- ObjC selector: @- relyingParty@
relyingParty :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSString)
relyingParty asPasskeyRegistrationCredential =
  sendMessage asPasskeyRegistrationCredential relyingPartySelector

-- | The hash of the client data for this registration result.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSData)
clientDataHash asPasskeyRegistrationCredential =
  sendMessage asPasskeyRegistrationCredential clientDataHashSelector

-- | The raw credential identifier of this passkey.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSData)
credentialID asPasskeyRegistrationCredential =
  sendMessage asPasskeyRegistrationCredential credentialIDSelector

-- | The attestation object for this passkey registration result.
--
-- ObjC selector: @- attestationObject@
attestationObject :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSData)
attestationObject asPasskeyRegistrationCredential =
  sendMessage asPasskeyRegistrationCredential attestationObjectSelector

-- | The outputs for WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- extensionOutput@
extensionOutput :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id ASPasskeyRegistrationCredentialExtensionOutput)
extensionOutput asPasskeyRegistrationCredential =
  sendMessage asPasskeyRegistrationCredential extensionOutputSelector

-- | The outputs for WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- setExtensionOutput:@
setExtensionOutput :: (IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential, IsASPasskeyRegistrationCredentialExtensionOutput value) => asPasskeyRegistrationCredential -> value -> IO ()
setExtensionOutput asPasskeyRegistrationCredential value =
  sendMessage asPasskeyRegistrationCredential setExtensionOutputSelector (toASPasskeyRegistrationCredentialExtensionOutput value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRelyingParty:clientDataHash:credentialID:attestationObject:@
initWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector :: Selector '[Id NSString, Id NSData, Id NSData, Id NSData] (Id ASPasskeyRegistrationCredential)
initWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector = mkSelector "initWithRelyingParty:clientDataHash:credentialID:attestationObject:"

-- | @Selector@ for @initWithRelyingParty:clientDataHash:credentialID:attestationObject:extensionOutput:@
initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutputSelector :: Selector '[Id NSString, Id NSData, Id NSData, Id NSData, Id ASPasskeyRegistrationCredentialExtensionOutput] (Id ASPasskeyRegistrationCredential)
initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutputSelector = mkSelector "initWithRelyingParty:clientDataHash:credentialID:attestationObject:extensionOutput:"

-- | @Selector@ for @credentialWithRelyingParty:clientDataHash:credentialID:attestationObject:@
credentialWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector :: Selector '[Id NSString, Id NSData, Id NSData, Id NSData] (Id ASPasskeyRegistrationCredential)
credentialWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector = mkSelector "credentialWithRelyingParty:clientDataHash:credentialID:attestationObject:"

-- | @Selector@ for @relyingParty@
relyingPartySelector :: Selector '[] (Id NSString)
relyingPartySelector = mkSelector "relyingParty"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector '[] (Id NSData)
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector '[] (Id NSData)
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @attestationObject@
attestationObjectSelector :: Selector '[] (Id NSData)
attestationObjectSelector = mkSelector "attestationObject"

-- | @Selector@ for @extensionOutput@
extensionOutputSelector :: Selector '[] (Id ASPasskeyRegistrationCredentialExtensionOutput)
extensionOutputSelector = mkSelector "extensionOutput"

-- | @Selector@ for @setExtensionOutput:@
setExtensionOutputSelector :: Selector '[Id ASPasskeyRegistrationCredentialExtensionOutput] ()
setExtensionOutputSelector = mkSelector "setExtensionOutput:"

