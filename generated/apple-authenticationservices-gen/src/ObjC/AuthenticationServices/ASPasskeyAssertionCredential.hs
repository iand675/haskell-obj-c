{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates a passkey assertion response created by a credential provider extension.
--
-- Generated bindings for @ASPasskeyAssertionCredential@.
module ObjC.AuthenticationServices.ASPasskeyAssertionCredential
  ( ASPasskeyAssertionCredential
  , IsASPasskeyAssertionCredential(..)
  , initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID
  , initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutput
  , credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID
  , userHandle
  , relyingParty
  , signature
  , clientDataHash
  , authenticatorData
  , credentialID
  , extensionOutput
  , setExtensionOutput
  , authenticatorDataSelector
  , clientDataHashSelector
  , credentialIDSelector
  , credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector
  , extensionOutputSelector
  , initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector
  , initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutputSelector
  , relyingPartySelector
  , setExtensionOutputSelector
  , signatureSelector
  , userHandleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes an ASPasskeyCredential object.
--
-- @userHandle@ — The identifier for the account the passkey is associated with.
--
-- @relyingParty@ — the relying party.
--
-- @signature@ — the signature for the assertion challenge.
--
-- ObjC selector: @- initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:@
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID :: (IsASPasskeyAssertionCredential asPasskeyAssertionCredential, IsNSData userHandle, IsNSString relyingParty, IsNSData signature, IsNSData clientDataHash, IsNSData authenticatorData, IsNSData credentialID) => asPasskeyAssertionCredential -> userHandle -> relyingParty -> signature -> clientDataHash -> authenticatorData -> credentialID -> IO (Id ASPasskeyAssertionCredential)
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID asPasskeyAssertionCredential userHandle relyingParty signature clientDataHash authenticatorData credentialID =
  sendOwnedMessage asPasskeyAssertionCredential initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector (toNSData userHandle) (toNSString relyingParty) (toNSData signature) (toNSData clientDataHash) (toNSData authenticatorData) (toNSData credentialID)

-- | Initializes an ASPasskeyCredential object.
--
-- @userHandle@ — The identifier for the account the passkey is associated with.
--
-- @relyingParty@ — The relying party.
--
-- @signature@ — The signature for the assertion challenge.
--
-- @extensionOutput@ — The outputs of WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:extensionOutput:@
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutput :: (IsASPasskeyAssertionCredential asPasskeyAssertionCredential, IsNSData userHandle, IsNSString relyingParty, IsNSData signature, IsNSData clientDataHash, IsNSData authenticatorData, IsNSData credentialID, IsASPasskeyAssertionCredentialExtensionOutput extensionOutput) => asPasskeyAssertionCredential -> userHandle -> relyingParty -> signature -> clientDataHash -> authenticatorData -> credentialID -> extensionOutput -> IO (Id ASPasskeyAssertionCredential)
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutput asPasskeyAssertionCredential userHandle relyingParty signature clientDataHash authenticatorData credentialID extensionOutput =
  sendOwnedMessage asPasskeyAssertionCredential initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutputSelector (toNSData userHandle) (toNSString relyingParty) (toNSData signature) (toNSData clientDataHash) (toNSData authenticatorData) (toNSData credentialID) (toASPasskeyAssertionCredentialExtensionOutput extensionOutput)

-- | Creates and initializes a new ASPasskeyCredential object.
--
-- @userHandle@ — The identifier for the account the passkey is associated with.
--
-- @relyingParty@ — the relying party.
--
-- @signature@ — the signature for the assertion challenge.
--
-- ObjC selector: @+ credentialWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:@
credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID :: (IsNSData userHandle, IsNSString relyingParty, IsNSData signature, IsNSData clientDataHash, IsNSData authenticatorData, IsNSData credentialID) => userHandle -> relyingParty -> signature -> clientDataHash -> authenticatorData -> credentialID -> IO (Id ASPasskeyAssertionCredential)
credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID userHandle relyingParty signature clientDataHash authenticatorData credentialID =
  do
    cls' <- getRequiredClass "ASPasskeyAssertionCredential"
    sendClassMessage cls' credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector (toNSData userHandle) (toNSString relyingParty) (toNSData signature) (toNSData clientDataHash) (toNSData authenticatorData) (toNSData credentialID)

-- | The user handle of this passkey.
--
-- ObjC selector: @- userHandle@
userHandle :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
userHandle asPasskeyAssertionCredential =
  sendMessage asPasskeyAssertionCredential userHandleSelector

-- | The relying party of this credential.
--
-- ObjC selector: @- relyingParty@
relyingParty :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSString)
relyingParty asPasskeyAssertionCredential =
  sendMessage asPasskeyAssertionCredential relyingPartySelector

-- | The signature of this credential.
--
-- ObjC selector: @- signature@
signature :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
signature asPasskeyAssertionCredential =
  sendMessage asPasskeyAssertionCredential signatureSelector

-- | The hash of the client data for this assertion result.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
clientDataHash asPasskeyAssertionCredential =
  sendMessage asPasskeyAssertionCredential clientDataHashSelector

-- | The authenticator data of the application that created this credential.
--
-- ObjC selector: @- authenticatorData@
authenticatorData :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
authenticatorData asPasskeyAssertionCredential =
  sendMessage asPasskeyAssertionCredential authenticatorDataSelector

-- | The raw credential ID for this passkey credential.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
credentialID asPasskeyAssertionCredential =
  sendMessage asPasskeyAssertionCredential credentialIDSelector

-- | The outputs of WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- extensionOutput@
extensionOutput :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id ASPasskeyAssertionCredentialExtensionOutput)
extensionOutput asPasskeyAssertionCredential =
  sendMessage asPasskeyAssertionCredential extensionOutputSelector

-- | The outputs of WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- setExtensionOutput:@
setExtensionOutput :: (IsASPasskeyAssertionCredential asPasskeyAssertionCredential, IsASPasskeyAssertionCredentialExtensionOutput value) => asPasskeyAssertionCredential -> value -> IO ()
setExtensionOutput asPasskeyAssertionCredential value =
  sendMessage asPasskeyAssertionCredential setExtensionOutputSelector (toASPasskeyAssertionCredentialExtensionOutput value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:@
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector :: Selector '[Id NSData, Id NSString, Id NSData, Id NSData, Id NSData, Id NSData] (Id ASPasskeyAssertionCredential)
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector = mkSelector "initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:"

-- | @Selector@ for @initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:extensionOutput:@
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutputSelector :: Selector '[Id NSData, Id NSString, Id NSData, Id NSData, Id NSData, Id NSData, Id ASPasskeyAssertionCredentialExtensionOutput] (Id ASPasskeyAssertionCredential)
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutputSelector = mkSelector "initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:extensionOutput:"

-- | @Selector@ for @credentialWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:@
credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector :: Selector '[Id NSData, Id NSString, Id NSData, Id NSData, Id NSData, Id NSData] (Id ASPasskeyAssertionCredential)
credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector = mkSelector "credentialWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:"

-- | @Selector@ for @userHandle@
userHandleSelector :: Selector '[] (Id NSData)
userHandleSelector = mkSelector "userHandle"

-- | @Selector@ for @relyingParty@
relyingPartySelector :: Selector '[] (Id NSString)
relyingPartySelector = mkSelector "relyingParty"

-- | @Selector@ for @signature@
signatureSelector :: Selector '[] (Id NSData)
signatureSelector = mkSelector "signature"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector '[] (Id NSData)
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @authenticatorData@
authenticatorDataSelector :: Selector '[] (Id NSData)
authenticatorDataSelector = mkSelector "authenticatorData"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector '[] (Id NSData)
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @extensionOutput@
extensionOutputSelector :: Selector '[] (Id ASPasskeyAssertionCredentialExtensionOutput)
extensionOutputSelector = mkSelector "extensionOutput"

-- | @Selector@ for @setExtensionOutput:@
setExtensionOutputSelector :: Selector '[Id ASPasskeyAssertionCredentialExtensionOutput] ()
setExtensionOutputSelector = mkSelector "setExtensionOutput:"

