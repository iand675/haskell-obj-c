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
  , initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector
  , initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutputSelector
  , credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector
  , userHandleSelector
  , relyingPartySelector
  , signatureSelector
  , clientDataHashSelector
  , authenticatorDataSelector
  , credentialIDSelector
  , extensionOutputSelector
  , setExtensionOutputSelector


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
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID asPasskeyAssertionCredential  userHandle relyingParty signature clientDataHash authenticatorData credentialID =
  withObjCPtr userHandle $ \raw_userHandle ->
    withObjCPtr relyingParty $ \raw_relyingParty ->
      withObjCPtr signature $ \raw_signature ->
        withObjCPtr clientDataHash $ \raw_clientDataHash ->
          withObjCPtr authenticatorData $ \raw_authenticatorData ->
            withObjCPtr credentialID $ \raw_credentialID ->
                sendMsg asPasskeyAssertionCredential (mkSelector "initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:") (retPtr retVoid) [argPtr (castPtr raw_userHandle :: Ptr ()), argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_signature :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_authenticatorData :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ())] >>= ownedObject . castPtr

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
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutput asPasskeyAssertionCredential  userHandle relyingParty signature clientDataHash authenticatorData credentialID extensionOutput =
  withObjCPtr userHandle $ \raw_userHandle ->
    withObjCPtr relyingParty $ \raw_relyingParty ->
      withObjCPtr signature $ \raw_signature ->
        withObjCPtr clientDataHash $ \raw_clientDataHash ->
          withObjCPtr authenticatorData $ \raw_authenticatorData ->
            withObjCPtr credentialID $ \raw_credentialID ->
              withObjCPtr extensionOutput $ \raw_extensionOutput ->
                  sendMsg asPasskeyAssertionCredential (mkSelector "initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:extensionOutput:") (retPtr retVoid) [argPtr (castPtr raw_userHandle :: Ptr ()), argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_signature :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_authenticatorData :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ()), argPtr (castPtr raw_extensionOutput :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr userHandle $ \raw_userHandle ->
      withObjCPtr relyingParty $ \raw_relyingParty ->
        withObjCPtr signature $ \raw_signature ->
          withObjCPtr clientDataHash $ \raw_clientDataHash ->
            withObjCPtr authenticatorData $ \raw_authenticatorData ->
              withObjCPtr credentialID $ \raw_credentialID ->
                sendClassMsg cls' (mkSelector "credentialWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:") (retPtr retVoid) [argPtr (castPtr raw_userHandle :: Ptr ()), argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_signature :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_authenticatorData :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ())] >>= retainedObject . castPtr

-- | The user handle of this passkey.
--
-- ObjC selector: @- userHandle@
userHandle :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
userHandle asPasskeyAssertionCredential  =
    sendMsg asPasskeyAssertionCredential (mkSelector "userHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The relying party of this credential.
--
-- ObjC selector: @- relyingParty@
relyingParty :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSString)
relyingParty asPasskeyAssertionCredential  =
    sendMsg asPasskeyAssertionCredential (mkSelector "relyingParty") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The signature of this credential.
--
-- ObjC selector: @- signature@
signature :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
signature asPasskeyAssertionCredential  =
    sendMsg asPasskeyAssertionCredential (mkSelector "signature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The hash of the client data for this assertion result.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
clientDataHash asPasskeyAssertionCredential  =
    sendMsg asPasskeyAssertionCredential (mkSelector "clientDataHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The authenticator data of the application that created this credential.
--
-- ObjC selector: @- authenticatorData@
authenticatorData :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
authenticatorData asPasskeyAssertionCredential  =
    sendMsg asPasskeyAssertionCredential (mkSelector "authenticatorData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The raw credential ID for this passkey credential.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id NSData)
credentialID asPasskeyAssertionCredential  =
    sendMsg asPasskeyAssertionCredential (mkSelector "credentialID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The outputs of WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- extensionOutput@
extensionOutput :: IsASPasskeyAssertionCredential asPasskeyAssertionCredential => asPasskeyAssertionCredential -> IO (Id ASPasskeyAssertionCredentialExtensionOutput)
extensionOutput asPasskeyAssertionCredential  =
    sendMsg asPasskeyAssertionCredential (mkSelector "extensionOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The outputs of WebAuthn extensions processed by the credential provider.
--
-- ObjC selector: @- setExtensionOutput:@
setExtensionOutput :: (IsASPasskeyAssertionCredential asPasskeyAssertionCredential, IsASPasskeyAssertionCredentialExtensionOutput value) => asPasskeyAssertionCredential -> value -> IO ()
setExtensionOutput asPasskeyAssertionCredential  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asPasskeyAssertionCredential (mkSelector "setExtensionOutput:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:@
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector :: Selector
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector = mkSelector "initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:"

-- | @Selector@ for @initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:extensionOutput:@
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutputSelector :: Selector
initWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialID_extensionOutputSelector = mkSelector "initWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:extensionOutput:"

-- | @Selector@ for @credentialWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:@
credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector :: Selector
credentialWithUserHandle_relyingParty_signature_clientDataHash_authenticatorData_credentialIDSelector = mkSelector "credentialWithUserHandle:relyingParty:signature:clientDataHash:authenticatorData:credentialID:"

-- | @Selector@ for @userHandle@
userHandleSelector :: Selector
userHandleSelector = mkSelector "userHandle"

-- | @Selector@ for @relyingParty@
relyingPartySelector :: Selector
relyingPartySelector = mkSelector "relyingParty"

-- | @Selector@ for @signature@
signatureSelector :: Selector
signatureSelector = mkSelector "signature"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @authenticatorData@
authenticatorDataSelector :: Selector
authenticatorDataSelector = mkSelector "authenticatorData"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @extensionOutput@
extensionOutputSelector :: Selector
extensionOutputSelector = mkSelector "extensionOutput"

-- | @Selector@ for @setExtensionOutput:@
setExtensionOutputSelector :: Selector
setExtensionOutputSelector = mkSelector "setExtensionOutput:"

