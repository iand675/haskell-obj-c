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
  , initWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector
  , initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutputSelector
  , credentialWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector
  , relyingPartySelector
  , clientDataHashSelector
  , credentialIDSelector
  , attestationObjectSelector


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
initWithRelyingParty_clientDataHash_credentialID_attestationObject asPasskeyRegistrationCredential  relyingParty clientDataHash credentialID attestationObject =
withObjCPtr relyingParty $ \raw_relyingParty ->
  withObjCPtr clientDataHash $ \raw_clientDataHash ->
    withObjCPtr credentialID $ \raw_credentialID ->
      withObjCPtr attestationObject $ \raw_attestationObject ->
          sendMsg asPasskeyRegistrationCredential (mkSelector "initWithRelyingParty:clientDataHash:credentialID:attestationObject:") (retPtr retVoid) [argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ()), argPtr (castPtr raw_attestationObject :: Ptr ())] >>= ownedObject . castPtr

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
initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutput asPasskeyRegistrationCredential  relyingParty clientDataHash credentialID attestationObject extensionOutput =
withObjCPtr relyingParty $ \raw_relyingParty ->
  withObjCPtr clientDataHash $ \raw_clientDataHash ->
    withObjCPtr credentialID $ \raw_credentialID ->
      withObjCPtr attestationObject $ \raw_attestationObject ->
        withObjCPtr extensionOutput $ \raw_extensionOutput ->
            sendMsg asPasskeyRegistrationCredential (mkSelector "initWithRelyingParty:clientDataHash:credentialID:attestationObject:extensionOutput:") (retPtr retVoid) [argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ()), argPtr (castPtr raw_attestationObject :: Ptr ()), argPtr (castPtr raw_extensionOutput :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr relyingParty $ \raw_relyingParty ->
      withObjCPtr clientDataHash $ \raw_clientDataHash ->
        withObjCPtr credentialID $ \raw_credentialID ->
          withObjCPtr attestationObject $ \raw_attestationObject ->
            sendClassMsg cls' (mkSelector "credentialWithRelyingParty:clientDataHash:credentialID:attestationObject:") (retPtr retVoid) [argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ()), argPtr (castPtr raw_attestationObject :: Ptr ())] >>= retainedObject . castPtr

-- | The relying party identifier associated with this passkey.
--
-- ObjC selector: @- relyingParty@
relyingParty :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSString)
relyingParty asPasskeyRegistrationCredential  =
  sendMsg asPasskeyRegistrationCredential (mkSelector "relyingParty") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The hash of the client data for this registration result.
--
-- ObjC selector: @- clientDataHash@
clientDataHash :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSData)
clientDataHash asPasskeyRegistrationCredential  =
  sendMsg asPasskeyRegistrationCredential (mkSelector "clientDataHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The raw credential identifier of this passkey.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSData)
credentialID asPasskeyRegistrationCredential  =
  sendMsg asPasskeyRegistrationCredential (mkSelector "credentialID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The attestation object for this passkey registration result.
--
-- ObjC selector: @- attestationObject@
attestationObject :: IsASPasskeyRegistrationCredential asPasskeyRegistrationCredential => asPasskeyRegistrationCredential -> IO (Id NSData)
attestationObject asPasskeyRegistrationCredential  =
  sendMsg asPasskeyRegistrationCredential (mkSelector "attestationObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRelyingParty:clientDataHash:credentialID:attestationObject:@
initWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector :: Selector
initWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector = mkSelector "initWithRelyingParty:clientDataHash:credentialID:attestationObject:"

-- | @Selector@ for @initWithRelyingParty:clientDataHash:credentialID:attestationObject:extensionOutput:@
initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutputSelector :: Selector
initWithRelyingParty_clientDataHash_credentialID_attestationObject_extensionOutputSelector = mkSelector "initWithRelyingParty:clientDataHash:credentialID:attestationObject:extensionOutput:"

-- | @Selector@ for @credentialWithRelyingParty:clientDataHash:credentialID:attestationObject:@
credentialWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector :: Selector
credentialWithRelyingParty_clientDataHash_credentialID_attestationObjectSelector = mkSelector "credentialWithRelyingParty:clientDataHash:credentialID:attestationObject:"

-- | @Selector@ for @relyingParty@
relyingPartySelector :: Selector
relyingPartySelector = mkSelector "relyingParty"

-- | @Selector@ for @clientDataHash@
clientDataHashSelector :: Selector
clientDataHashSelector = mkSelector "clientDataHash"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @attestationObject@
attestationObjectSelector :: Selector
attestationObjectSelector = mkSelector "attestationObject"

