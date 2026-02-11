{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationProviderExtensionLoginManager@.
module ObjC.AuthenticationServices.ASAuthorizationProviderExtensionLoginManager
  ( ASAuthorizationProviderExtensionLoginManager
  , IsASAuthorizationProviderExtensionLoginManager(..)
  , init_
  , new
  , saveUserLoginConfiguration_error
  , saveLoginConfiguration_error
  , saveCertificate_keyType
  , copyKeyForKeyType
  , copyIdentityForKeyType
  , beginKeyRotationForKeyType
  , completeKeyRotationForKeyType
  , userNeedsReauthenticationWithCompletion
  , deviceRegistrationsNeedsRepair
  , userRegistrationsNeedsRepair
  , decryptionKeysNeedRepair
  , resetKeys
  , resetDeviceKeys
  , resetUserSecureEnclaveKey
  , attestKey_clientDataHash_completion
  , attestPendingKey_clientDataHash_completion
  , presentRegistrationViewControllerWithCompletion
  , deviceRegistered
  , userRegistered
  , registrationToken
  , authenticationMethod
  , ssoTokens
  , setSsoTokens
  , loginConfiguration
  , initSelector
  , newSelector
  , saveUserLoginConfiguration_errorSelector
  , saveLoginConfiguration_errorSelector
  , saveCertificate_keyTypeSelector
  , copyKeyForKeyTypeSelector
  , copyIdentityForKeyTypeSelector
  , beginKeyRotationForKeyTypeSelector
  , completeKeyRotationForKeyTypeSelector
  , userNeedsReauthenticationWithCompletionSelector
  , deviceRegistrationsNeedsRepairSelector
  , userRegistrationsNeedsRepairSelector
  , decryptionKeysNeedRepairSelector
  , resetKeysSelector
  , resetDeviceKeysSelector
  , resetUserSecureEnclaveKeySelector
  , attestKey_clientDataHash_completionSelector
  , attestPendingKey_clientDataHash_completionSelector
  , presentRegistrationViewControllerWithCompletionSelector
  , deviceRegisteredSelector
  , userRegisteredSelector
  , registrationTokenSelector
  , authenticationMethodSelector
  , ssoTokensSelector
  , setSsoTokensSelector
  , loginConfigurationSelector

  -- * Enum types
  , ASAuthorizationProviderExtensionAuthenticationMethod(ASAuthorizationProviderExtensionAuthenticationMethod)
  , pattern ASAuthorizationProviderExtensionAuthenticationMethodPassword
  , pattern ASAuthorizationProviderExtensionAuthenticationMethodUserSecureEnclaveKey
  , pattern ASAuthorizationProviderExtensionAuthenticationMethodSmartCard
  , ASAuthorizationProviderExtensionKeyType(ASAuthorizationProviderExtensionKeyType)
  , pattern ASAuthorizationProviderExtensionKeyTypeUserDeviceSigning
  , pattern ASAuthorizationProviderExtensionKeyTypeUserDeviceEncryption
  , pattern ASAuthorizationProviderExtensionKeyTypeUserSecureEnclaveKey
  , pattern ASAuthorizationProviderExtensionKeyTypeSharedDeviceSigning
  , pattern ASAuthorizationProviderExtensionKeyTypeSharedDeviceEncryption
  , pattern ASAuthorizationProviderExtensionKeyTypeCurrentDeviceSigning
  , pattern ASAuthorizationProviderExtensionKeyTypeCurrentDeviceEncryption
  , pattern ASAuthorizationProviderExtensionKeyTypeUserSmartCard

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
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO (Id ASAuthorizationProviderExtensionLoginManager)
init_ asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationProviderExtensionLoginManager)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationProviderExtensionLoginManager"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Saves or replaces the user login configration.
--
-- @userLoginConfiguration@ — The user login configration to use.
--
-- @error@ — The error when there are validation errors or nil.
--
-- ObjC selector: @- saveUserLoginConfiguration:error:@
saveUserLoginConfiguration_error :: (IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager, IsASAuthorizationProviderExtensionUserLoginConfiguration userLoginConfiguration, IsNSError error_) => asAuthorizationProviderExtensionLoginManager -> userLoginConfiguration -> error_ -> IO Bool
saveUserLoginConfiguration_error asAuthorizationProviderExtensionLoginManager  userLoginConfiguration error_ =
withObjCPtr userLoginConfiguration $ \raw_userLoginConfiguration ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "saveUserLoginConfiguration:error:") retCULong [argPtr (castPtr raw_userLoginConfiguration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Saves or replaces the login configration.
--
-- @loginConfiguration@ — The login configration to use.
--
-- @error@ — The error when there are validation errors or nil.
--
-- ObjC selector: @- saveLoginConfiguration:error:@
saveLoginConfiguration_error :: (IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager, IsASAuthorizationProviderExtensionLoginConfiguration loginConfiguration, IsNSError error_) => asAuthorizationProviderExtensionLoginManager -> loginConfiguration -> error_ -> IO Bool
saveLoginConfiguration_error asAuthorizationProviderExtensionLoginManager  loginConfiguration error_ =
withObjCPtr loginConfiguration $ \raw_loginConfiguration ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "saveLoginConfiguration:error:") retCULong [argPtr (castPtr raw_loginConfiguration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Saves the provided certificate for the key type.
--
-- @certificate@ — The certificate to save.
--
-- @keyType@ — The key type for the certificate.
--
-- ObjC selector: @- saveCertificate:keyType:@
saveCertificate_keyType :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> Ptr () -> ASAuthorizationProviderExtensionKeyType -> IO ()
saveCertificate_keyType asAuthorizationProviderExtensionLoginManager  certificate keyType =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "saveCertificate:keyType:") retVoid [argPtr certificate, argCLong (coerce keyType)]

-- | Retrieves the key for the specified platform SSO key type.
--
-- @keyType@ — The key type to retrieve.
--
-- ObjC selector: @- copyKeyForKeyType:@
copyKeyForKeyType :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> ASAuthorizationProviderExtensionKeyType -> IO (Ptr ())
copyKeyForKeyType asAuthorizationProviderExtensionLoginManager  keyType =
  fmap castPtr $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "copyKeyForKeyType:") (retPtr retVoid) [argCLong (coerce keyType)]

-- | Retrieves the identity for the specified platform SSO key type.
--
-- @keyType@ — The key type to retrieve.
--
-- ObjC selector: @- copyIdentityForKeyType:@
copyIdentityForKeyType :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> ASAuthorizationProviderExtensionKeyType -> IO (Ptr ())
copyIdentityForKeyType asAuthorizationProviderExtensionLoginManager  keyType =
  fmap castPtr $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "copyIdentityForKeyType:") (retPtr retVoid) [argCLong (coerce keyType)]

-- | Generates a new key for the specified platform SSO key type using the strongest supported key strength returning the new key.  Nil is returned if there is an error generating the new key.
--
-- @keyType@ — The key type to retrieve.
--
-- ObjC selector: @- beginKeyRotationForKeyType:@
beginKeyRotationForKeyType :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> ASAuthorizationProviderExtensionKeyType -> IO (Ptr ())
beginKeyRotationForKeyType asAuthorizationProviderExtensionLoginManager  keyType =
  fmap castPtr $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "beginKeyRotationForKeyType:") (retPtr retVoid) [argCLong (coerce keyType)]

-- | Completes rotation for the key to replace the previous key.
--
-- @keyType@ — The key type to retrieve.
--
-- ObjC selector: @- completeKeyRotationForKeyType:@
completeKeyRotationForKeyType :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> ASAuthorizationProviderExtensionKeyType -> IO ()
completeKeyRotationForKeyType asAuthorizationProviderExtensionLoginManager  keyType =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "completeKeyRotationForKeyType:") retVoid [argCLong (coerce keyType)]

-- | Requests AppSSOAgent reauthenticate the current user for the current extension.  This is used when the tokens are revoked, or expired and need to be requested again.
--
-- ObjC selector: @- userNeedsReauthenticationWithCompletion:@
userNeedsReauthenticationWithCompletion :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> Ptr () -> IO ()
userNeedsReauthenticationWithCompletion asAuthorizationProviderExtensionLoginManager  completion =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "userNeedsReauthenticationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Requests that the device registration be run again to repair it.
--
-- ObjC selector: @- deviceRegistrationsNeedsRepair@
deviceRegistrationsNeedsRepair :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO ()
deviceRegistrationsNeedsRepair asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "deviceRegistrationsNeedsRepair") retVoid []

-- | Requests that user registration be run again for the current user to repair it.
--
-- ObjC selector: @- userRegistrationsNeedsRepair@
userRegistrationsNeedsRepair :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO ()
userRegistrationsNeedsRepair asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "userRegistrationsNeedsRepair") retVoid []

-- | Requests that the decryption keys are repaired.
--
-- ObjC selector: @- decryptionKeysNeedRepair@
decryptionKeysNeedRepair :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO ()
decryptionKeysNeedRepair asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "decryptionKeysNeedRepair") retVoid []

-- | Creates new Encryption, Signing, and Secure Enclave keys for the user.  The old keys will be destroyed.
--
-- ObjC selector: @- resetKeys@
resetKeys :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO ()
resetKeys asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "resetKeys") retVoid []

-- | Creates new Encryption, and Signing keys for the device or user.  The old keys will be destroyed.
--
-- ObjC selector: @- resetDeviceKeys@
resetDeviceKeys :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO ()
resetDeviceKeys asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "resetDeviceKeys") retVoid []

-- | Creates new Encryption, Signing, and Secure Enclave keys for the user.  The old keys will be destroyed.
--
-- ObjC selector: @- resetUserSecureEnclaveKey@
resetUserSecureEnclaveKey :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO ()
resetUserSecureEnclaveKey asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "resetUserSecureEnclaveKey") retVoid []

-- | Provides a new or cached attestation for the specified key type.
--
-- @keyType@ — The key type for the attestation.
--
-- @clientDataHash@ — A SHA256 hash of a unique, single-use data block that embeds a challenge from your server.
--
-- @completion@ — A closure that the method calls upon completion with the following parameters:  * attestationCertificates An array of certificates that verify the validity of the key associated with the keyType. Send this to your server for processing.  * error A DCError instance that indicates the reason for failure, or nil on success.
--
-- ObjC selector: @- attestKey:clientDataHash:completion:@
attestKey_clientDataHash_completion :: (IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager, IsNSData clientDataHash) => asAuthorizationProviderExtensionLoginManager -> ASAuthorizationProviderExtensionKeyType -> clientDataHash -> Ptr () -> IO ()
attestKey_clientDataHash_completion asAuthorizationProviderExtensionLoginManager  keyType clientDataHash completion =
withObjCPtr clientDataHash $ \raw_clientDataHash ->
    sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "attestKey:clientDataHash:completion:") retVoid [argCLong (coerce keyType), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Provides a new or cached attestation for the specified pending key type.
--
-- @keyType@ — The pending key type for the attestation.
--
-- @clientDataHash@ — A SHA256 hash of a unique, single-use data block that embeds a challenge from your server.
--
-- @completion@ — A closure that the method calls upon completion with the following parameters:  * attestationCertificates An array of certificates that verify the validity of the pending key associated with the keyType. Send this to your server for processing.  * error A DCError instance that indicates the reason for failure, or nil on success.
--
-- ObjC selector: @- attestPendingKey:clientDataHash:completion:@
attestPendingKey_clientDataHash_completion :: (IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager, IsNSData clientDataHash) => asAuthorizationProviderExtensionLoginManager -> ASAuthorizationProviderExtensionKeyType -> clientDataHash -> Ptr () -> IO ()
attestPendingKey_clientDataHash_completion asAuthorizationProviderExtensionLoginManager  keyType clientDataHash completion =
withObjCPtr clientDataHash $ \raw_clientDataHash ->
    sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "attestPendingKey:clientDataHash:completion:") retVoid [argCLong (coerce keyType), argPtr (castPtr raw_clientDataHash :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Asks authorization service to show extension view controller for registration. If the controller cannot be shown an error is returned.  This is only valid during registration.
--
-- ObjC selector: @- presentRegistrationViewControllerWithCompletion:@
presentRegistrationViewControllerWithCompletion :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> Ptr () -> IO ()
presentRegistrationViewControllerWithCompletion asAuthorizationProviderExtensionLoginManager  completion =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "presentRegistrationViewControllerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Returns YES if the current device completed registration.
--
-- ObjC selector: @- deviceRegistered@
deviceRegistered :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO Bool
deviceRegistered asAuthorizationProviderExtensionLoginManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "deviceRegistered") retCULong []

-- | Returns YES if current user completed registration.
--
-- ObjC selector: @- userRegistered@
userRegistered :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO Bool
userRegistered asAuthorizationProviderExtensionLoginManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "userRegistered") retCULong []

-- | Returns the device registration token from the MDM profile.
--
-- ObjC selector: @- registrationToken@
registrationToken :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO (Id NSString)
registrationToken asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "registrationToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the authentication method used for the device.
--
-- ObjC selector: @- authenticationMethod@
authenticationMethod :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO ASAuthorizationProviderExtensionAuthenticationMethod
authenticationMethod asAuthorizationProviderExtensionLoginManager  =
  fmap (coerce :: CLong -> ASAuthorizationProviderExtensionAuthenticationMethod) $ sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "authenticationMethod") retCLong []

-- | Retrieves or sets the current SSO tokens response for the current user and extension.
--
-- ObjC selector: @- ssoTokens@
ssoTokens :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO (Id NSDictionary)
ssoTokens asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "ssoTokens") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Retrieves or sets the current SSO tokens response for the current user and extension.
--
-- ObjC selector: @- setSsoTokens:@
setSsoTokens :: (IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager, IsNSDictionary value) => asAuthorizationProviderExtensionLoginManager -> value -> IO ()
setSsoTokens asAuthorizationProviderExtensionLoginManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "setSsoTokens:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Retrieves or sets the current login configuration for the extension.
--
-- ObjC selector: @- loginConfiguration@
loginConfiguration :: IsASAuthorizationProviderExtensionLoginManager asAuthorizationProviderExtensionLoginManager => asAuthorizationProviderExtensionLoginManager -> IO (Id ASAuthorizationProviderExtensionLoginConfiguration)
loginConfiguration asAuthorizationProviderExtensionLoginManager  =
  sendMsg asAuthorizationProviderExtensionLoginManager (mkSelector "loginConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @saveUserLoginConfiguration:error:@
saveUserLoginConfiguration_errorSelector :: Selector
saveUserLoginConfiguration_errorSelector = mkSelector "saveUserLoginConfiguration:error:"

-- | @Selector@ for @saveLoginConfiguration:error:@
saveLoginConfiguration_errorSelector :: Selector
saveLoginConfiguration_errorSelector = mkSelector "saveLoginConfiguration:error:"

-- | @Selector@ for @saveCertificate:keyType:@
saveCertificate_keyTypeSelector :: Selector
saveCertificate_keyTypeSelector = mkSelector "saveCertificate:keyType:"

-- | @Selector@ for @copyKeyForKeyType:@
copyKeyForKeyTypeSelector :: Selector
copyKeyForKeyTypeSelector = mkSelector "copyKeyForKeyType:"

-- | @Selector@ for @copyIdentityForKeyType:@
copyIdentityForKeyTypeSelector :: Selector
copyIdentityForKeyTypeSelector = mkSelector "copyIdentityForKeyType:"

-- | @Selector@ for @beginKeyRotationForKeyType:@
beginKeyRotationForKeyTypeSelector :: Selector
beginKeyRotationForKeyTypeSelector = mkSelector "beginKeyRotationForKeyType:"

-- | @Selector@ for @completeKeyRotationForKeyType:@
completeKeyRotationForKeyTypeSelector :: Selector
completeKeyRotationForKeyTypeSelector = mkSelector "completeKeyRotationForKeyType:"

-- | @Selector@ for @userNeedsReauthenticationWithCompletion:@
userNeedsReauthenticationWithCompletionSelector :: Selector
userNeedsReauthenticationWithCompletionSelector = mkSelector "userNeedsReauthenticationWithCompletion:"

-- | @Selector@ for @deviceRegistrationsNeedsRepair@
deviceRegistrationsNeedsRepairSelector :: Selector
deviceRegistrationsNeedsRepairSelector = mkSelector "deviceRegistrationsNeedsRepair"

-- | @Selector@ for @userRegistrationsNeedsRepair@
userRegistrationsNeedsRepairSelector :: Selector
userRegistrationsNeedsRepairSelector = mkSelector "userRegistrationsNeedsRepair"

-- | @Selector@ for @decryptionKeysNeedRepair@
decryptionKeysNeedRepairSelector :: Selector
decryptionKeysNeedRepairSelector = mkSelector "decryptionKeysNeedRepair"

-- | @Selector@ for @resetKeys@
resetKeysSelector :: Selector
resetKeysSelector = mkSelector "resetKeys"

-- | @Selector@ for @resetDeviceKeys@
resetDeviceKeysSelector :: Selector
resetDeviceKeysSelector = mkSelector "resetDeviceKeys"

-- | @Selector@ for @resetUserSecureEnclaveKey@
resetUserSecureEnclaveKeySelector :: Selector
resetUserSecureEnclaveKeySelector = mkSelector "resetUserSecureEnclaveKey"

-- | @Selector@ for @attestKey:clientDataHash:completion:@
attestKey_clientDataHash_completionSelector :: Selector
attestKey_clientDataHash_completionSelector = mkSelector "attestKey:clientDataHash:completion:"

-- | @Selector@ for @attestPendingKey:clientDataHash:completion:@
attestPendingKey_clientDataHash_completionSelector :: Selector
attestPendingKey_clientDataHash_completionSelector = mkSelector "attestPendingKey:clientDataHash:completion:"

-- | @Selector@ for @presentRegistrationViewControllerWithCompletion:@
presentRegistrationViewControllerWithCompletionSelector :: Selector
presentRegistrationViewControllerWithCompletionSelector = mkSelector "presentRegistrationViewControllerWithCompletion:"

-- | @Selector@ for @deviceRegistered@
deviceRegisteredSelector :: Selector
deviceRegisteredSelector = mkSelector "deviceRegistered"

-- | @Selector@ for @userRegistered@
userRegisteredSelector :: Selector
userRegisteredSelector = mkSelector "userRegistered"

-- | @Selector@ for @registrationToken@
registrationTokenSelector :: Selector
registrationTokenSelector = mkSelector "registrationToken"

-- | @Selector@ for @authenticationMethod@
authenticationMethodSelector :: Selector
authenticationMethodSelector = mkSelector "authenticationMethod"

-- | @Selector@ for @ssoTokens@
ssoTokensSelector :: Selector
ssoTokensSelector = mkSelector "ssoTokens"

-- | @Selector@ for @setSsoTokens:@
setSsoTokensSelector :: Selector
setSsoTokensSelector = mkSelector "setSsoTokens:"

-- | @Selector@ for @loginConfiguration@
loginConfigurationSelector :: Selector
loginConfigurationSelector = mkSelector "loginConfiguration"

