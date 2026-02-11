{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASCredentialProviderViewController@.
module ObjC.AuthenticationServices.ASCredentialProviderViewController
  ( ASCredentialProviderViewController
  , IsASCredentialProviderViewController(..)
  , prepareCredentialListForServiceIdentifiers
  , prepareCredentialListForServiceIdentifiers_requestParameters
  , prepareOneTimeCodeCredentialListForServiceIdentifiers
  , provideCredentialWithoutUserInteractionForIdentity
  , provideCredentialWithoutUserInteractionForRequest
  , prepareInterfaceToProvideCredentialForIdentity
  , prepareInterfaceToProvideCredentialForRequest
  , prepareInterfaceForExtensionConfiguration
  , prepareInterfaceForPasskeyRegistration
  , performPasskeyRegistrationWithoutUserInteractionIfPossible
  , reportPublicKeyCredentialUpdateForRelyingParty_userHandle_newName
  , reportUnknownPublicKeyCredentialForRelyingParty_credentialID
  , reportAllAcceptedPublicKeyCredentialsForRelyingParty_userHandle_acceptedCredentialIDs
  , reportUnusedPasswordCredentialForDomain_userName
  , performSavePasswordRequestWithoutUserInteractionIfPossible
  , prepareInterfaceForSavePasswordRequest
  , performGeneratePasswordsRequestWithoutUserInteraction
  , prepareInterfaceForGeneratePasswordsRequest
  , extensionContext
  , prepareCredentialListForServiceIdentifiersSelector
  , prepareCredentialListForServiceIdentifiers_requestParametersSelector
  , prepareOneTimeCodeCredentialListForServiceIdentifiersSelector
  , provideCredentialWithoutUserInteractionForIdentitySelector
  , provideCredentialWithoutUserInteractionForRequestSelector
  , prepareInterfaceToProvideCredentialForIdentitySelector
  , prepareInterfaceToProvideCredentialForRequestSelector
  , prepareInterfaceForExtensionConfigurationSelector
  , prepareInterfaceForPasskeyRegistrationSelector
  , performPasskeyRegistrationWithoutUserInteractionIfPossibleSelector
  , reportPublicKeyCredentialUpdateForRelyingParty_userHandle_newNameSelector
  , reportUnknownPublicKeyCredentialForRelyingParty_credentialIDSelector
  , reportAllAcceptedPublicKeyCredentialsForRelyingParty_userHandle_acceptedCredentialIDsSelector
  , reportUnusedPasswordCredentialForDomain_userNameSelector
  , performSavePasswordRequestWithoutUserInteractionIfPossibleSelector
  , prepareInterfaceForSavePasswordRequestSelector
  , performGeneratePasswordsRequestWithoutUserInteractionSelector
  , prepareInterfaceForGeneratePasswordsRequestSelector
  , extensionContextSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Prepare the view controller to show a list of credentials.
--
-- @serviceIdentifiers@ — the array of service identifiers.
--
-- This method is called by the system to prepare the extension's view controller to present the list of credentials. A service identifier array is passed which can be used to filter or prioritize the credentials that closely match each service. The service identifier array could have zero or more items. If there are more than one item in the array, items with lower indexes represent more specific identifiers for which a credential is being requested. For example, the array could contain identifiers [m.example.com, example.com] with the first item representing the more specifc service that requires a credential. If the array of service identifiers is empty, it is expected that the credential list should still show credentials that the user can pick from.
--
-- ObjC selector: @- prepareCredentialListForServiceIdentifiers:@
prepareCredentialListForServiceIdentifiers :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsNSArray serviceIdentifiers) => asCredentialProviderViewController -> serviceIdentifiers -> IO ()
prepareCredentialListForServiceIdentifiers asCredentialProviderViewController  serviceIdentifiers =
withObjCPtr serviceIdentifiers $ \raw_serviceIdentifiers ->
    sendMsg asCredentialProviderViewController (mkSelector "prepareCredentialListForServiceIdentifiers:") retVoid [argPtr (castPtr raw_serviceIdentifiers :: Ptr ())]

-- | Prepare the view controller to show a list of passkey and password credentials.
--
-- @serviceIdentifiers@ — the array of service identifiers.
--
-- @requestParameters@ — the parameters of the active passkey request.
--
-- This method is called by the system to prepare the extension's view controller to present the list of credentials. A service identifier array is passed which can be used to filter or prioritize the credentials that closely match each service. The service identifier array could have zero or more items. If there is more than one item in the array, items with lower indexes represent more specific identifiers for which a credential is being requested. For example, the array could contain identifiers [m.example.com, example.com] with the first item representing the more specifc service that requires a credential. If the array of service identifiers is empty, it is expected that the credential list should still show credentials that the user can pick from. If a passkey credential is selected, the extension should use the requestParameters object to complete the request using the selected passkey credential.
--
-- ObjC selector: @- prepareCredentialListForServiceIdentifiers:requestParameters:@
prepareCredentialListForServiceIdentifiers_requestParameters :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsNSArray serviceIdentifiers, IsASPasskeyCredentialRequestParameters requestParameters) => asCredentialProviderViewController -> serviceIdentifiers -> requestParameters -> IO ()
prepareCredentialListForServiceIdentifiers_requestParameters asCredentialProviderViewController  serviceIdentifiers requestParameters =
withObjCPtr serviceIdentifiers $ \raw_serviceIdentifiers ->
  withObjCPtr requestParameters $ \raw_requestParameters ->
      sendMsg asCredentialProviderViewController (mkSelector "prepareCredentialListForServiceIdentifiers:requestParameters:") retVoid [argPtr (castPtr raw_serviceIdentifiers :: Ptr ()), argPtr (castPtr raw_requestParameters :: Ptr ())]

-- | Prepare the view controller to show a list of one time code credentials.
--
-- @serviceIdentifiers@ — the array of service identifiers.
--
-- This method is called by the system to prepare the extension's view controller to present the list of credentials. A service identifier array is passed which can be used to filter or prioritize the credentials that closely match each service. The service identifier array could have zero or more items. If there is more than one item in the array, items with lower indexes represent more specific identifiers for which a credential is being requested. For example, the array could contain identifiers [m.example.com, example.com] with the first item representing the more specifc service that requires a credential. If the array of service identifiers is empty, it is expected that the credential list should still show credentials that the user can pick from.
--
-- ObjC selector: @- prepareOneTimeCodeCredentialListForServiceIdentifiers:@
prepareOneTimeCodeCredentialListForServiceIdentifiers :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsNSArray serviceIdentifiers) => asCredentialProviderViewController -> serviceIdentifiers -> IO ()
prepareOneTimeCodeCredentialListForServiceIdentifiers asCredentialProviderViewController  serviceIdentifiers =
withObjCPtr serviceIdentifiers $ \raw_serviceIdentifiers ->
    sendMsg asCredentialProviderViewController (mkSelector "prepareOneTimeCodeCredentialListForServiceIdentifiers:") retVoid [argPtr (castPtr raw_serviceIdentifiers :: Ptr ())]

-- | Attempt to provide the user-requested credential without any user interaction.
--
-- @credentialIdentity@ — the credential identity for which a credential should be provided.
--
-- After the user selects a credential identity, the system may ask your extension to provide the credential without showing any user interface if possible to enhance the user experience. If your extension can accomplish this (for example, the user’s passwords database is still unlocked from a recent interaction), call -[ASCredentialProviderExtensionContext completeRequestWithSelectedCredential:completionHandler:] to provide the credential. If an error occurs, call -[ASCredentialProviderExtensionContext cancelRequestWithError:] and pass an error with domain ASExtensionErrorDomain and an appropriate error code from ASExtensionErrorCode.  For example, if your extension requires user interaction because the passwords database needs to be unlocked, pass an error with code ASExtensionErrorCodeUserInteractionRequired.
--
-- Note: When this method is called, your extension's view controller is not present on the screen. Do not attempt or expect to show any user interface in this method.
--
-- ObjC selector: @- provideCredentialWithoutUserInteractionForIdentity:@
provideCredentialWithoutUserInteractionForIdentity :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsASPasswordCredentialIdentity credentialIdentity) => asCredentialProviderViewController -> credentialIdentity -> IO ()
provideCredentialWithoutUserInteractionForIdentity asCredentialProviderViewController  credentialIdentity =
withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
    sendMsg asCredentialProviderViewController (mkSelector "provideCredentialWithoutUserInteractionForIdentity:") retVoid [argPtr (castPtr raw_credentialIdentity :: Ptr ())]

-- | Attempt to provide the user-requested credential without any user interaction.
--
-- After the user selects a credential identity, the system will create a credential request, the contents of which will depend on whether the credential to use is a password or passkey. The request type will match the type of credential that was requested. Refer to @ASPasswordCredentialRequest@, @ASPasskeyCredentialRequest@, and @ASOneTimeCodeCredentialRequest@ for details.
--
-- The system may ask your extension to provide the credential without showing any user interface if possible to enhance the user experience. If your extension can accomplish this (for example, the user’s passwords database is still unlocked from a recent interaction), call @-[ASCredentialProviderExtensionContext completeRequestWithSelectedCredential:completionHandler:]@ for password credentials, @-[ASCredentialProviderExtensionContext completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:]@ for passkey credentials, or @-[ASCredentialProviderExtensionContext completeOneTimeCodeRequestWithSelectedCredential:completionHandler:]@ for one time code credentials. If an error occurs, call @-[ASCredentialProviderExtensionContext cancelRequestWithError:]@ and pass an error with domain @ASExtensionErrorDomain@ and an appropriate error code from @ASExtensionErrorCode@. For example, if your extension requires user interaction because the passwords database needs to be unlocked, pass an error with code @ASExtensionErrorCodeUserInteractionRequired@.
--
-- In order for your extension to be presented in the list of options for passkey assertion requests, your extension needs to specify a true value for the Information Property List key @ProvidesPasskeys@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ ProvidesPasskeys => true
--
-- Similarly, your extension needs to specify a true value for the Information Property List key @ProvidesOneTimeCodes@ under the @ASCredentialProviderExtensionCapabilities@ dictionary in order to be presented in the list of options for one time code requests.
--
-- - Note: When this method is called, your extension's view controller is not present on the screen. Do not   attempt or expect to show any user interface in this method.
--
-- - Parameter credentialRequest: The credential request for which a credential should be provided.
--
-- ObjC selector: @- provideCredentialWithoutUserInteractionForRequest:@
provideCredentialWithoutUserInteractionForRequest :: IsASCredentialProviderViewController asCredentialProviderViewController => asCredentialProviderViewController -> RawId -> IO ()
provideCredentialWithoutUserInteractionForRequest asCredentialProviderViewController  credentialRequest =
  sendMsg asCredentialProviderViewController (mkSelector "provideCredentialWithoutUserInteractionForRequest:") retVoid [argPtr (castPtr (unRawId credentialRequest) :: Ptr ())]

-- | Prepare the view controller to show user interface for providing the user-requested credential.
--
-- @credentialIdentity@ — the credential identity for which a credential should be provided.
--
-- The system calls this method when your extension cannot provide the requested credential without user interaction. Set up the view controller for any user interaction required to provide the requested credential only. The user interaction should be limited in nature to operations required for providing the requested credential. An example is showing an authentication UI to unlock the user's passwords database. Call -[ASCredentialProviderExtensionContext completeRequestWithSelectedCredential:completionHandler:] to provide the credential. If an error occurs, call -[ASCredentialProviderExtensionContext cancelRequestWithError:] and pass an error with domain ASExtensionErrorDomain and an appropriate error code from ASExtensionErrorCode. For example, if the credential identity cannot be found in the database, pass an error with code ASExtensionErrorCodeCredentialIdentityNotFound.
--
-- ObjC selector: @- prepareInterfaceToProvideCredentialForIdentity:@
prepareInterfaceToProvideCredentialForIdentity :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsASPasswordCredentialIdentity credentialIdentity) => asCredentialProviderViewController -> credentialIdentity -> IO ()
prepareInterfaceToProvideCredentialForIdentity asCredentialProviderViewController  credentialIdentity =
withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
    sendMsg asCredentialProviderViewController (mkSelector "prepareInterfaceToProvideCredentialForIdentity:") retVoid [argPtr (castPtr raw_credentialIdentity :: Ptr ())]

-- | Prepare the view controller to show user interface for providing the user-requested credential.
--
-- @credentialRequest@ — the credential request for which a credential should be provided.
--
-- The system calls this method when your extension cannot provide the requested credential without user interaction. Set up the view controller for any user interaction required to provide the requested credential only. The user interaction should be limited in nature to operations required for providing the requested credential. An example is showing an authentication UI to unlock the user's passwords database. Call -[ASCredentialProviderExtensionContext completeRequestWithSelectedCredential:completionHandler:] for password credentials or -[ASCredentialProviderExtensionContext completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:] for passkey credentials. If an error occurs, call -[ASCredentialProviderExtensionContext cancelRequestWithError:] and pass an error with domain ASExtensionErrorDomain and an appropriate error code from ASExtensionErrorCode. For example, if the credential identity cannot be found in the database, pass an error with code ASExtensionErrorCodeCredentialIdentityNotFound.
--
-- ObjC selector: @- prepareInterfaceToProvideCredentialForRequest:@
prepareInterfaceToProvideCredentialForRequest :: IsASCredentialProviderViewController asCredentialProviderViewController => asCredentialProviderViewController -> RawId -> IO ()
prepareInterfaceToProvideCredentialForRequest asCredentialProviderViewController  credentialRequest =
  sendMsg asCredentialProviderViewController (mkSelector "prepareInterfaceToProvideCredentialForRequest:") retVoid [argPtr (castPtr (unRawId credentialRequest) :: Ptr ())]

-- | Prepare the view controller to show user interface when the user enables your extension.
--
-- The system calls this method after your extension is enabled by the user in Settings. You can use this method to give the user a chance to configure the extension or to provide credential identities to the system. After the configuration is done, call -[ASCredentialProviderExtensionContext completeExtensionConfigurationRequest].
--
-- Note: This method only gets called if your extension supports this functionality by specifying "ASCredentialProviderExtensionShowsConfigurationUI": YES in its extension attributes.
--
-- ObjC selector: @- prepareInterfaceForExtensionConfiguration@
prepareInterfaceForExtensionConfiguration :: IsASCredentialProviderViewController asCredentialProviderViewController => asCredentialProviderViewController -> IO ()
prepareInterfaceForExtensionConfiguration asCredentialProviderViewController  =
  sendMsg asCredentialProviderViewController (mkSelector "prepareInterfaceForExtensionConfiguration") retVoid []

-- | Prepare UI to register a passkey for the specified relying party.
--
-- The system calls this method when the user selects your extension to use for creating a passkey. In order for your extension to be presented in the list of options for passkey registration requests, your extension needs to specify a true value for the Information Property List key @ProvidesPasskeys@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ ProvidesPasskeys => true
--
-- This method will present your extension's UI for user authentication before creating the passkey. Once the passkey is created, your extension should call @-[ASCredentialProviderExtensionContext completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:]@ with the newly created ASPasskeyCredential object. If an error occurs, call @-[ASCredentialProviderExtensionContext cancelRequestWithError:]@ and pass an error with domain @ASExtensionErrorDomain@ and an appropriate error code from @ASExtensionErrorCode@.
--
-- - Parameter registrationRequest: The passkey registration request parameters needed to   register a new passkey.
--
-- ObjC selector: @- prepareInterfaceForPasskeyRegistration:@
prepareInterfaceForPasskeyRegistration :: IsASCredentialProviderViewController asCredentialProviderViewController => asCredentialProviderViewController -> RawId -> IO ()
prepareInterfaceForPasskeyRegistration asCredentialProviderViewController  registrationRequest =
  sendMsg asCredentialProviderViewController (mkSelector "prepareInterfaceForPasskeyRegistration:") retVoid [argPtr (castPtr (unRawId registrationRequest) :: Ptr ())]

-- | Perform a conditional passkey registration, if possible.
--
-- This method will be called for handling conditional passkey registration requests. A conditional passkey registration request allows your extension to opportunistically register passkeys in the background, if and only if you believe the user is in a good state to do so. Your extension decides can decide what conditions make sense for whether to fulfill or reject this request. For example, an extension may decide to register a passkey only if all of the following conditions are met: - The user's vault is currently unlocked. - The user name for the registration request matches that for an existing saved password. - The matching saved password was filled recently. - The user does not already have a passkey for this account.
--
-- Fulfilling this request should not remove a user's saved password for this account, but it may mean that the passkey will be preferred over the password in future AutoFill invocations, if both are supported.
--
-- Your extension should complete this request by calling @-[ASCredentialProviderExtensionContext completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:]@ or@-[ASCredentialProviderExtensionContext cancelRequestWithError:]@, like for standard registration requests. However, this request is not allowed to show UI and @ASExtensionErrorCodeUserInteractionRequired@ will be treated like any other error. The intent of this API is to provide a method of performing a background registration only where easy and convenient, so no blocking UI or error should ever be shown.
--
-- To indicate support for this feature, add @SupportsConditionalPasskeyRegistration@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsConditionalPasskeyRegistration => true
--
-- ObjC selector: @- performPasskeyRegistrationWithoutUserInteractionIfPossible:@
performPasskeyRegistrationWithoutUserInteractionIfPossible :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsASPasskeyCredentialRequest registrationRequest) => asCredentialProviderViewController -> registrationRequest -> IO ()
performPasskeyRegistrationWithoutUserInteractionIfPossible asCredentialProviderViewController  registrationRequest =
withObjCPtr registrationRequest $ \raw_registrationRequest ->
    sendMsg asCredentialProviderViewController (mkSelector "performPasskeyRegistrationWithoutUserInteractionIfPossible:") retVoid [argPtr (castPtr raw_registrationRequest :: Ptr ())]

-- | Receive report when a relying party indicates that a passkey's user name was updated.
--
-- This method will be called for handling passkey updates when a relying party reports an update using the @ASCredentialUpdater@ API. This update should be handled in the background, so no blocking UI or error should ever be shown.
--
-- - Parameter relyingParty: Relying party (website) that the credential is saved for. - Parameter userHandle: User identifier. - Parameter newName: The new user name for the credential.
--
-- To indicate support for this feature, add @SupportsCredentialUpdate@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsCredentialUpdate => true
--
-- ObjC selector: @- reportPublicKeyCredentialUpdateForRelyingParty:userHandle:newName:@
reportPublicKeyCredentialUpdateForRelyingParty_userHandle_newName :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsNSString relyingParty, IsNSData userHandle, IsNSString newName) => asCredentialProviderViewController -> relyingParty -> userHandle -> newName -> IO ()
reportPublicKeyCredentialUpdateForRelyingParty_userHandle_newName asCredentialProviderViewController  relyingParty userHandle newName =
withObjCPtr relyingParty $ \raw_relyingParty ->
  withObjCPtr userHandle $ \raw_userHandle ->
    withObjCPtr newName $ \raw_newName ->
        sendMsg asCredentialProviderViewController (mkSelector "reportPublicKeyCredentialUpdateForRelyingParty:userHandle:newName:") retVoid [argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_userHandle :: Ptr ()), argPtr (castPtr raw_newName :: Ptr ())]

-- | Receive report when a relying party indicates an invalid passkey credential.
--
-- This method will be called for handling passkey updates when a relying party reports the credential is no longer valid using the @ASCredentialUpdater@ API. You may hide or remove this credential. This update should be handled in the background, so no blocking UI or error should ever be shown.
--
-- - Parameter relyingParty: Relying party (website) that the credential is saved for. - Parameter credentialID: An identifier that uniquely identifies the passkey.
--
-- To indicate support for this feature, add @SupportsCredentialUpdate@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsCredentialUpdate => true
--
-- ObjC selector: @- reportUnknownPublicKeyCredentialForRelyingParty:credentialID:@
reportUnknownPublicKeyCredentialForRelyingParty_credentialID :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsNSString relyingParty, IsNSData credentialID) => asCredentialProviderViewController -> relyingParty -> credentialID -> IO ()
reportUnknownPublicKeyCredentialForRelyingParty_credentialID asCredentialProviderViewController  relyingParty credentialID =
withObjCPtr relyingParty $ \raw_relyingParty ->
  withObjCPtr credentialID $ \raw_credentialID ->
      sendMsg asCredentialProviderViewController (mkSelector "reportUnknownPublicKeyCredentialForRelyingParty:credentialID:") retVoid [argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ())]

-- | Receive report when relying party sends a snapshot of all the accepted credentials for an account.
--
-- This method will be called for handling passkey updates when a relying party sends a list of accepted credentials using the @ASCredentialUpdater@ API. You may hide or remove any credential not present in the accepted credentials list. This update should be handled in the background, so no blocking UI or error should ever be shown.
--
-- - Parameter relyingParty: Relying party (website) that the credential is saved for. - Parameter userHandle: User identifier. - Parameter acceptedCredentialIDs: An array of identifiers that uniquely identifies the accepted credentials.
--
-- To indicate support for this feature, add @SupportsCredentialUpdate@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsCredentialUpdate => true
--
-- ObjC selector: @- reportAllAcceptedPublicKeyCredentialsForRelyingParty:userHandle:acceptedCredentialIDs:@
reportAllAcceptedPublicKeyCredentialsForRelyingParty_userHandle_acceptedCredentialIDs :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsNSString relyingParty, IsNSData userHandle, IsNSArray acceptedCredentialIDs) => asCredentialProviderViewController -> relyingParty -> userHandle -> acceptedCredentialIDs -> IO ()
reportAllAcceptedPublicKeyCredentialsForRelyingParty_userHandle_acceptedCredentialIDs asCredentialProviderViewController  relyingParty userHandle acceptedCredentialIDs =
withObjCPtr relyingParty $ \raw_relyingParty ->
  withObjCPtr userHandle $ \raw_userHandle ->
    withObjCPtr acceptedCredentialIDs $ \raw_acceptedCredentialIDs ->
        sendMsg asCredentialProviderViewController (mkSelector "reportAllAcceptedPublicKeyCredentialsForRelyingParty:userHandle:acceptedCredentialIDs:") retVoid [argPtr (castPtr raw_relyingParty :: Ptr ()), argPtr (castPtr raw_userHandle :: Ptr ()), argPtr (castPtr raw_acceptedCredentialIDs :: Ptr ())]

-- | Receive report when relying party indicates a password credential is no longer needed for a given user name.
--
-- This method will be called for handling password credential updates when a relying party indicates a password is no longer needed using the @ASCredentialUpdater@ API. You may hide or remove the credential. This update should be handled in the background, so no blocking UI or error should ever be shown.
--
-- - Parameter domain: The website domain that the credential is saved for. - Parameter userName: The account user name.
--
-- To indicate support for this feature, add @SupportsCredentialUpdate@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsCredentialUpdate => true
--
-- ObjC selector: @- reportUnusedPasswordCredentialForDomain:userName:@
reportUnusedPasswordCredentialForDomain_userName :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsNSString domain, IsNSString userName) => asCredentialProviderViewController -> domain -> userName -> IO ()
reportUnusedPasswordCredentialForDomain_userName asCredentialProviderViewController  domain userName =
withObjCPtr domain $ \raw_domain ->
  withObjCPtr userName $ \raw_userName ->
      sendMsg asCredentialProviderViewController (mkSelector "reportUnusedPasswordCredentialForDomain:userName:") retVoid [argPtr (castPtr raw_domain :: Ptr ()), argPtr (castPtr raw_userName :: Ptr ())]

-- | Attempt to save a password credential.
--
-- To return results, you must call  ``ASCredentialProviderExtensionContext/completeSavePasswordRequest(completionHandler:)``. - Parameter request: The request to save a password. - Note: When this method is called, your extension's view controller is not present on the screen. You can request user interaction by calling ``ASCredentialProviderExtensionContext/cancelRequest(with:)``, using ``ASExtensionError/userInteractionRequired``.
--
-- To indicate support for this feature, add @SupportsSavePasswordCredentials@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsSavePasswordCredentials => true
--
-- ObjC selector: @- performSavePasswordRequestWithoutUserInteractionIfPossible:@
performSavePasswordRequestWithoutUserInteractionIfPossible :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsASSavePasswordRequest savePasswordRequest) => asCredentialProviderViewController -> savePasswordRequest -> IO ()
performSavePasswordRequestWithoutUserInteractionIfPossible asCredentialProviderViewController  savePasswordRequest =
withObjCPtr savePasswordRequest $ \raw_savePasswordRequest ->
    sendMsg asCredentialProviderViewController (mkSelector "performSavePasswordRequestWithoutUserInteractionIfPossible:") retVoid [argPtr (castPtr raw_savePasswordRequest :: Ptr ())]

-- | Prepares the interface to display a prompt to save a password credential.
--
-- The system calls this method to tell your extension’s view controller to prepare to present a prompt to save a password credential. After calling this method, the system presents the view controller to the user.
--
-- Upon success, call ``ASCredentialProviderExtensionContext/completeSavePasswordRequest(completionHandler:)``.
--
-- Always provide a way for someone to cancel the operation from your view controller, for example, by including a Cancel button in the navigation bar. When someone cancels the operation, call ``ASCredentialProviderExtensionContext/cancelRequest(with:)``, using ``ASExtensionError/userCanceled``. - Parameter request: The request to save a password.
--
-- ObjC selector: @- prepareInterfaceForSavePasswordRequest:@
prepareInterfaceForSavePasswordRequest :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsASSavePasswordRequest savePasswordRequest) => asCredentialProviderViewController -> savePasswordRequest -> IO ()
prepareInterfaceForSavePasswordRequest asCredentialProviderViewController  savePasswordRequest =
withObjCPtr savePasswordRequest $ \raw_savePasswordRequest ->
    sendMsg asCredentialProviderViewController (mkSelector "prepareInterfaceForSavePasswordRequest:") retVoid [argPtr (castPtr raw_savePasswordRequest :: Ptr ())]

-- | Attempt to generate passwords based on developer-specified rules.
--
-- To return results, you must call ``ASCredentialProviderExtensionContext/completeGeneratePasswordRequest(results:completionHandler:)`. - Parameter request: The request to generate a password. - Note: When this method is called, your extension's view controller is not present on the screen. ``ASExtensionError/userInteractionRequired`` will not be honored and treated as a failure. - Note: You should not update or replace any existing credentials when this API is called.
--
-- To indicate support for this feature, add @SupportsGeneratePasswordCredentials@ under the @ASCredentialProviderExtensionCapabilities@ dictionary.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsSavePasswordCredentials => true                 ├─ SupportsGeneratePasswordCredentials => true
--
-- ObjC selector: @- performGeneratePasswordsRequestWithoutUserInteraction:@
performGeneratePasswordsRequestWithoutUserInteraction :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsASGeneratePasswordsRequest generatePasswordsRequest) => asCredentialProviderViewController -> generatePasswordsRequest -> IO ()
performGeneratePasswordsRequestWithoutUserInteraction asCredentialProviderViewController  generatePasswordsRequest =
withObjCPtr generatePasswordsRequest $ \raw_generatePasswordsRequest ->
    sendMsg asCredentialProviderViewController (mkSelector "performGeneratePasswordsRequestWithoutUserInteraction:") retVoid [argPtr (castPtr raw_generatePasswordsRequest :: Ptr ())]

-- | Prepares the interface to display a prompt to generate passwords based on developer-specified rules.
--
-- The system calls this method to tell your extension’s view controller to prepare to present a prompt to generate passwords. After calling this method, the system presents the view controller to the user.
--
-- Upon success, call ``ASCredentialProviderExtensionContext/completeGeneratePasswordRequest(results:completionHandler:)``.
--
-- Always provide a way for someone to cancel the operation from your view controller, for example, by including a Cancel button in the navigation bar. When someone cancels the operation, call ``ASCredentialProviderExtensionContext/cancelRequest(with:)``, using ``ASExtensionError/userCanceled``. - Parameter request: The request to generate a password. - Note: This flow can only be initiated by the user. It will not be triggered from @-performGeneratePasswordsRequestWithoutUserInteraction:@ - Note: You should not update or replace any existing credentials when this API is called.
--
-- Support of this feature is implied by default when adding support for generating passwords. You can opt out of showing UI by adding the @SupportsGeneratePasswordCredentialsWithUI@ under the @ASCredentialProviderExtensionCapabilities@ dictionary. When opting out, you must provide a value of @false@ as the default value for this field is @true@. When you opt out, the OS will provide a relevant experience after requesting generated passwords from your extension.
--
-- Info.plist     ├─ NSExtension         ├─ NSExtensionAttributes             ├─ ASCredentialProviderExtensionCapabilities                 ├─ SupportsSavePasswordCredentials => true                 ├─ SupportsGeneratePasswordCredentials => true                 ├─ SupportsGeneratePasswordCredentialsWithUI => true
--
-- ObjC selector: @- prepareInterfaceForGeneratePasswordsRequest:@
prepareInterfaceForGeneratePasswordsRequest :: (IsASCredentialProviderViewController asCredentialProviderViewController, IsASGeneratePasswordsRequest generatePasswordsRequest) => asCredentialProviderViewController -> generatePasswordsRequest -> IO ()
prepareInterfaceForGeneratePasswordsRequest asCredentialProviderViewController  generatePasswordsRequest =
withObjCPtr generatePasswordsRequest $ \raw_generatePasswordsRequest ->
    sendMsg asCredentialProviderViewController (mkSelector "prepareInterfaceForGeneratePasswordsRequest:") retVoid [argPtr (castPtr raw_generatePasswordsRequest :: Ptr ())]

-- | @- extensionContext@
extensionContext :: IsASCredentialProviderViewController asCredentialProviderViewController => asCredentialProviderViewController -> IO (Id ASCredentialProviderExtensionContext)
extensionContext asCredentialProviderViewController  =
  sendMsg asCredentialProviderViewController (mkSelector "extensionContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareCredentialListForServiceIdentifiers:@
prepareCredentialListForServiceIdentifiersSelector :: Selector
prepareCredentialListForServiceIdentifiersSelector = mkSelector "prepareCredentialListForServiceIdentifiers:"

-- | @Selector@ for @prepareCredentialListForServiceIdentifiers:requestParameters:@
prepareCredentialListForServiceIdentifiers_requestParametersSelector :: Selector
prepareCredentialListForServiceIdentifiers_requestParametersSelector = mkSelector "prepareCredentialListForServiceIdentifiers:requestParameters:"

-- | @Selector@ for @prepareOneTimeCodeCredentialListForServiceIdentifiers:@
prepareOneTimeCodeCredentialListForServiceIdentifiersSelector :: Selector
prepareOneTimeCodeCredentialListForServiceIdentifiersSelector = mkSelector "prepareOneTimeCodeCredentialListForServiceIdentifiers:"

-- | @Selector@ for @provideCredentialWithoutUserInteractionForIdentity:@
provideCredentialWithoutUserInteractionForIdentitySelector :: Selector
provideCredentialWithoutUserInteractionForIdentitySelector = mkSelector "provideCredentialWithoutUserInteractionForIdentity:"

-- | @Selector@ for @provideCredentialWithoutUserInteractionForRequest:@
provideCredentialWithoutUserInteractionForRequestSelector :: Selector
provideCredentialWithoutUserInteractionForRequestSelector = mkSelector "provideCredentialWithoutUserInteractionForRequest:"

-- | @Selector@ for @prepareInterfaceToProvideCredentialForIdentity:@
prepareInterfaceToProvideCredentialForIdentitySelector :: Selector
prepareInterfaceToProvideCredentialForIdentitySelector = mkSelector "prepareInterfaceToProvideCredentialForIdentity:"

-- | @Selector@ for @prepareInterfaceToProvideCredentialForRequest:@
prepareInterfaceToProvideCredentialForRequestSelector :: Selector
prepareInterfaceToProvideCredentialForRequestSelector = mkSelector "prepareInterfaceToProvideCredentialForRequest:"

-- | @Selector@ for @prepareInterfaceForExtensionConfiguration@
prepareInterfaceForExtensionConfigurationSelector :: Selector
prepareInterfaceForExtensionConfigurationSelector = mkSelector "prepareInterfaceForExtensionConfiguration"

-- | @Selector@ for @prepareInterfaceForPasskeyRegistration:@
prepareInterfaceForPasskeyRegistrationSelector :: Selector
prepareInterfaceForPasskeyRegistrationSelector = mkSelector "prepareInterfaceForPasskeyRegistration:"

-- | @Selector@ for @performPasskeyRegistrationWithoutUserInteractionIfPossible:@
performPasskeyRegistrationWithoutUserInteractionIfPossibleSelector :: Selector
performPasskeyRegistrationWithoutUserInteractionIfPossibleSelector = mkSelector "performPasskeyRegistrationWithoutUserInteractionIfPossible:"

-- | @Selector@ for @reportPublicKeyCredentialUpdateForRelyingParty:userHandle:newName:@
reportPublicKeyCredentialUpdateForRelyingParty_userHandle_newNameSelector :: Selector
reportPublicKeyCredentialUpdateForRelyingParty_userHandle_newNameSelector = mkSelector "reportPublicKeyCredentialUpdateForRelyingParty:userHandle:newName:"

-- | @Selector@ for @reportUnknownPublicKeyCredentialForRelyingParty:credentialID:@
reportUnknownPublicKeyCredentialForRelyingParty_credentialIDSelector :: Selector
reportUnknownPublicKeyCredentialForRelyingParty_credentialIDSelector = mkSelector "reportUnknownPublicKeyCredentialForRelyingParty:credentialID:"

-- | @Selector@ for @reportAllAcceptedPublicKeyCredentialsForRelyingParty:userHandle:acceptedCredentialIDs:@
reportAllAcceptedPublicKeyCredentialsForRelyingParty_userHandle_acceptedCredentialIDsSelector :: Selector
reportAllAcceptedPublicKeyCredentialsForRelyingParty_userHandle_acceptedCredentialIDsSelector = mkSelector "reportAllAcceptedPublicKeyCredentialsForRelyingParty:userHandle:acceptedCredentialIDs:"

-- | @Selector@ for @reportUnusedPasswordCredentialForDomain:userName:@
reportUnusedPasswordCredentialForDomain_userNameSelector :: Selector
reportUnusedPasswordCredentialForDomain_userNameSelector = mkSelector "reportUnusedPasswordCredentialForDomain:userName:"

-- | @Selector@ for @performSavePasswordRequestWithoutUserInteractionIfPossible:@
performSavePasswordRequestWithoutUserInteractionIfPossibleSelector :: Selector
performSavePasswordRequestWithoutUserInteractionIfPossibleSelector = mkSelector "performSavePasswordRequestWithoutUserInteractionIfPossible:"

-- | @Selector@ for @prepareInterfaceForSavePasswordRequest:@
prepareInterfaceForSavePasswordRequestSelector :: Selector
prepareInterfaceForSavePasswordRequestSelector = mkSelector "prepareInterfaceForSavePasswordRequest:"

-- | @Selector@ for @performGeneratePasswordsRequestWithoutUserInteraction:@
performGeneratePasswordsRequestWithoutUserInteractionSelector :: Selector
performGeneratePasswordsRequestWithoutUserInteractionSelector = mkSelector "performGeneratePasswordsRequestWithoutUserInteraction:"

-- | @Selector@ for @prepareInterfaceForGeneratePasswordsRequest:@
prepareInterfaceForGeneratePasswordsRequestSelector :: Selector
prepareInterfaceForGeneratePasswordsRequestSelector = mkSelector "prepareInterfaceForGeneratePasswordsRequest:"

-- | @Selector@ for @extensionContext@
extensionContextSelector :: Selector
extensionContextSelector = mkSelector "extensionContext"

