{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASCredentialProviderExtensionContext@.
module ObjC.AuthenticationServices.ASCredentialProviderExtensionContext
  ( ASCredentialProviderExtensionContext
  , IsASCredentialProviderExtensionContext(..)
  , completeRequestWithSelectedCredential_completionHandler
  , completeAssertionRequestWithSelectedPasskeyCredential_completionHandler
  , completeRegistrationRequestWithSelectedPasskeyCredential_completionHandler
  , completeOneTimeCodeRequestWithSelectedCredential_completionHandler
  , completeSavePasswordRequestWithCompletionHandler
  , completeGeneratePasswordRequestWithResults_completionHandler
  , completeExtensionConfigurationRequest
  , completeRequestReturningItems_completionHandler
  , cancelRequestWithError
  , cancelRequestWithErrorSelector
  , completeAssertionRequestWithSelectedPasskeyCredential_completionHandlerSelector
  , completeExtensionConfigurationRequestSelector
  , completeGeneratePasswordRequestWithResults_completionHandlerSelector
  , completeOneTimeCodeRequestWithSelectedCredential_completionHandlerSelector
  , completeRegistrationRequestWithSelectedPasskeyCredential_completionHandlerSelector
  , completeRequestReturningItems_completionHandlerSelector
  , completeRequestWithSelectedCredential_completionHandlerSelector
  , completeSavePasswordRequestWithCompletionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Complete the request by providing the user selected credential.
--
-- @credential@ — the credential that the user has selected.
--
-- @completionHandler@ — optionally contains any work which the extension may need to perform after the request has been completed, as a background-priority task. The @expired@ parameter will be YES if the system decides to prematurely terminate a previous non-expiration invocation of the completionHandler.
--
-- Calling this method will eventually dismiss the associated view controller.
--
-- ObjC selector: @- completeRequestWithSelectedCredential:completionHandler:@
completeRequestWithSelectedCredential_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsASPasswordCredential credential) => asCredentialProviderExtensionContext -> credential -> Ptr () -> IO ()
completeRequestWithSelectedCredential_completionHandler asCredentialProviderExtensionContext credential completionHandler =
  sendMessage asCredentialProviderExtensionContext completeRequestWithSelectedCredential_completionHandlerSelector (toASPasswordCredential credential) completionHandler

-- | Complete the assertion request by providing the user selected passkey credential.
--
-- @credential@ — the credential that the user has selected. Includes assertion response.
--
-- @completionHandler@ — optionally contains any work which the extension may need to perform after the request has been completed, as a background-priority task. The @expired@ parameter will be YES if the system decides to prematurely terminate a previous non-expiration invocation of the completionHandler.
--
-- Calling this method will eventually dismiss the associated view controller.
--
-- ObjC selector: @- completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:@
completeAssertionRequestWithSelectedPasskeyCredential_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsASPasskeyAssertionCredential credential) => asCredentialProviderExtensionContext -> credential -> Ptr () -> IO ()
completeAssertionRequestWithSelectedPasskeyCredential_completionHandler asCredentialProviderExtensionContext credential completionHandler =
  sendMessage asCredentialProviderExtensionContext completeAssertionRequestWithSelectedPasskeyCredential_completionHandlerSelector (toASPasskeyAssertionCredential credential) completionHandler

-- | Complete the registration request by providing the newly created passkey credential.
--
-- @credential@ — the credential that was created in response to the registration request.
--
-- @completionHandler@ — optionally contains any work which the extension may need to perform after the request has been completed, as a background-priority task. The @expired@ parameter will be YES if the system decides to prematurely terminate a previous non-expiration invocation of the completionHandler.
--
-- Calling this method will eventually dismiss the associated view controller.
--
-- ObjC selector: @- completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:@
completeRegistrationRequestWithSelectedPasskeyCredential_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsASPasskeyRegistrationCredential credential) => asCredentialProviderExtensionContext -> credential -> Ptr () -> IO ()
completeRegistrationRequestWithSelectedPasskeyCredential_completionHandler asCredentialProviderExtensionContext credential completionHandler =
  sendMessage asCredentialProviderExtensionContext completeRegistrationRequestWithSelectedPasskeyCredential_completionHandlerSelector (toASPasskeyRegistrationCredential credential) completionHandler

-- | Complete the request by providing the user selected one time code credential.
--
-- @credential@ — the credential that the user has selected.
--
-- @completionHandler@ — optionally contains any work which the extension may need to perform after the request has been completed, as a background-priority task. The @expired@ parameter will be YES if the system decides to prematurely terminate a previous non-expiration invocation of the completionHandler.
--
-- Calling this method will eventually dismiss the associated view controller.
--
-- ObjC selector: @- completeOneTimeCodeRequestWithSelectedCredential:completionHandler:@
completeOneTimeCodeRequestWithSelectedCredential_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsASOneTimeCodeCredential credential) => asCredentialProviderExtensionContext -> credential -> Ptr () -> IO ()
completeOneTimeCodeRequestWithSelectedCredential_completionHandler asCredentialProviderExtensionContext credential completionHandler =
  sendMessage asCredentialProviderExtensionContext completeOneTimeCodeRequestWithSelectedCredential_completionHandlerSelector (toASOneTimeCodeCredential credential) completionHandler

-- | Signal that a password request was successfully saved. - parameter completionHandler: An optional block your extension can provide to perform any cleanup work after the system has captured the results. The expired parameter is true if the system decides to prematurely end a previous non-expiration invocation of the completion handler. - note: You are responsible for updating the ASCredentialIdentityStore.
--
-- ObjC selector: @- completeSavePasswordRequestWithCompletionHandler:@
completeSavePasswordRequestWithCompletionHandler :: IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext => asCredentialProviderExtensionContext -> Ptr () -> IO ()
completeSavePasswordRequestWithCompletionHandler asCredentialProviderExtensionContext completionHandler =
  sendMessage asCredentialProviderExtensionContext completeSavePasswordRequestWithCompletionHandlerSelector completionHandler

-- | Return potential passwords for the given request. - parameter results: Potential passwords that the user can select. You can provide multiple options for increased flexibility on the user's behalf. These results should be returned in priority order. - parameter completionHandler: An optional block your extension can provide to perform any cleanup work after the system has captured the results. The expired parameter is true if the system decides to prematurely end a previous non-expiration invocation of the completion handler.
--
-- ObjC selector: @- completeGeneratePasswordRequestWithResults:completionHandler:@
completeGeneratePasswordRequestWithResults_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsNSArray results) => asCredentialProviderExtensionContext -> results -> Ptr () -> IO ()
completeGeneratePasswordRequestWithResults_completionHandler asCredentialProviderExtensionContext results completionHandler =
  sendMessage asCredentialProviderExtensionContext completeGeneratePasswordRequestWithResults_completionHandlerSelector (toNSArray results) completionHandler

-- | Complete the request to configure the extension.
--
-- Calling this method will eventually dismiss the associated view controller.
--
-- ObjC selector: @- completeExtensionConfigurationRequest@
completeExtensionConfigurationRequest :: IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext => asCredentialProviderExtensionContext -> IO ()
completeExtensionConfigurationRequest asCredentialProviderExtensionContext =
  sendMessage asCredentialProviderExtensionContext completeExtensionConfigurationRequestSelector

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsNSArray items) => asCredentialProviderExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler asCredentialProviderExtensionContext items completionHandler =
  sendMessage asCredentialProviderExtensionContext completeRequestReturningItems_completionHandlerSelector (toNSArray items) completionHandler

-- | Cancels the request.
--
-- @error@ — error's domain should be ASExtensionErrorDomain and the code should be a value of type ASExtensionErrorCode.
--
-- The extension should call this method when the user cancels the action or a failure occurs.
--
-- ObjC selector: @- cancelRequestWithError:@
cancelRequestWithError :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsNSError error_) => asCredentialProviderExtensionContext -> error_ -> IO ()
cancelRequestWithError asCredentialProviderExtensionContext error_ =
  sendMessage asCredentialProviderExtensionContext cancelRequestWithErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completeRequestWithSelectedCredential:completionHandler:@
completeRequestWithSelectedCredential_completionHandlerSelector :: Selector '[Id ASPasswordCredential, Ptr ()] ()
completeRequestWithSelectedCredential_completionHandlerSelector = mkSelector "completeRequestWithSelectedCredential:completionHandler:"

-- | @Selector@ for @completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:@
completeAssertionRequestWithSelectedPasskeyCredential_completionHandlerSelector :: Selector '[Id ASPasskeyAssertionCredential, Ptr ()] ()
completeAssertionRequestWithSelectedPasskeyCredential_completionHandlerSelector = mkSelector "completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:"

-- | @Selector@ for @completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:@
completeRegistrationRequestWithSelectedPasskeyCredential_completionHandlerSelector :: Selector '[Id ASPasskeyRegistrationCredential, Ptr ()] ()
completeRegistrationRequestWithSelectedPasskeyCredential_completionHandlerSelector = mkSelector "completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:"

-- | @Selector@ for @completeOneTimeCodeRequestWithSelectedCredential:completionHandler:@
completeOneTimeCodeRequestWithSelectedCredential_completionHandlerSelector :: Selector '[Id ASOneTimeCodeCredential, Ptr ()] ()
completeOneTimeCodeRequestWithSelectedCredential_completionHandlerSelector = mkSelector "completeOneTimeCodeRequestWithSelectedCredential:completionHandler:"

-- | @Selector@ for @completeSavePasswordRequestWithCompletionHandler:@
completeSavePasswordRequestWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
completeSavePasswordRequestWithCompletionHandlerSelector = mkSelector "completeSavePasswordRequestWithCompletionHandler:"

-- | @Selector@ for @completeGeneratePasswordRequestWithResults:completionHandler:@
completeGeneratePasswordRequestWithResults_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
completeGeneratePasswordRequestWithResults_completionHandlerSelector = mkSelector "completeGeneratePasswordRequestWithResults:completionHandler:"

-- | @Selector@ for @completeExtensionConfigurationRequest@
completeExtensionConfigurationRequestSelector :: Selector '[] ()
completeExtensionConfigurationRequestSelector = mkSelector "completeExtensionConfigurationRequest"

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector '[Id NSError] ()
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

