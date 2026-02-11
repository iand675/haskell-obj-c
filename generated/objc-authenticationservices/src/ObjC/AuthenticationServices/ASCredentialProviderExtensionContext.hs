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
  , completeRequestWithSelectedCredential_completionHandlerSelector
  , completeAssertionRequestWithSelectedPasskeyCredential_completionHandlerSelector
  , completeRegistrationRequestWithSelectedPasskeyCredential_completionHandlerSelector
  , completeOneTimeCodeRequestWithSelectedCredential_completionHandlerSelector
  , completeSavePasswordRequestWithCompletionHandlerSelector
  , completeGeneratePasswordRequestWithResults_completionHandlerSelector
  , completeExtensionConfigurationRequestSelector
  , completeRequestReturningItems_completionHandlerSelector
  , cancelRequestWithErrorSelector


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
completeRequestWithSelectedCredential_completionHandler asCredentialProviderExtensionContext  credential completionHandler =
withObjCPtr credential $ \raw_credential ->
    sendMsg asCredentialProviderExtensionContext (mkSelector "completeRequestWithSelectedCredential:completionHandler:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
completeAssertionRequestWithSelectedPasskeyCredential_completionHandler asCredentialProviderExtensionContext  credential completionHandler =
withObjCPtr credential $ \raw_credential ->
    sendMsg asCredentialProviderExtensionContext (mkSelector "completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
completeRegistrationRequestWithSelectedPasskeyCredential_completionHandler asCredentialProviderExtensionContext  credential completionHandler =
withObjCPtr credential $ \raw_credential ->
    sendMsg asCredentialProviderExtensionContext (mkSelector "completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
completeOneTimeCodeRequestWithSelectedCredential_completionHandler asCredentialProviderExtensionContext  credential completionHandler =
withObjCPtr credential $ \raw_credential ->
    sendMsg asCredentialProviderExtensionContext (mkSelector "completeOneTimeCodeRequestWithSelectedCredential:completionHandler:") retVoid [argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Signal that a password request was successfully saved. - parameter completionHandler: An optional block your extension can provide to perform any cleanup work after the system has captured the results. The expired parameter is true if the system decides to prematurely end a previous non-expiration invocation of the completion handler. - note: You are responsible for updating the ASCredentialIdentityStore.
--
-- ObjC selector: @- completeSavePasswordRequestWithCompletionHandler:@
completeSavePasswordRequestWithCompletionHandler :: IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext => asCredentialProviderExtensionContext -> Ptr () -> IO ()
completeSavePasswordRequestWithCompletionHandler asCredentialProviderExtensionContext  completionHandler =
  sendMsg asCredentialProviderExtensionContext (mkSelector "completeSavePasswordRequestWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Return potential passwords for the given request. - parameter results: Potential passwords that the user can select. You can provide multiple options for increased flexibility on the user's behalf. These results should be returned in priority order. - parameter completionHandler: An optional block your extension can provide to perform any cleanup work after the system has captured the results. The expired parameter is true if the system decides to prematurely end a previous non-expiration invocation of the completion handler.
--
-- ObjC selector: @- completeGeneratePasswordRequestWithResults:completionHandler:@
completeGeneratePasswordRequestWithResults_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsNSArray results) => asCredentialProviderExtensionContext -> results -> Ptr () -> IO ()
completeGeneratePasswordRequestWithResults_completionHandler asCredentialProviderExtensionContext  results completionHandler =
withObjCPtr results $ \raw_results ->
    sendMsg asCredentialProviderExtensionContext (mkSelector "completeGeneratePasswordRequestWithResults:completionHandler:") retVoid [argPtr (castPtr raw_results :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Complete the request to configure the extension.
--
-- Calling this method will eventually dismiss the associated view controller.
--
-- ObjC selector: @- completeExtensionConfigurationRequest@
completeExtensionConfigurationRequest :: IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext => asCredentialProviderExtensionContext -> IO ()
completeExtensionConfigurationRequest asCredentialProviderExtensionContext  =
  sendMsg asCredentialProviderExtensionContext (mkSelector "completeExtensionConfigurationRequest") retVoid []

-- | @- completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandler :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsNSArray items) => asCredentialProviderExtensionContext -> items -> Ptr () -> IO ()
completeRequestReturningItems_completionHandler asCredentialProviderExtensionContext  items completionHandler =
withObjCPtr items $ \raw_items ->
    sendMsg asCredentialProviderExtensionContext (mkSelector "completeRequestReturningItems:completionHandler:") retVoid [argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Cancels the request.
--
-- @error@ — error's domain should be ASExtensionErrorDomain and the code should be a value of type ASExtensionErrorCode.
--
-- The extension should call this method when the user cancels the action or a failure occurs.
--
-- ObjC selector: @- cancelRequestWithError:@
cancelRequestWithError :: (IsASCredentialProviderExtensionContext asCredentialProviderExtensionContext, IsNSError error_) => asCredentialProviderExtensionContext -> error_ -> IO ()
cancelRequestWithError asCredentialProviderExtensionContext  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg asCredentialProviderExtensionContext (mkSelector "cancelRequestWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completeRequestWithSelectedCredential:completionHandler:@
completeRequestWithSelectedCredential_completionHandlerSelector :: Selector
completeRequestWithSelectedCredential_completionHandlerSelector = mkSelector "completeRequestWithSelectedCredential:completionHandler:"

-- | @Selector@ for @completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:@
completeAssertionRequestWithSelectedPasskeyCredential_completionHandlerSelector :: Selector
completeAssertionRequestWithSelectedPasskeyCredential_completionHandlerSelector = mkSelector "completeAssertionRequestWithSelectedPasskeyCredential:completionHandler:"

-- | @Selector@ for @completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:@
completeRegistrationRequestWithSelectedPasskeyCredential_completionHandlerSelector :: Selector
completeRegistrationRequestWithSelectedPasskeyCredential_completionHandlerSelector = mkSelector "completeRegistrationRequestWithSelectedPasskeyCredential:completionHandler:"

-- | @Selector@ for @completeOneTimeCodeRequestWithSelectedCredential:completionHandler:@
completeOneTimeCodeRequestWithSelectedCredential_completionHandlerSelector :: Selector
completeOneTimeCodeRequestWithSelectedCredential_completionHandlerSelector = mkSelector "completeOneTimeCodeRequestWithSelectedCredential:completionHandler:"

-- | @Selector@ for @completeSavePasswordRequestWithCompletionHandler:@
completeSavePasswordRequestWithCompletionHandlerSelector :: Selector
completeSavePasswordRequestWithCompletionHandlerSelector = mkSelector "completeSavePasswordRequestWithCompletionHandler:"

-- | @Selector@ for @completeGeneratePasswordRequestWithResults:completionHandler:@
completeGeneratePasswordRequestWithResults_completionHandlerSelector :: Selector
completeGeneratePasswordRequestWithResults_completionHandlerSelector = mkSelector "completeGeneratePasswordRequestWithResults:completionHandler:"

-- | @Selector@ for @completeExtensionConfigurationRequest@
completeExtensionConfigurationRequestSelector :: Selector
completeExtensionConfigurationRequestSelector = mkSelector "completeExtensionConfigurationRequest"

-- | @Selector@ for @completeRequestReturningItems:completionHandler:@
completeRequestReturningItems_completionHandlerSelector :: Selector
completeRequestReturningItems_completionHandlerSelector = mkSelector "completeRequestReturningItems:completionHandler:"

-- | @Selector@ for @cancelRequestWithError:@
cancelRequestWithErrorSelector :: Selector
cancelRequestWithErrorSelector = mkSelector "cancelRequestWithError:"

