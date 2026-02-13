{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationProviderExtensionAuthorizationRequest@.
module ObjC.AuthenticationServices.ASAuthorizationProviderExtensionAuthorizationRequest
  ( ASAuthorizationProviderExtensionAuthorizationRequest
  , IsASAuthorizationProviderExtensionAuthorizationRequest(..)
  , doNotHandle
  , cancel
  , complete
  , completeWithHTTPAuthorizationHeaders
  , completeWithHTTPResponse_httpBody
  , completeWithAuthorizationResult
  , completeWithError
  , presentAuthorizationViewControllerWithCompletion
  , url
  , requestedOperation
  , httpHeaders
  , httpBody
  , realm
  , extensionData
  , callerBundleIdentifier
  , authorizationOptions
  , callerManaged
  , callerTeamIdentifier
  , localizedCallerDisplayName
  , callerAuditToken
  , userInterfaceEnabled
  , loginManager
  , authorizationOptionsSelector
  , callerAuditTokenSelector
  , callerBundleIdentifierSelector
  , callerManagedSelector
  , callerTeamIdentifierSelector
  , cancelSelector
  , completeSelector
  , completeWithAuthorizationResultSelector
  , completeWithErrorSelector
  , completeWithHTTPAuthorizationHeadersSelector
  , completeWithHTTPResponse_httpBodySelector
  , doNotHandleSelector
  , extensionDataSelector
  , httpBodySelector
  , httpHeadersSelector
  , localizedCallerDisplayNameSelector
  , loginManagerSelector
  , presentAuthorizationViewControllerWithCompletionSelector
  , realmSelector
  , requestedOperationSelector
  , urlSelector
  , userInterfaceEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Call when authorization was not handled.
--
-- ObjC selector: @- doNotHandle@
doNotHandle :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO ()
doNotHandle asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest doNotHandleSelector

-- | Call when authorization needs to be canceled from some reason (for example user pressed Cancel button).
--
-- ObjC selector: @- cancel@
cancel :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO ()
cancel asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest cancelSelector

-- | Call when authorization succeeded without any output.
--
-- ObjC selector: @- complete@
complete :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO ()
complete asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest completeSelector

-- | Call when authorization succeeded with an authorization tokens stored in HTTP headers.
--
-- ObjC selector: @- completeWithHTTPAuthorizationHeaders:@
completeWithHTTPAuthorizationHeaders :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsNSDictionary httpAuthorizationHeaders) => asAuthorizationProviderExtensionAuthorizationRequest -> httpAuthorizationHeaders -> IO ()
completeWithHTTPAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationRequest httpAuthorizationHeaders =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest completeWithHTTPAuthorizationHeadersSelector (toNSDictionary httpAuthorizationHeaders)

-- | Call when authorization succeeded with a HTTP response.
--
-- ObjC selector: @- completeWithHTTPResponse:httpBody:@
completeWithHTTPResponse_httpBody :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsNSHTTPURLResponse httpResponse, IsNSData httpBody) => asAuthorizationProviderExtensionAuthorizationRequest -> httpResponse -> httpBody -> IO ()
completeWithHTTPResponse_httpBody asAuthorizationProviderExtensionAuthorizationRequest httpResponse httpBody =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest completeWithHTTPResponse_httpBodySelector (toNSHTTPURLResponse httpResponse) (toNSData httpBody)

-- | Call when authorization succeeded with
--
-- See: ASAuthorizationProviderExtensionAuthorizationResult.
--
-- ObjC selector: @- completeWithAuthorizationResult:@
completeWithAuthorizationResult :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsASAuthorizationProviderExtensionAuthorizationResult authorizationResult) => asAuthorizationProviderExtensionAuthorizationRequest -> authorizationResult -> IO ()
completeWithAuthorizationResult asAuthorizationProviderExtensionAuthorizationRequest authorizationResult =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest completeWithAuthorizationResultSelector (toASAuthorizationProviderExtensionAuthorizationResult authorizationResult)

-- | Call when authorization failed with an error.
--
-- ObjC selector: @- completeWithError:@
completeWithError :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsNSError error_) => asAuthorizationProviderExtensionAuthorizationRequest -> error_ -> IO ()
completeWithError asAuthorizationProviderExtensionAuthorizationRequest error_ =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest completeWithErrorSelector (toNSError error_)

-- | Asks authorization service to show extension view controller. If the controller cannot be shown an error is returned.
--
-- ObjC selector: @- presentAuthorizationViewControllerWithCompletion:@
presentAuthorizationViewControllerWithCompletion :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> Ptr () -> IO ()
presentAuthorizationViewControllerWithCompletion asAuthorizationProviderExtensionAuthorizationRequest completion =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest presentAuthorizationViewControllerWithCompletionSelector completion

-- | Request URL with all components.
--
-- ObjC selector: @- url@
url :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSURL)
url asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest urlSelector

-- | Operation to be executed by the extension.
--
-- ObjC selector: @- requestedOperation@
requestedOperation :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
requestedOperation asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest requestedOperationSelector

-- | Request HTTP headers.
--
-- ObjC selector: @- httpHeaders@
httpHeaders :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSDictionary)
httpHeaders asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest httpHeadersSelector

-- | Request body.
--
-- ObjC selector: @- httpBody@
httpBody :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSData)
httpBody asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest httpBodySelector

-- | Realm.
--
-- ObjC selector: @- realm@
realm :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
realm asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest realmSelector

-- | Extension data from extension configuration provided by MDM stored as a property-list.
--
-- ObjC selector: @- extensionData@
extensionData :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSDictionary)
extensionData asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest extensionDataSelector

-- | Identification of the calling application.
--
-- ObjC selector: @- callerBundleIdentifier@
callerBundleIdentifier :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
callerBundleIdentifier asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest callerBundleIdentifierSelector

-- | Authorization options.
--
-- ObjC selector: @- authorizationOptions@
authorizationOptions :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSDictionary)
authorizationOptions asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest authorizationOptionsSelector

-- | Indicates whether the calling application is managed.
--
-- ObjC selector: @- callerManaged@
callerManaged :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO Bool
callerManaged asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest callerManagedSelector

-- | Team identifier of the calling application.
--
-- ObjC selector: @- callerTeamIdentifier@
callerTeamIdentifier :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
callerTeamIdentifier asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest callerTeamIdentifierSelector

-- | Localized display name of the calling application.
--
-- ObjC selector: @- localizedCallerDisplayName@
localizedCallerDisplayName :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
localizedCallerDisplayName asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest localizedCallerDisplayNameSelector

-- | Audit token of the calling application.
--
-- ObjC selector: @- callerAuditToken@
callerAuditToken :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSData)
callerAuditToken asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest callerAuditTokenSelector

-- | Indicates whether the authorization user interface is enabled.
--
-- If user interface is not enabled, then the authorization will fail with
--
-- See: ASAuthorizationErrorNotInteractive if it attempts to display the authorization user interface via
--
-- See: presentAuthorizationViewControllerWithCompletion.
--
-- ObjC selector: @- userInterfaceEnabled@
userInterfaceEnabled :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO Bool
userInterfaceEnabled asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest userInterfaceEnabledSelector

-- | The login manager to interface with the Platform SSO configuration.
--
-- ObjC selector: @- loginManager@
loginManager :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id ASAuthorizationProviderExtensionLoginManager)
loginManager asAuthorizationProviderExtensionAuthorizationRequest =
  sendMessage asAuthorizationProviderExtensionAuthorizationRequest loginManagerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @doNotHandle@
doNotHandleSelector :: Selector '[] ()
doNotHandleSelector = mkSelector "doNotHandle"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @complete@
completeSelector :: Selector '[] ()
completeSelector = mkSelector "complete"

-- | @Selector@ for @completeWithHTTPAuthorizationHeaders:@
completeWithHTTPAuthorizationHeadersSelector :: Selector '[Id NSDictionary] ()
completeWithHTTPAuthorizationHeadersSelector = mkSelector "completeWithHTTPAuthorizationHeaders:"

-- | @Selector@ for @completeWithHTTPResponse:httpBody:@
completeWithHTTPResponse_httpBodySelector :: Selector '[Id NSHTTPURLResponse, Id NSData] ()
completeWithHTTPResponse_httpBodySelector = mkSelector "completeWithHTTPResponse:httpBody:"

-- | @Selector@ for @completeWithAuthorizationResult:@
completeWithAuthorizationResultSelector :: Selector '[Id ASAuthorizationProviderExtensionAuthorizationResult] ()
completeWithAuthorizationResultSelector = mkSelector "completeWithAuthorizationResult:"

-- | @Selector@ for @completeWithError:@
completeWithErrorSelector :: Selector '[Id NSError] ()
completeWithErrorSelector = mkSelector "completeWithError:"

-- | @Selector@ for @presentAuthorizationViewControllerWithCompletion:@
presentAuthorizationViewControllerWithCompletionSelector :: Selector '[Ptr ()] ()
presentAuthorizationViewControllerWithCompletionSelector = mkSelector "presentAuthorizationViewControllerWithCompletion:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @requestedOperation@
requestedOperationSelector :: Selector '[] (Id NSString)
requestedOperationSelector = mkSelector "requestedOperation"

-- | @Selector@ for @httpHeaders@
httpHeadersSelector :: Selector '[] (Id NSDictionary)
httpHeadersSelector = mkSelector "httpHeaders"

-- | @Selector@ for @httpBody@
httpBodySelector :: Selector '[] (Id NSData)
httpBodySelector = mkSelector "httpBody"

-- | @Selector@ for @realm@
realmSelector :: Selector '[] (Id NSString)
realmSelector = mkSelector "realm"

-- | @Selector@ for @extensionData@
extensionDataSelector :: Selector '[] (Id NSDictionary)
extensionDataSelector = mkSelector "extensionData"

-- | @Selector@ for @callerBundleIdentifier@
callerBundleIdentifierSelector :: Selector '[] (Id NSString)
callerBundleIdentifierSelector = mkSelector "callerBundleIdentifier"

-- | @Selector@ for @authorizationOptions@
authorizationOptionsSelector :: Selector '[] (Id NSDictionary)
authorizationOptionsSelector = mkSelector "authorizationOptions"

-- | @Selector@ for @callerManaged@
callerManagedSelector :: Selector '[] Bool
callerManagedSelector = mkSelector "callerManaged"

-- | @Selector@ for @callerTeamIdentifier@
callerTeamIdentifierSelector :: Selector '[] (Id NSString)
callerTeamIdentifierSelector = mkSelector "callerTeamIdentifier"

-- | @Selector@ for @localizedCallerDisplayName@
localizedCallerDisplayNameSelector :: Selector '[] (Id NSString)
localizedCallerDisplayNameSelector = mkSelector "localizedCallerDisplayName"

-- | @Selector@ for @callerAuditToken@
callerAuditTokenSelector :: Selector '[] (Id NSData)
callerAuditTokenSelector = mkSelector "callerAuditToken"

-- | @Selector@ for @userInterfaceEnabled@
userInterfaceEnabledSelector :: Selector '[] Bool
userInterfaceEnabledSelector = mkSelector "userInterfaceEnabled"

-- | @Selector@ for @loginManager@
loginManagerSelector :: Selector '[] (Id ASAuthorizationProviderExtensionLoginManager)
loginManagerSelector = mkSelector "loginManager"

