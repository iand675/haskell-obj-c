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
  , doNotHandleSelector
  , cancelSelector
  , completeSelector
  , completeWithHTTPAuthorizationHeadersSelector
  , completeWithHTTPResponse_httpBodySelector
  , completeWithAuthorizationResultSelector
  , completeWithErrorSelector
  , presentAuthorizationViewControllerWithCompletionSelector
  , urlSelector
  , requestedOperationSelector
  , httpHeadersSelector
  , httpBodySelector
  , realmSelector
  , extensionDataSelector
  , callerBundleIdentifierSelector
  , authorizationOptionsSelector
  , callerManagedSelector
  , callerTeamIdentifierSelector
  , localizedCallerDisplayNameSelector
  , callerAuditTokenSelector
  , userInterfaceEnabledSelector


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

-- | Call when authorization was not handled.
--
-- ObjC selector: @- doNotHandle@
doNotHandle :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO ()
doNotHandle asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "doNotHandle") retVoid []

-- | Call when authorization needs to be canceled from some reason (for example user pressed Cancel button).
--
-- ObjC selector: @- cancel@
cancel :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO ()
cancel asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "cancel") retVoid []

-- | Call when authorization succeeded without any output.
--
-- ObjC selector: @- complete@
complete :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO ()
complete asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "complete") retVoid []

-- | Call when authorization succeeded with an authorization tokens stored in HTTP headers.
--
-- ObjC selector: @- completeWithHTTPAuthorizationHeaders:@
completeWithHTTPAuthorizationHeaders :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsNSDictionary httpAuthorizationHeaders) => asAuthorizationProviderExtensionAuthorizationRequest -> httpAuthorizationHeaders -> IO ()
completeWithHTTPAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationRequest  httpAuthorizationHeaders =
withObjCPtr httpAuthorizationHeaders $ \raw_httpAuthorizationHeaders ->
    sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "completeWithHTTPAuthorizationHeaders:") retVoid [argPtr (castPtr raw_httpAuthorizationHeaders :: Ptr ())]

-- | Call when authorization succeeded with a HTTP response.
--
-- ObjC selector: @- completeWithHTTPResponse:httpBody:@
completeWithHTTPResponse_httpBody :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsNSHTTPURLResponse httpResponse, IsNSData httpBody) => asAuthorizationProviderExtensionAuthorizationRequest -> httpResponse -> httpBody -> IO ()
completeWithHTTPResponse_httpBody asAuthorizationProviderExtensionAuthorizationRequest  httpResponse httpBody =
withObjCPtr httpResponse $ \raw_httpResponse ->
  withObjCPtr httpBody $ \raw_httpBody ->
      sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "completeWithHTTPResponse:httpBody:") retVoid [argPtr (castPtr raw_httpResponse :: Ptr ()), argPtr (castPtr raw_httpBody :: Ptr ())]

-- | Call when authorization succeeded with
--
-- See: ASAuthorizationProviderExtensionAuthorizationResult.
--
-- ObjC selector: @- completeWithAuthorizationResult:@
completeWithAuthorizationResult :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsASAuthorizationProviderExtensionAuthorizationResult authorizationResult) => asAuthorizationProviderExtensionAuthorizationRequest -> authorizationResult -> IO ()
completeWithAuthorizationResult asAuthorizationProviderExtensionAuthorizationRequest  authorizationResult =
withObjCPtr authorizationResult $ \raw_authorizationResult ->
    sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "completeWithAuthorizationResult:") retVoid [argPtr (castPtr raw_authorizationResult :: Ptr ())]

-- | Call when authorization failed with an error.
--
-- ObjC selector: @- completeWithError:@
completeWithError :: (IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest, IsNSError error_) => asAuthorizationProviderExtensionAuthorizationRequest -> error_ -> IO ()
completeWithError asAuthorizationProviderExtensionAuthorizationRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "completeWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Asks authorization service to show extension view controller. If the controller cannot be shown an error is returned.
--
-- ObjC selector: @- presentAuthorizationViewControllerWithCompletion:@
presentAuthorizationViewControllerWithCompletion :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> Ptr () -> IO ()
presentAuthorizationViewControllerWithCompletion asAuthorizationProviderExtensionAuthorizationRequest  completion =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "presentAuthorizationViewControllerWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Request URL with all components.
--
-- ObjC selector: @- url@
url :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSURL)
url asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Operation to be executed by the extension.
--
-- ObjC selector: @- requestedOperation@
requestedOperation :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
requestedOperation asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "requestedOperation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Request HTTP headers.
--
-- ObjC selector: @- httpHeaders@
httpHeaders :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSDictionary)
httpHeaders asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "httpHeaders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Request body.
--
-- ObjC selector: @- httpBody@
httpBody :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSData)
httpBody asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "httpBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Realm.
--
-- ObjC selector: @- realm@
realm :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
realm asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "realm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Extension data from extension configuration provided by MDM stored as a property-list.
--
-- ObjC selector: @- extensionData@
extensionData :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSDictionary)
extensionData asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "extensionData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Identification of the calling application.
--
-- ObjC selector: @- callerBundleIdentifier@
callerBundleIdentifier :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
callerBundleIdentifier asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "callerBundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Authorization options.
--
-- ObjC selector: @- authorizationOptions@
authorizationOptions :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSDictionary)
authorizationOptions asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "authorizationOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates whether the calling application is managed.
--
-- ObjC selector: @- callerManaged@
callerManaged :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO Bool
callerManaged asAuthorizationProviderExtensionAuthorizationRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "callerManaged") retCULong []

-- | Team identifier of the calling application.
--
-- ObjC selector: @- callerTeamIdentifier@
callerTeamIdentifier :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
callerTeamIdentifier asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "callerTeamIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized display name of the calling application.
--
-- ObjC selector: @- localizedCallerDisplayName@
localizedCallerDisplayName :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSString)
localizedCallerDisplayName asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "localizedCallerDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Audit token of the calling application.
--
-- ObjC selector: @- callerAuditToken@
callerAuditToken :: IsASAuthorizationProviderExtensionAuthorizationRequest asAuthorizationProviderExtensionAuthorizationRequest => asAuthorizationProviderExtensionAuthorizationRequest -> IO (Id NSData)
callerAuditToken asAuthorizationProviderExtensionAuthorizationRequest  =
  sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "callerAuditToken") (retPtr retVoid) [] >>= retainedObject . castPtr

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
userInterfaceEnabled asAuthorizationProviderExtensionAuthorizationRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionAuthorizationRequest (mkSelector "userInterfaceEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @doNotHandle@
doNotHandleSelector :: Selector
doNotHandleSelector = mkSelector "doNotHandle"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @complete@
completeSelector :: Selector
completeSelector = mkSelector "complete"

-- | @Selector@ for @completeWithHTTPAuthorizationHeaders:@
completeWithHTTPAuthorizationHeadersSelector :: Selector
completeWithHTTPAuthorizationHeadersSelector = mkSelector "completeWithHTTPAuthorizationHeaders:"

-- | @Selector@ for @completeWithHTTPResponse:httpBody:@
completeWithHTTPResponse_httpBodySelector :: Selector
completeWithHTTPResponse_httpBodySelector = mkSelector "completeWithHTTPResponse:httpBody:"

-- | @Selector@ for @completeWithAuthorizationResult:@
completeWithAuthorizationResultSelector :: Selector
completeWithAuthorizationResultSelector = mkSelector "completeWithAuthorizationResult:"

-- | @Selector@ for @completeWithError:@
completeWithErrorSelector :: Selector
completeWithErrorSelector = mkSelector "completeWithError:"

-- | @Selector@ for @presentAuthorizationViewControllerWithCompletion:@
presentAuthorizationViewControllerWithCompletionSelector :: Selector
presentAuthorizationViewControllerWithCompletionSelector = mkSelector "presentAuthorizationViewControllerWithCompletion:"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @requestedOperation@
requestedOperationSelector :: Selector
requestedOperationSelector = mkSelector "requestedOperation"

-- | @Selector@ for @httpHeaders@
httpHeadersSelector :: Selector
httpHeadersSelector = mkSelector "httpHeaders"

-- | @Selector@ for @httpBody@
httpBodySelector :: Selector
httpBodySelector = mkSelector "httpBody"

-- | @Selector@ for @realm@
realmSelector :: Selector
realmSelector = mkSelector "realm"

-- | @Selector@ for @extensionData@
extensionDataSelector :: Selector
extensionDataSelector = mkSelector "extensionData"

-- | @Selector@ for @callerBundleIdentifier@
callerBundleIdentifierSelector :: Selector
callerBundleIdentifierSelector = mkSelector "callerBundleIdentifier"

-- | @Selector@ for @authorizationOptions@
authorizationOptionsSelector :: Selector
authorizationOptionsSelector = mkSelector "authorizationOptions"

-- | @Selector@ for @callerManaged@
callerManagedSelector :: Selector
callerManagedSelector = mkSelector "callerManaged"

-- | @Selector@ for @callerTeamIdentifier@
callerTeamIdentifierSelector :: Selector
callerTeamIdentifierSelector = mkSelector "callerTeamIdentifier"

-- | @Selector@ for @localizedCallerDisplayName@
localizedCallerDisplayNameSelector :: Selector
localizedCallerDisplayNameSelector = mkSelector "localizedCallerDisplayName"

-- | @Selector@ for @callerAuditToken@
callerAuditTokenSelector :: Selector
callerAuditTokenSelector = mkSelector "callerAuditToken"

-- | @Selector@ for @userInterfaceEnabled@
userInterfaceEnabledSelector :: Selector
userInterfaceEnabledSelector = mkSelector "userInterfaceEnabled"

