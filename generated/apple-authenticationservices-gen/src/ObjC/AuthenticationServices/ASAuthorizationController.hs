{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationController@.
module ObjC.AuthenticationServices.ASAuthorizationController
  ( ASAuthorizationController
  , IsASAuthorizationController(..)
  , initWithAuthorizationRequests
  , performRequests
  , performAutoFillAssistedRequests
  , performRequestsWithOptions
  , cancel
  , new
  , init_
  , authorizationRequests
  , delegate
  , setDelegate
  , presentationContextProvider
  , setPresentationContextProvider
  , customAuthorizationMethods
  , setCustomAuthorizationMethods
  , authorizationRequestsSelector
  , cancelSelector
  , customAuthorizationMethodsSelector
  , delegateSelector
  , initSelector
  , initWithAuthorizationRequestsSelector
  , newSelector
  , performAutoFillAssistedRequestsSelector
  , performRequestsSelector
  , performRequestsWithOptionsSelector
  , presentationContextProviderSelector
  , setCustomAuthorizationMethodsSelector
  , setDelegateSelector
  , setPresentationContextProviderSelector

  -- * Enum types
  , ASAuthorizationControllerRequestOptions(ASAuthorizationControllerRequestOptions)
  , pattern ASAuthorizationControllerRequestOptionPreferImmediatelyAvailableCredentials

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize the controller with authorization requests.
--
-- @authorizationRequests@ — At least one request should be provided. Requests of same type maybe honored in first in first out order
--
-- ObjC selector: @- initWithAuthorizationRequests:@
initWithAuthorizationRequests :: (IsASAuthorizationController asAuthorizationController, IsNSArray authorizationRequests) => asAuthorizationController -> authorizationRequests -> IO (Id ASAuthorizationController)
initWithAuthorizationRequests asAuthorizationController authorizationRequests =
  sendOwnedMessage asAuthorizationController initWithAuthorizationRequestsSelector (toNSArray authorizationRequests)

-- | Initiate the authorization flows. Upon completion, the delegate will be called with either success or failure. Certain authorization flows may require a presentation context. The @presentationContextProvider@ will be called to provide it.
--
-- The instance will remain retained until the flow is either completed or canceled, and the delegate callback is made.
--
-- ObjC selector: @- performRequests@
performRequests :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO ()
performRequests asAuthorizationController =
  sendMessage asAuthorizationController performRequestsSelector

-- | Initiate the authorization flows for requests that support AutoFill presentation. UI will be shown when focusing a text field with the appropriate text content type. Upon completion, the delegate will be called with either success or failure.
--
-- The instance will remain retained until the flow is either completed or canceled, and the delegate callback is made.
--
-- ObjC selector: @- performAutoFillAssistedRequests@
performAutoFillAssistedRequests :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO ()
performAutoFillAssistedRequests asAuthorizationController =
  sendMessage asAuthorizationController performAutoFillAssistedRequestsSelector

-- | Initiate the authorization flows. Upon completion, the delegate will be called with either success or failure. Certain authorization flows may require a presentation context. The @presentationContextProvider@ will be called to provide it.
--
-- Calling this method with no options is the same as calling @performRequests.@ The instance will remain retained until the flow is either completed or canceled, and the delegate callback is made.
--
-- ObjC selector: @- performRequestsWithOptions:@
performRequestsWithOptions :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> ASAuthorizationControllerRequestOptions -> IO ()
performRequestsWithOptions asAuthorizationController options =
  sendMessage asAuthorizationController performRequestsWithOptionsSelector options

-- | Cancel the running authorization flows, if there are any. If a flow is canceled, the delegate callback will be made indicating the cancel.
--
-- ObjC selector: @- cancel@
cancel :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO ()
cancel asAuthorizationController =
  sendMessage asAuthorizationController cancelSelector

-- | @+ new@
new :: IO (Id ASAuthorizationController)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationController"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO (Id ASAuthorizationController)
init_ asAuthorizationController =
  sendOwnedMessage asAuthorizationController initSelector

-- | Authorization requests that are being serviced by this controller
--
-- ObjC selector: @- authorizationRequests@
authorizationRequests :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO (Id NSArray)
authorizationRequests asAuthorizationController =
  sendMessage asAuthorizationController authorizationRequestsSelector

-- | This delegate will be invoked upon completion of the authorization indicating success or failure. Delegate is required to receive the results of authorization.
--
-- ObjC selector: @- delegate@
delegate :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO RawId
delegate asAuthorizationController =
  sendMessage asAuthorizationController delegateSelector

-- | This delegate will be invoked upon completion of the authorization indicating success or failure. Delegate is required to receive the results of authorization.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> RawId -> IO ()
setDelegate asAuthorizationController value =
  sendMessage asAuthorizationController setDelegateSelector value

-- | This delegate will be invoked upon needing a presentation context to display authorization UI.
--
-- ObjC selector: @- presentationContextProvider@
presentationContextProvider :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO RawId
presentationContextProvider asAuthorizationController =
  sendMessage asAuthorizationController presentationContextProviderSelector

-- | This delegate will be invoked upon needing a presentation context to display authorization UI.
--
-- ObjC selector: @- setPresentationContextProvider:@
setPresentationContextProvider :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> RawId -> IO ()
setPresentationContextProvider asAuthorizationController value =
  sendMessage asAuthorizationController setPresentationContextProviderSelector value

-- | A list of custom authorization methods that may be displayed in the authorization UI.
--
-- If the user selects one of these methods, instead of attempting to secure an authorization for the requests, the controller will call authorizationController:didCompleteWithCustomMethod: with the selected method, allowing the client to perform the requested authorization.
--
-- ObjC selector: @- customAuthorizationMethods@
customAuthorizationMethods :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO (Id NSArray)
customAuthorizationMethods asAuthorizationController =
  sendMessage asAuthorizationController customAuthorizationMethodsSelector

-- | A list of custom authorization methods that may be displayed in the authorization UI.
--
-- If the user selects one of these methods, instead of attempting to secure an authorization for the requests, the controller will call authorizationController:didCompleteWithCustomMethod: with the selected method, allowing the client to perform the requested authorization.
--
-- ObjC selector: @- setCustomAuthorizationMethods:@
setCustomAuthorizationMethods :: (IsASAuthorizationController asAuthorizationController, IsNSArray value) => asAuthorizationController -> value -> IO ()
setCustomAuthorizationMethods asAuthorizationController value =
  sendMessage asAuthorizationController setCustomAuthorizationMethodsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAuthorizationRequests:@
initWithAuthorizationRequestsSelector :: Selector '[Id NSArray] (Id ASAuthorizationController)
initWithAuthorizationRequestsSelector = mkSelector "initWithAuthorizationRequests:"

-- | @Selector@ for @performRequests@
performRequestsSelector :: Selector '[] ()
performRequestsSelector = mkSelector "performRequests"

-- | @Selector@ for @performAutoFillAssistedRequests@
performAutoFillAssistedRequestsSelector :: Selector '[] ()
performAutoFillAssistedRequestsSelector = mkSelector "performAutoFillAssistedRequests"

-- | @Selector@ for @performRequestsWithOptions:@
performRequestsWithOptionsSelector :: Selector '[ASAuthorizationControllerRequestOptions] ()
performRequestsWithOptionsSelector = mkSelector "performRequestsWithOptions:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationController)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationController)
initSelector = mkSelector "init"

-- | @Selector@ for @authorizationRequests@
authorizationRequestsSelector :: Selector '[] (Id NSArray)
authorizationRequestsSelector = mkSelector "authorizationRequests"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @presentationContextProvider@
presentationContextProviderSelector :: Selector '[] RawId
presentationContextProviderSelector = mkSelector "presentationContextProvider"

-- | @Selector@ for @setPresentationContextProvider:@
setPresentationContextProviderSelector :: Selector '[RawId] ()
setPresentationContextProviderSelector = mkSelector "setPresentationContextProvider:"

-- | @Selector@ for @customAuthorizationMethods@
customAuthorizationMethodsSelector :: Selector '[] (Id NSArray)
customAuthorizationMethodsSelector = mkSelector "customAuthorizationMethods"

-- | @Selector@ for @setCustomAuthorizationMethods:@
setCustomAuthorizationMethodsSelector :: Selector '[Id NSArray] ()
setCustomAuthorizationMethodsSelector = mkSelector "setCustomAuthorizationMethods:"

