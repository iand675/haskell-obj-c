{-# LANGUAGE PatternSynonyms #-}
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
  , initWithAuthorizationRequestsSelector
  , performRequestsSelector
  , performAutoFillAssistedRequestsSelector
  , performRequestsWithOptionsSelector
  , cancelSelector
  , newSelector
  , initSelector
  , authorizationRequestsSelector

  -- * Enum types
  , ASAuthorizationControllerRequestOptions(ASAuthorizationControllerRequestOptions)
  , pattern ASAuthorizationControllerRequestOptionPreferImmediatelyAvailableCredentials

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

-- | Initialize the controller with authorization requests.
--
-- @authorizationRequests@ — At least one request should be provided. Requests of same type maybe honored in first in first out order
--
-- ObjC selector: @- initWithAuthorizationRequests:@
initWithAuthorizationRequests :: (IsASAuthorizationController asAuthorizationController, IsNSArray authorizationRequests) => asAuthorizationController -> authorizationRequests -> IO (Id ASAuthorizationController)
initWithAuthorizationRequests asAuthorizationController  authorizationRequests =
withObjCPtr authorizationRequests $ \raw_authorizationRequests ->
    sendMsg asAuthorizationController (mkSelector "initWithAuthorizationRequests:") (retPtr retVoid) [argPtr (castPtr raw_authorizationRequests :: Ptr ())] >>= ownedObject . castPtr

-- | Initiate the authorization flows. Upon completion, the delegate will be called with either success or failure. Certain authorization flows may require a presentation context. The @presentationContextProvider@ will be called to provide it.
--
-- The instance will remain retained until the flow is either completed or canceled, and the delegate callback is made.
--
-- ObjC selector: @- performRequests@
performRequests :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO ()
performRequests asAuthorizationController  =
  sendMsg asAuthorizationController (mkSelector "performRequests") retVoid []

-- | Initiate the authorization flows for requests that support AutoFill presentation. UI will be shown when focusing a text field with the appropriate text content type. Upon completion, the delegate will be called with either success or failure.
--
-- The instance will remain retained until the flow is either completed or canceled, and the delegate callback is made.
--
-- ObjC selector: @- performAutoFillAssistedRequests@
performAutoFillAssistedRequests :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO ()
performAutoFillAssistedRequests asAuthorizationController  =
  sendMsg asAuthorizationController (mkSelector "performAutoFillAssistedRequests") retVoid []

-- | Initiate the authorization flows. Upon completion, the delegate will be called with either success or failure. Certain authorization flows may require a presentation context. The @presentationContextProvider@ will be called to provide it.
--
-- Calling this method with no options is the same as calling @performRequests.@ The instance will remain retained until the flow is either completed or canceled, and the delegate callback is made.
--
-- ObjC selector: @- performRequestsWithOptions:@
performRequestsWithOptions :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> ASAuthorizationControllerRequestOptions -> IO ()
performRequestsWithOptions asAuthorizationController  options =
  sendMsg asAuthorizationController (mkSelector "performRequestsWithOptions:") retVoid [argCULong (coerce options)]

-- | Cancel the running authorization flows, if there are any. If a flow is canceled, the delegate callback will be made indicating the cancel.
--
-- ObjC selector: @- cancel@
cancel :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO ()
cancel asAuthorizationController  =
  sendMsg asAuthorizationController (mkSelector "cancel") retVoid []

-- | @+ new@
new :: IO (Id ASAuthorizationController)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationController"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO (Id ASAuthorizationController)
init_ asAuthorizationController  =
  sendMsg asAuthorizationController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Authorization requests that are being serviced by this controller
--
-- ObjC selector: @- authorizationRequests@
authorizationRequests :: IsASAuthorizationController asAuthorizationController => asAuthorizationController -> IO (Id NSArray)
authorizationRequests asAuthorizationController  =
  sendMsg asAuthorizationController (mkSelector "authorizationRequests") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAuthorizationRequests:@
initWithAuthorizationRequestsSelector :: Selector
initWithAuthorizationRequestsSelector = mkSelector "initWithAuthorizationRequests:"

-- | @Selector@ for @performRequests@
performRequestsSelector :: Selector
performRequestsSelector = mkSelector "performRequests"

-- | @Selector@ for @performAutoFillAssistedRequests@
performAutoFillAssistedRequestsSelector :: Selector
performAutoFillAssistedRequestsSelector = mkSelector "performAutoFillAssistedRequests"

-- | @Selector@ for @performRequestsWithOptions:@
performRequestsWithOptionsSelector :: Selector
performRequestsWithOptionsSelector = mkSelector "performRequestsWithOptions:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @authorizationRequests@
authorizationRequestsSelector :: Selector
authorizationRequestsSelector = mkSelector "authorizationRequests"

