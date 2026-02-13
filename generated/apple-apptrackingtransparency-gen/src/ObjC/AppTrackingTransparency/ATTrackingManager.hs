{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that provides a tracking authorization request and the tracking authorization status of the app.
--
-- Generated bindings for @ATTrackingManager@.
module ObjC.AppTrackingTransparency.ATTrackingManager
  ( ATTrackingManager
  , IsATTrackingManager(..)
  , requestTrackingAuthorizationWithCompletionHandler
  , new
  , init_
  , trackingAuthorizationStatus
  , initSelector
  , newSelector
  , requestTrackingAuthorizationWithCompletionHandlerSelector
  , trackingAuthorizationStatusSelector

  -- * Enum types
  , ATTrackingManagerAuthorizationStatus(ATTrackingManagerAuthorizationStatus)
  , pattern ATTrackingManagerAuthorizationStatusNotDetermined
  , pattern ATTrackingManagerAuthorizationStatusRestricted
  , pattern ATTrackingManagerAuthorizationStatusDenied
  , pattern ATTrackingManagerAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppTrackingTransparency.Internal.Classes
import ObjC.AppTrackingTransparency.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The request for user authorization to access app-related data.
--
-- The ``ATTrackingManager/requestTrackingAuthorizationWithCompletionHandler:`` is a one-time request to authorize or deny access to app-related data that can be used for tracking the user or the device. The system remembers the user’s choice and doesn’t prompt again unless a user uninstalls and then reinstalls the app on the device.
--
-- Calls to the API only prompt when the application state is @UIApplicationStateActive@. The authorization prompt doesn’t display if another permission request is pending user confirmation. Concurrent requests aren’t preserved by iOS, and calls to the API through an app extension don’t prompt. Check the ``ATTrackingManager/trackingAuthorizationStatus`` for a status of ``ATTrackingManagerAuthorizationStatus/ATTrackingManagerAuthorizationStatusNotDetermined`` to determine if you need to make an additional call.
--
-- The completion handler will be called with the result of the user's decision for granting or denying permission to use application tracking. The completion handler will be called immediately if access to request authorization is restricted.
--
-- - Important: To use ``ATTrackingManager/requestTrackingAuthorizationWithCompletionHandler:``, the <doc://com.apple.documentation/documentation/bundleresources/information_property_list/NSUserTrackingUsageDescription> key must be in the <doc://com.apple.documentation/documentation/bundleresources/information_property_list>.
--
-- ObjC selector: @+ requestTrackingAuthorizationWithCompletionHandler:@
requestTrackingAuthorizationWithCompletionHandler :: Ptr () -> IO ()
requestTrackingAuthorizationWithCompletionHandler completion =
  do
    cls' <- getRequiredClass "ATTrackingManager"
    sendClassMessage cls' requestTrackingAuthorizationWithCompletionHandlerSelector completion

-- | @+ new@
new :: IO (Id ATTrackingManager)
new  =
  do
    cls' <- getRequiredClass "ATTrackingManager"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsATTrackingManager atTrackingManager => atTrackingManager -> IO (Id ATTrackingManager)
init_ atTrackingManager =
  sendOwnedMessage atTrackingManager initSelector

-- | The authorization status that is current for the calling application.
--
-- If the user has not yet been prompted to approve access, the return value will either be ``ATTrackingManagerAuthorizationStatusNotDetermined``, or ``ATTrackingManagerAuthorizationStatusRestricted`` if this value is managed. Once the user has been prompted, the return value will be either ``ATTrackingManagerAuthorizationStatusDenied`` or ``ATTrackingManagerAuthorizationStatusAuthorized``.
--
-- Use the ``ATTrackingManager/trackingAuthorizationStatus`` property to check authorization status.
--
-- - Returns: Information about your application’s tracking authorization   status. Users are able to grant or deny developers tracking privileges on   a per-app basis. Application developers must call   @requestTrackingAuthorizationWithCompletionHandler:@ for the ability to   track users.
--
-- ObjC selector: @+ trackingAuthorizationStatus@
trackingAuthorizationStatus :: IO ATTrackingManagerAuthorizationStatus
trackingAuthorizationStatus  =
  do
    cls' <- getRequiredClass "ATTrackingManager"
    sendClassMessage cls' trackingAuthorizationStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestTrackingAuthorizationWithCompletionHandler:@
requestTrackingAuthorizationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestTrackingAuthorizationWithCompletionHandlerSelector = mkSelector "requestTrackingAuthorizationWithCompletionHandler:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ATTrackingManager)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ATTrackingManager)
initSelector = mkSelector "init"

-- | @Selector@ for @trackingAuthorizationStatus@
trackingAuthorizationStatusSelector :: Selector '[] ATTrackingManagerAuthorizationStatus
trackingAuthorizationStatusSelector = mkSelector "trackingAuthorizationStatus"

