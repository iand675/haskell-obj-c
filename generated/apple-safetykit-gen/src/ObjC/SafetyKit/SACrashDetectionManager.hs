{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SACrashDetectionManager
--
-- Use SACrashDetectionManager to receive information about Vehicular Crash Detection events. Not all phone models support Crash Detection, check for availability before creating an instance of SACrashDetectionManager. Set the delegate immediately after creating an instance of SACrashDetectionManager. Creating multiple instances of SACrashDetectionManager is not supported and should be avoided.
--
-- SACrashDetectionManager requires an entitlement from Apple. To apply for the entitlement, see Crash Detection Entitlement Request.
--
-- Generated bindings for @SACrashDetectionManager@.
module ObjC.SafetyKit.SACrashDetectionManager
  ( SACrashDetectionManager
  , IsSACrashDetectionManager(..)
  , requestAuthorizationWithCompletionHandler
  , available
  , authorizationStatus
  , delegate
  , setDelegate
  , authorizationStatusSelector
  , availableSelector
  , delegateSelector
  , requestAuthorizationWithCompletionHandlerSelector
  , setDelegateSelector

  -- * Enum types
  , SAAuthorizationStatus(SAAuthorizationStatus)
  , pattern SAAuthorizationStatusNotDetermined
  , pattern SAAuthorizationStatusDenied
  , pattern SAAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafetyKit.Internal.Classes
import ObjC.SafetyKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Requests the user’s permission to access Crash Detection information.
--
-- @handler@ — Completion handler invoked with the status of the authorization request.
--
-- ObjC selector: @- requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandler :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> Ptr () -> IO ()
requestAuthorizationWithCompletionHandler saCrashDetectionManager handler =
  sendMessage saCrashDetectionManager requestAuthorizationWithCompletionHandlerSelector handler

-- | available
--
-- Returns a  value indicating whether the current device supports Crash Detection.
--
-- ObjC selector: @+ available@
available :: IO Bool
available  =
  do
    cls' <- getRequiredClass "SACrashDetectionManager"
    sendClassMessage cls' availableSelector

-- | authorizationStatus
--
-- Returns a value indicating whether the user has authorized the app to receive Crash Detection updates
--
-- ObjC selector: @- authorizationStatus@
authorizationStatus :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> IO SAAuthorizationStatus
authorizationStatus saCrashDetectionManager =
  sendMessage saCrashDetectionManager authorizationStatusSelector

-- | delegate
--
-- The delegate object to receive Crash Detection events.
--
-- ObjC selector: @- delegate@
delegate :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> IO RawId
delegate saCrashDetectionManager =
  sendMessage saCrashDetectionManager delegateSelector

-- | delegate
--
-- The delegate object to receive Crash Detection events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> RawId -> IO ()
setDelegate saCrashDetectionManager value =
  sendMessage saCrashDetectionManager setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
requestAuthorizationWithCompletionHandlerSelector = mkSelector "requestAuthorizationWithCompletionHandler:"

-- | @Selector@ for @available@
availableSelector :: Selector '[] Bool
availableSelector = mkSelector "available"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] SAAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

