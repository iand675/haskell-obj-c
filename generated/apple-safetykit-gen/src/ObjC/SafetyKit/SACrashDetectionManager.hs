{-# LANGUAGE PatternSynonyms #-}
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
  , requestAuthorizationWithCompletionHandlerSelector
  , availableSelector
  , authorizationStatusSelector
  , delegateSelector
  , setDelegateSelector

  -- * Enum types
  , SAAuthorizationStatus(SAAuthorizationStatus)
  , pattern SAAuthorizationStatusNotDetermined
  , pattern SAAuthorizationStatusDenied
  , pattern SAAuthorizationStatusAuthorized

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

import ObjC.SafetyKit.Internal.Classes
import ObjC.SafetyKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Requests the user’s permission to access Crash Detection information.
--
-- @handler@ — Completion handler invoked with the status of the authorization request.
--
-- ObjC selector: @- requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandler :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> Ptr () -> IO ()
requestAuthorizationWithCompletionHandler saCrashDetectionManager  handler =
    sendMsg saCrashDetectionManager (mkSelector "requestAuthorizationWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | available
--
-- Returns a  value indicating whether the current device supports Crash Detection.
--
-- ObjC selector: @+ available@
available :: IO Bool
available  =
  do
    cls' <- getRequiredClass "SACrashDetectionManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "available") retCULong []

-- | authorizationStatus
--
-- Returns a value indicating whether the user has authorized the app to receive Crash Detection updates
--
-- ObjC selector: @- authorizationStatus@
authorizationStatus :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> IO SAAuthorizationStatus
authorizationStatus saCrashDetectionManager  =
    fmap (coerce :: CLong -> SAAuthorizationStatus) $ sendMsg saCrashDetectionManager (mkSelector "authorizationStatus") retCLong []

-- | delegate
--
-- The delegate object to receive Crash Detection events.
--
-- ObjC selector: @- delegate@
delegate :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> IO RawId
delegate saCrashDetectionManager  =
    fmap (RawId . castPtr) $ sendMsg saCrashDetectionManager (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- The delegate object to receive Crash Detection events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSACrashDetectionManager saCrashDetectionManager => saCrashDetectionManager -> RawId -> IO ()
setDelegate saCrashDetectionManager  value =
    sendMsg saCrashDetectionManager (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestAuthorizationWithCompletionHandler:@
requestAuthorizationWithCompletionHandlerSelector :: Selector
requestAuthorizationWithCompletionHandlerSelector = mkSelector "requestAuthorizationWithCompletionHandler:"

-- | @Selector@ for @available@
availableSelector :: Selector
availableSelector = mkSelector "available"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

