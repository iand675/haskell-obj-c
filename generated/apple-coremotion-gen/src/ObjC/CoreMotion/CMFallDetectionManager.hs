{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMFallDetectionManager
--
-- Use CMFallDetectionManager to receive information about Fall Detection events. Not all watch models support Fall Detection, check for availability before creating an instance of CMFallDetectionManager.  CMFallDetectionManager requires an entitlement from Apple. To apply for the entitlement, see Fall Detection Entitlement Request.
--
-- Set the delegate immediately after creating an instance of CMFallDetectionManager. Creating multiple instances of CMFallDetectionManager is not supported and should be avoided.
--
-- Generated bindings for @CMFallDetectionManager@.
module ObjC.CoreMotion.CMFallDetectionManager
  ( CMFallDetectionManager
  , IsCMFallDetectionManager(..)
  , requestAuthorizationWithHandler
  , available
  , authorizationStatus
  , delegate
  , setDelegate
  , authorizationStatusSelector
  , availableSelector
  , delegateSelector
  , requestAuthorizationWithHandlerSelector
  , setDelegateSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Requests the user’s permission to access Fall Detection information.
--
-- ObjC selector: @- requestAuthorizationWithHandler:@
requestAuthorizationWithHandler :: IsCMFallDetectionManager cmFallDetectionManager => cmFallDetectionManager -> Ptr () -> IO ()
requestAuthorizationWithHandler cmFallDetectionManager handler =
  sendMessage cmFallDetectionManager requestAuthorizationWithHandlerSelector handler

-- | available
--
-- Returns a  value indicating whether the current device supports Fall Detection.
--
-- ObjC selector: @+ available@
available :: IO Bool
available  =
  do
    cls' <- getRequiredClass "CMFallDetectionManager"
    sendClassMessage cls' availableSelector

-- | authorizationStatus
--
-- Returns a value indicating whether the user has authorized the app to receive Fall Detection updates
--
-- ObjC selector: @- authorizationStatus@
authorizationStatus :: IsCMFallDetectionManager cmFallDetectionManager => cmFallDetectionManager -> IO CMAuthorizationStatus
authorizationStatus cmFallDetectionManager =
  sendMessage cmFallDetectionManager authorizationStatusSelector

-- | delegate
--
-- The delegate object to receive Fall Detection events.
--
-- ObjC selector: @- delegate@
delegate :: IsCMFallDetectionManager cmFallDetectionManager => cmFallDetectionManager -> IO RawId
delegate cmFallDetectionManager =
  sendMessage cmFallDetectionManager delegateSelector

-- | delegate
--
-- The delegate object to receive Fall Detection events.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsCMFallDetectionManager cmFallDetectionManager => cmFallDetectionManager -> RawId -> IO ()
setDelegate cmFallDetectionManager value =
  sendMessage cmFallDetectionManager setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestAuthorizationWithHandler:@
requestAuthorizationWithHandlerSelector :: Selector '[Ptr ()] ()
requestAuthorizationWithHandlerSelector = mkSelector "requestAuthorizationWithHandler:"

-- | @Selector@ for @available@
availableSelector :: Selector '[] Bool
availableSelector = mkSelector "available"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

