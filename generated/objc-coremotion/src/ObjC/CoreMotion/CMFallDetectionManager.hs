{-# LANGUAGE PatternSynonyms #-}
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
  , requestAuthorizationWithHandlerSelector
  , availableSelector
  , authorizationStatusSelector

  -- * Enum types
  , CMAuthorizationStatus(CMAuthorizationStatus)
  , pattern CMAuthorizationStatusNotDetermined
  , pattern CMAuthorizationStatusRestricted
  , pattern CMAuthorizationStatusDenied
  , pattern CMAuthorizationStatusAuthorized

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

import ObjC.CoreMotion.Internal.Classes
import ObjC.CoreMotion.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Requests the user’s permission to access Fall Detection information.
--
-- ObjC selector: @- requestAuthorizationWithHandler:@
requestAuthorizationWithHandler :: IsCMFallDetectionManager cmFallDetectionManager => cmFallDetectionManager -> Ptr () -> IO ()
requestAuthorizationWithHandler cmFallDetectionManager  handler =
  sendMsg cmFallDetectionManager (mkSelector "requestAuthorizationWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | available
--
-- Returns a  value indicating whether the current device supports Fall Detection.
--
-- ObjC selector: @+ available@
available :: IO Bool
available  =
  do
    cls' <- getRequiredClass "CMFallDetectionManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "available") retCULong []

-- | authorizationStatus
--
-- Returns a value indicating whether the user has authorized the app to receive Fall Detection updates
--
-- ObjC selector: @- authorizationStatus@
authorizationStatus :: IsCMFallDetectionManager cmFallDetectionManager => cmFallDetectionManager -> IO CMAuthorizationStatus
authorizationStatus cmFallDetectionManager  =
  fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendMsg cmFallDetectionManager (mkSelector "authorizationStatus") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestAuthorizationWithHandler:@
requestAuthorizationWithHandlerSelector :: Selector
requestAuthorizationWithHandlerSelector = mkSelector "requestAuthorizationWithHandler:"

-- | @Selector@ for @available@
availableSelector :: Selector
availableSelector = mkSelector "available"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

