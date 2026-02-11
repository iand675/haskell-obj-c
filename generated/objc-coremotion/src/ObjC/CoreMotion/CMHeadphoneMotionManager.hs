{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMHeadphoneMotionManager@.
module ObjC.CoreMotion.CMHeadphoneMotionManager
  ( CMHeadphoneMotionManager
  , IsCMHeadphoneMotionManager(..)
  , authorizationStatus
  , startDeviceMotionUpdates
  , startDeviceMotionUpdatesToQueue_withHandler
  , stopDeviceMotionUpdates
  , startConnectionStatusUpdates
  , stopConnectionStatusUpdates
  , connectionStatusActive
  , deviceMotionAvailable
  , deviceMotionActive
  , deviceMotion
  , authorizationStatusSelector
  , startDeviceMotionUpdatesSelector
  , startDeviceMotionUpdatesToQueue_withHandlerSelector
  , stopDeviceMotionUpdatesSelector
  , startConnectionStatusUpdatesSelector
  , stopConnectionStatusUpdatesSelector
  , connectionStatusActiveSelector
  , deviceMotionAvailableSelector
  , deviceMotionActiveSelector
  , deviceMotionSelector

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

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMHeadphoneMotionManager"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @- startDeviceMotionUpdates@
startDeviceMotionUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
startDeviceMotionUpdates cmHeadphoneMotionManager  =
  sendMsg cmHeadphoneMotionManager (mkSelector "startDeviceMotionUpdates") retVoid []

-- | @- startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandler :: (IsCMHeadphoneMotionManager cmHeadphoneMotionManager, IsNSOperationQueue queue) => cmHeadphoneMotionManager -> queue -> Ptr () -> IO ()
startDeviceMotionUpdatesToQueue_withHandler cmHeadphoneMotionManager  queue handler =
withObjCPtr queue $ \raw_queue ->
    sendMsg cmHeadphoneMotionManager (mkSelector "startDeviceMotionUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopDeviceMotionUpdates@
stopDeviceMotionUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
stopDeviceMotionUpdates cmHeadphoneMotionManager  =
  sendMsg cmHeadphoneMotionManager (mkSelector "stopDeviceMotionUpdates") retVoid []

-- | @- startConnectionStatusUpdates@
startConnectionStatusUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
startConnectionStatusUpdates cmHeadphoneMotionManager  =
  sendMsg cmHeadphoneMotionManager (mkSelector "startConnectionStatusUpdates") retVoid []

-- | @- stopConnectionStatusUpdates@
stopConnectionStatusUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
stopConnectionStatusUpdates cmHeadphoneMotionManager  =
  sendMsg cmHeadphoneMotionManager (mkSelector "stopConnectionStatusUpdates") retVoid []

-- | @- connectionStatusActive@
connectionStatusActive :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO Bool
connectionStatusActive cmHeadphoneMotionManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmHeadphoneMotionManager (mkSelector "connectionStatusActive") retCULong []

-- | @- deviceMotionAvailable@
deviceMotionAvailable :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO Bool
deviceMotionAvailable cmHeadphoneMotionManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmHeadphoneMotionManager (mkSelector "deviceMotionAvailable") retCULong []

-- | @- deviceMotionActive@
deviceMotionActive :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO Bool
deviceMotionActive cmHeadphoneMotionManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmHeadphoneMotionManager (mkSelector "deviceMotionActive") retCULong []

-- | @- deviceMotion@
deviceMotion :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO (Id CMDeviceMotion)
deviceMotion cmHeadphoneMotionManager  =
  sendMsg cmHeadphoneMotionManager (mkSelector "deviceMotion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @startDeviceMotionUpdates@
startDeviceMotionUpdatesSelector :: Selector
startDeviceMotionUpdatesSelector = mkSelector "startDeviceMotionUpdates"

-- | @Selector@ for @startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandlerSelector :: Selector
startDeviceMotionUpdatesToQueue_withHandlerSelector = mkSelector "startDeviceMotionUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopDeviceMotionUpdates@
stopDeviceMotionUpdatesSelector :: Selector
stopDeviceMotionUpdatesSelector = mkSelector "stopDeviceMotionUpdates"

-- | @Selector@ for @startConnectionStatusUpdates@
startConnectionStatusUpdatesSelector :: Selector
startConnectionStatusUpdatesSelector = mkSelector "startConnectionStatusUpdates"

-- | @Selector@ for @stopConnectionStatusUpdates@
stopConnectionStatusUpdatesSelector :: Selector
stopConnectionStatusUpdatesSelector = mkSelector "stopConnectionStatusUpdates"

-- | @Selector@ for @connectionStatusActive@
connectionStatusActiveSelector :: Selector
connectionStatusActiveSelector = mkSelector "connectionStatusActive"

-- | @Selector@ for @deviceMotionAvailable@
deviceMotionAvailableSelector :: Selector
deviceMotionAvailableSelector = mkSelector "deviceMotionAvailable"

-- | @Selector@ for @deviceMotionActive@
deviceMotionActiveSelector :: Selector
deviceMotionActiveSelector = mkSelector "deviceMotionActive"

-- | @Selector@ for @deviceMotion@
deviceMotionSelector :: Selector
deviceMotionSelector = mkSelector "deviceMotion"

