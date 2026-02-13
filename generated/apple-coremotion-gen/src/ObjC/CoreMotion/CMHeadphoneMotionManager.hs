{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , connectionStatusActive
  , deviceMotionAvailable
  , deviceMotionActive
  , deviceMotion
  , authorizationStatusSelector
  , connectionStatusActiveSelector
  , delegateSelector
  , deviceMotionActiveSelector
  , deviceMotionAvailableSelector
  , deviceMotionSelector
  , setDelegateSelector
  , startConnectionStatusUpdatesSelector
  , startDeviceMotionUpdatesSelector
  , startDeviceMotionUpdatesToQueue_withHandlerSelector
  , stopConnectionStatusUpdatesSelector
  , stopDeviceMotionUpdatesSelector

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

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMHeadphoneMotionManager"
    sendClassMessage cls' authorizationStatusSelector

-- | @- startDeviceMotionUpdates@
startDeviceMotionUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
startDeviceMotionUpdates cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager startDeviceMotionUpdatesSelector

-- | @- startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandler :: (IsCMHeadphoneMotionManager cmHeadphoneMotionManager, IsNSOperationQueue queue) => cmHeadphoneMotionManager -> queue -> Ptr () -> IO ()
startDeviceMotionUpdatesToQueue_withHandler cmHeadphoneMotionManager queue handler =
  sendMessage cmHeadphoneMotionManager startDeviceMotionUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopDeviceMotionUpdates@
stopDeviceMotionUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
stopDeviceMotionUpdates cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager stopDeviceMotionUpdatesSelector

-- | @- startConnectionStatusUpdates@
startConnectionStatusUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
startConnectionStatusUpdates cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager startConnectionStatusUpdatesSelector

-- | @- stopConnectionStatusUpdates@
stopConnectionStatusUpdates :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO ()
stopConnectionStatusUpdates cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager stopConnectionStatusUpdatesSelector

-- | @- delegate@
delegate :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO RawId
delegate cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> RawId -> IO ()
setDelegate cmHeadphoneMotionManager value =
  sendMessage cmHeadphoneMotionManager setDelegateSelector value

-- | @- connectionStatusActive@
connectionStatusActive :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO Bool
connectionStatusActive cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager connectionStatusActiveSelector

-- | @- deviceMotionAvailable@
deviceMotionAvailable :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO Bool
deviceMotionAvailable cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager deviceMotionAvailableSelector

-- | @- deviceMotionActive@
deviceMotionActive :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO Bool
deviceMotionActive cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager deviceMotionActiveSelector

-- | @- deviceMotion@
deviceMotion :: IsCMHeadphoneMotionManager cmHeadphoneMotionManager => cmHeadphoneMotionManager -> IO (Id CMDeviceMotion)
deviceMotion cmHeadphoneMotionManager =
  sendMessage cmHeadphoneMotionManager deviceMotionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @startDeviceMotionUpdates@
startDeviceMotionUpdatesSelector :: Selector '[] ()
startDeviceMotionUpdatesSelector = mkSelector "startDeviceMotionUpdates"

-- | @Selector@ for @startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startDeviceMotionUpdatesToQueue_withHandlerSelector = mkSelector "startDeviceMotionUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopDeviceMotionUpdates@
stopDeviceMotionUpdatesSelector :: Selector '[] ()
stopDeviceMotionUpdatesSelector = mkSelector "stopDeviceMotionUpdates"

-- | @Selector@ for @startConnectionStatusUpdates@
startConnectionStatusUpdatesSelector :: Selector '[] ()
startConnectionStatusUpdatesSelector = mkSelector "startConnectionStatusUpdates"

-- | @Selector@ for @stopConnectionStatusUpdates@
stopConnectionStatusUpdatesSelector :: Selector '[] ()
stopConnectionStatusUpdatesSelector = mkSelector "stopConnectionStatusUpdates"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @connectionStatusActive@
connectionStatusActiveSelector :: Selector '[] Bool
connectionStatusActiveSelector = mkSelector "connectionStatusActive"

-- | @Selector@ for @deviceMotionAvailable@
deviceMotionAvailableSelector :: Selector '[] Bool
deviceMotionAvailableSelector = mkSelector "deviceMotionAvailable"

-- | @Selector@ for @deviceMotionActive@
deviceMotionActiveSelector :: Selector '[] Bool
deviceMotionActiveSelector = mkSelector "deviceMotionActive"

-- | @Selector@ for @deviceMotion@
deviceMotionSelector :: Selector '[] (Id CMDeviceMotion)
deviceMotionSelector = mkSelector "deviceMotion"

