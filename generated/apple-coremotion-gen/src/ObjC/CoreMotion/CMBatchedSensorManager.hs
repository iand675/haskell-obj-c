{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMBatchedSensorManager@.
module ObjC.CoreMotion.CMBatchedSensorManager
  ( CMBatchedSensorManager
  , IsCMBatchedSensorManager(..)
  , startAccelerometerUpdates
  , stopAccelerometerUpdates
  , startDeviceMotionUpdates
  , stopDeviceMotionUpdates
  , authorizationStatus
  , accelerometerSupported
  , accelerometerActive
  , accelerometerDataFrequency
  , accelerometerBatch
  , deviceMotionSupported
  , deviceMotionDataFrequency
  , deviceMotionActive
  , deviceMotionBatch
  , accelerometerActiveSelector
  , accelerometerBatchSelector
  , accelerometerDataFrequencySelector
  , accelerometerSupportedSelector
  , authorizationStatusSelector
  , deviceMotionActiveSelector
  , deviceMotionBatchSelector
  , deviceMotionDataFrequencySelector
  , deviceMotionSupportedSelector
  , startAccelerometerUpdatesSelector
  , startDeviceMotionUpdatesSelector
  , stopAccelerometerUpdatesSelector
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

-- | @- startAccelerometerUpdates@
startAccelerometerUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
startAccelerometerUpdates cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager startAccelerometerUpdatesSelector

-- | @- stopAccelerometerUpdates@
stopAccelerometerUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
stopAccelerometerUpdates cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager stopAccelerometerUpdatesSelector

-- | @- startDeviceMotionUpdates@
startDeviceMotionUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
startDeviceMotionUpdates cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager startDeviceMotionUpdatesSelector

-- | @- stopDeviceMotionUpdates@
stopDeviceMotionUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
stopDeviceMotionUpdates cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager stopDeviceMotionUpdatesSelector

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMBatchedSensorManager"
    sendClassMessage cls' authorizationStatusSelector

-- | @+ accelerometerSupported@
accelerometerSupported :: IO Bool
accelerometerSupported  =
  do
    cls' <- getRequiredClass "CMBatchedSensorManager"
    sendClassMessage cls' accelerometerSupportedSelector

-- | @- accelerometerActive@
accelerometerActive :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO Bool
accelerometerActive cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager accelerometerActiveSelector

-- | @- accelerometerDataFrequency@
accelerometerDataFrequency :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO CLong
accelerometerDataFrequency cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager accelerometerDataFrequencySelector

-- | @- accelerometerBatch@
accelerometerBatch :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO (Id NSArray)
accelerometerBatch cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager accelerometerBatchSelector

-- | @+ deviceMotionSupported@
deviceMotionSupported :: IO Bool
deviceMotionSupported  =
  do
    cls' <- getRequiredClass "CMBatchedSensorManager"
    sendClassMessage cls' deviceMotionSupportedSelector

-- | @- deviceMotionDataFrequency@
deviceMotionDataFrequency :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO CLong
deviceMotionDataFrequency cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager deviceMotionDataFrequencySelector

-- | @- deviceMotionActive@
deviceMotionActive :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO Bool
deviceMotionActive cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager deviceMotionActiveSelector

-- | @- deviceMotionBatch@
deviceMotionBatch :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO (Id NSArray)
deviceMotionBatch cmBatchedSensorManager =
  sendMessage cmBatchedSensorManager deviceMotionBatchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startAccelerometerUpdates@
startAccelerometerUpdatesSelector :: Selector '[] ()
startAccelerometerUpdatesSelector = mkSelector "startAccelerometerUpdates"

-- | @Selector@ for @stopAccelerometerUpdates@
stopAccelerometerUpdatesSelector :: Selector '[] ()
stopAccelerometerUpdatesSelector = mkSelector "stopAccelerometerUpdates"

-- | @Selector@ for @startDeviceMotionUpdates@
startDeviceMotionUpdatesSelector :: Selector '[] ()
startDeviceMotionUpdatesSelector = mkSelector "startDeviceMotionUpdates"

-- | @Selector@ for @stopDeviceMotionUpdates@
stopDeviceMotionUpdatesSelector :: Selector '[] ()
stopDeviceMotionUpdatesSelector = mkSelector "stopDeviceMotionUpdates"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector '[] CMAuthorizationStatus
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @accelerometerSupported@
accelerometerSupportedSelector :: Selector '[] Bool
accelerometerSupportedSelector = mkSelector "accelerometerSupported"

-- | @Selector@ for @accelerometerActive@
accelerometerActiveSelector :: Selector '[] Bool
accelerometerActiveSelector = mkSelector "accelerometerActive"

-- | @Selector@ for @accelerometerDataFrequency@
accelerometerDataFrequencySelector :: Selector '[] CLong
accelerometerDataFrequencySelector = mkSelector "accelerometerDataFrequency"

-- | @Selector@ for @accelerometerBatch@
accelerometerBatchSelector :: Selector '[] (Id NSArray)
accelerometerBatchSelector = mkSelector "accelerometerBatch"

-- | @Selector@ for @deviceMotionSupported@
deviceMotionSupportedSelector :: Selector '[] Bool
deviceMotionSupportedSelector = mkSelector "deviceMotionSupported"

-- | @Selector@ for @deviceMotionDataFrequency@
deviceMotionDataFrequencySelector :: Selector '[] CLong
deviceMotionDataFrequencySelector = mkSelector "deviceMotionDataFrequency"

-- | @Selector@ for @deviceMotionActive@
deviceMotionActiveSelector :: Selector '[] Bool
deviceMotionActiveSelector = mkSelector "deviceMotionActive"

-- | @Selector@ for @deviceMotionBatch@
deviceMotionBatchSelector :: Selector '[] (Id NSArray)
deviceMotionBatchSelector = mkSelector "deviceMotionBatch"

