{-# LANGUAGE PatternSynonyms #-}
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
  , startAccelerometerUpdatesSelector
  , stopAccelerometerUpdatesSelector
  , startDeviceMotionUpdatesSelector
  , stopDeviceMotionUpdatesSelector
  , authorizationStatusSelector
  , accelerometerSupportedSelector
  , accelerometerActiveSelector
  , accelerometerDataFrequencySelector
  , accelerometerBatchSelector
  , deviceMotionSupportedSelector
  , deviceMotionDataFrequencySelector
  , deviceMotionActiveSelector
  , deviceMotionBatchSelector

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

-- | @- startAccelerometerUpdates@
startAccelerometerUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
startAccelerometerUpdates cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "startAccelerometerUpdates") retVoid []

-- | @- stopAccelerometerUpdates@
stopAccelerometerUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
stopAccelerometerUpdates cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "stopAccelerometerUpdates") retVoid []

-- | @- startDeviceMotionUpdates@
startDeviceMotionUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
startDeviceMotionUpdates cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "startDeviceMotionUpdates") retVoid []

-- | @- stopDeviceMotionUpdates@
stopDeviceMotionUpdates :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO ()
stopDeviceMotionUpdates cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "stopDeviceMotionUpdates") retVoid []

-- | @+ authorizationStatus@
authorizationStatus :: IO CMAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "CMBatchedSensorManager"
    fmap (coerce :: CLong -> CMAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | @+ accelerometerSupported@
accelerometerSupported :: IO Bool
accelerometerSupported  =
  do
    cls' <- getRequiredClass "CMBatchedSensorManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "accelerometerSupported") retCULong []

-- | @- accelerometerActive@
accelerometerActive :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO Bool
accelerometerActive cmBatchedSensorManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmBatchedSensorManager (mkSelector "accelerometerActive") retCULong []

-- | @- accelerometerDataFrequency@
accelerometerDataFrequency :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO CLong
accelerometerDataFrequency cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "accelerometerDataFrequency") retCLong []

-- | @- accelerometerBatch@
accelerometerBatch :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO (Id NSArray)
accelerometerBatch cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "accelerometerBatch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ deviceMotionSupported@
deviceMotionSupported :: IO Bool
deviceMotionSupported  =
  do
    cls' <- getRequiredClass "CMBatchedSensorManager"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "deviceMotionSupported") retCULong []

-- | @- deviceMotionDataFrequency@
deviceMotionDataFrequency :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO CLong
deviceMotionDataFrequency cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "deviceMotionDataFrequency") retCLong []

-- | @- deviceMotionActive@
deviceMotionActive :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO Bool
deviceMotionActive cmBatchedSensorManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmBatchedSensorManager (mkSelector "deviceMotionActive") retCULong []

-- | @- deviceMotionBatch@
deviceMotionBatch :: IsCMBatchedSensorManager cmBatchedSensorManager => cmBatchedSensorManager -> IO (Id NSArray)
deviceMotionBatch cmBatchedSensorManager  =
  sendMsg cmBatchedSensorManager (mkSelector "deviceMotionBatch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startAccelerometerUpdates@
startAccelerometerUpdatesSelector :: Selector
startAccelerometerUpdatesSelector = mkSelector "startAccelerometerUpdates"

-- | @Selector@ for @stopAccelerometerUpdates@
stopAccelerometerUpdatesSelector :: Selector
stopAccelerometerUpdatesSelector = mkSelector "stopAccelerometerUpdates"

-- | @Selector@ for @startDeviceMotionUpdates@
startDeviceMotionUpdatesSelector :: Selector
startDeviceMotionUpdatesSelector = mkSelector "startDeviceMotionUpdates"

-- | @Selector@ for @stopDeviceMotionUpdates@
stopDeviceMotionUpdatesSelector :: Selector
stopDeviceMotionUpdatesSelector = mkSelector "stopDeviceMotionUpdates"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @accelerometerSupported@
accelerometerSupportedSelector :: Selector
accelerometerSupportedSelector = mkSelector "accelerometerSupported"

-- | @Selector@ for @accelerometerActive@
accelerometerActiveSelector :: Selector
accelerometerActiveSelector = mkSelector "accelerometerActive"

-- | @Selector@ for @accelerometerDataFrequency@
accelerometerDataFrequencySelector :: Selector
accelerometerDataFrequencySelector = mkSelector "accelerometerDataFrequency"

-- | @Selector@ for @accelerometerBatch@
accelerometerBatchSelector :: Selector
accelerometerBatchSelector = mkSelector "accelerometerBatch"

-- | @Selector@ for @deviceMotionSupported@
deviceMotionSupportedSelector :: Selector
deviceMotionSupportedSelector = mkSelector "deviceMotionSupported"

-- | @Selector@ for @deviceMotionDataFrequency@
deviceMotionDataFrequencySelector :: Selector
deviceMotionDataFrequencySelector = mkSelector "deviceMotionDataFrequency"

-- | @Selector@ for @deviceMotionActive@
deviceMotionActiveSelector :: Selector
deviceMotionActiveSelector = mkSelector "deviceMotionActive"

-- | @Selector@ for @deviceMotionBatch@
deviceMotionBatchSelector :: Selector
deviceMotionBatchSelector = mkSelector "deviceMotionBatch"

