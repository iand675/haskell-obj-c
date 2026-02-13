{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMMotionManager@.
module ObjC.CoreMotion.CMMotionManager
  ( CMMotionManager
  , IsCMMotionManager(..)
  , startAccelerometerUpdates
  , startAccelerometerUpdatesToQueue_withHandler
  , stopAccelerometerUpdates
  , startGyroUpdates
  , startGyroUpdatesToQueue_withHandler
  , stopGyroUpdates
  , startMagnetometerUpdates
  , startMagnetometerUpdatesToQueue_withHandler
  , stopMagnetometerUpdates
  , availableAttitudeReferenceFrames
  , startDeviceMotionUpdates
  , startDeviceMotionUpdatesToQueue_withHandler
  , startDeviceMotionUpdatesUsingReferenceFrame
  , startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandler
  , stopDeviceMotionUpdates
  , accelerometerUpdateInterval
  , setAccelerometerUpdateInterval
  , accelerometerAvailable
  , accelerometerActive
  , accelerometerData
  , gyroUpdateInterval
  , setGyroUpdateInterval
  , gyroAvailable
  , gyroActive
  , gyroData
  , magnetometerUpdateInterval
  , setMagnetometerUpdateInterval
  , magnetometerAvailable
  , magnetometerActive
  , magnetometerData
  , deviceMotionUpdateInterval
  , setDeviceMotionUpdateInterval
  , attitudeReferenceFrame
  , deviceMotionAvailable
  , deviceMotionActive
  , deviceMotion
  , showsDeviceMovementDisplay
  , setShowsDeviceMovementDisplay
  , accelerometerActiveSelector
  , accelerometerAvailableSelector
  , accelerometerDataSelector
  , accelerometerUpdateIntervalSelector
  , attitudeReferenceFrameSelector
  , availableAttitudeReferenceFramesSelector
  , deviceMotionActiveSelector
  , deviceMotionAvailableSelector
  , deviceMotionSelector
  , deviceMotionUpdateIntervalSelector
  , gyroActiveSelector
  , gyroAvailableSelector
  , gyroDataSelector
  , gyroUpdateIntervalSelector
  , magnetometerActiveSelector
  , magnetometerAvailableSelector
  , magnetometerDataSelector
  , magnetometerUpdateIntervalSelector
  , setAccelerometerUpdateIntervalSelector
  , setDeviceMotionUpdateIntervalSelector
  , setGyroUpdateIntervalSelector
  , setMagnetometerUpdateIntervalSelector
  , setShowsDeviceMovementDisplaySelector
  , showsDeviceMovementDisplaySelector
  , startAccelerometerUpdatesSelector
  , startAccelerometerUpdatesToQueue_withHandlerSelector
  , startDeviceMotionUpdatesSelector
  , startDeviceMotionUpdatesToQueue_withHandlerSelector
  , startDeviceMotionUpdatesUsingReferenceFrameSelector
  , startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandlerSelector
  , startGyroUpdatesSelector
  , startGyroUpdatesToQueue_withHandlerSelector
  , startMagnetometerUpdatesSelector
  , startMagnetometerUpdatesToQueue_withHandlerSelector
  , stopAccelerometerUpdatesSelector
  , stopDeviceMotionUpdatesSelector
  , stopGyroUpdatesSelector
  , stopMagnetometerUpdatesSelector

  -- * Enum types
  , CMAttitudeReferenceFrame(CMAttitudeReferenceFrame)
  , pattern CMAttitudeReferenceFrameXArbitraryZVertical
  , pattern CMAttitudeReferenceFrameXArbitraryCorrectedZVertical
  , pattern CMAttitudeReferenceFrameXMagneticNorthZVertical
  , pattern CMAttitudeReferenceFrameXTrueNorthZVertical

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
startAccelerometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startAccelerometerUpdates cmMotionManager =
  sendMessage cmMotionManager startAccelerometerUpdatesSelector

-- | @- startAccelerometerUpdatesToQueue:withHandler:@
startAccelerometerUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startAccelerometerUpdatesToQueue_withHandler cmMotionManager queue handler =
  sendMessage cmMotionManager startAccelerometerUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopAccelerometerUpdates@
stopAccelerometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopAccelerometerUpdates cmMotionManager =
  sendMessage cmMotionManager stopAccelerometerUpdatesSelector

-- | @- startGyroUpdates@
startGyroUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startGyroUpdates cmMotionManager =
  sendMessage cmMotionManager startGyroUpdatesSelector

-- | @- startGyroUpdatesToQueue:withHandler:@
startGyroUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startGyroUpdatesToQueue_withHandler cmMotionManager queue handler =
  sendMessage cmMotionManager startGyroUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopGyroUpdates@
stopGyroUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopGyroUpdates cmMotionManager =
  sendMessage cmMotionManager stopGyroUpdatesSelector

-- | @- startMagnetometerUpdates@
startMagnetometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startMagnetometerUpdates cmMotionManager =
  sendMessage cmMotionManager startMagnetometerUpdatesSelector

-- | @- startMagnetometerUpdatesToQueue:withHandler:@
startMagnetometerUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startMagnetometerUpdatesToQueue_withHandler cmMotionManager queue handler =
  sendMessage cmMotionManager startMagnetometerUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- stopMagnetometerUpdates@
stopMagnetometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopMagnetometerUpdates cmMotionManager =
  sendMessage cmMotionManager stopMagnetometerUpdatesSelector

-- | @+ availableAttitudeReferenceFrames@
availableAttitudeReferenceFrames :: IO CMAttitudeReferenceFrame
availableAttitudeReferenceFrames  =
  do
    cls' <- getRequiredClass "CMMotionManager"
    sendClassMessage cls' availableAttitudeReferenceFramesSelector

-- | @- startDeviceMotionUpdates@
startDeviceMotionUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startDeviceMotionUpdates cmMotionManager =
  sendMessage cmMotionManager startDeviceMotionUpdatesSelector

-- | @- startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startDeviceMotionUpdatesToQueue_withHandler cmMotionManager queue handler =
  sendMessage cmMotionManager startDeviceMotionUpdatesToQueue_withHandlerSelector (toNSOperationQueue queue) handler

-- | @- startDeviceMotionUpdatesUsingReferenceFrame:@
startDeviceMotionUpdatesUsingReferenceFrame :: IsCMMotionManager cmMotionManager => cmMotionManager -> CMAttitudeReferenceFrame -> IO ()
startDeviceMotionUpdatesUsingReferenceFrame cmMotionManager referenceFrame =
  sendMessage cmMotionManager startDeviceMotionUpdatesUsingReferenceFrameSelector referenceFrame

-- | @- startDeviceMotionUpdatesUsingReferenceFrame:toQueue:withHandler:@
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> CMAttitudeReferenceFrame -> queue -> Ptr () -> IO ()
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandler cmMotionManager referenceFrame queue handler =
  sendMessage cmMotionManager startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandlerSelector referenceFrame (toNSOperationQueue queue) handler

-- | @- stopDeviceMotionUpdates@
stopDeviceMotionUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopDeviceMotionUpdates cmMotionManager =
  sendMessage cmMotionManager stopDeviceMotionUpdatesSelector

-- | @- accelerometerUpdateInterval@
accelerometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
accelerometerUpdateInterval cmMotionManager =
  sendMessage cmMotionManager accelerometerUpdateIntervalSelector

-- | @- setAccelerometerUpdateInterval:@
setAccelerometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setAccelerometerUpdateInterval cmMotionManager value =
  sendMessage cmMotionManager setAccelerometerUpdateIntervalSelector value

-- | @- accelerometerAvailable@
accelerometerAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
accelerometerAvailable cmMotionManager =
  sendMessage cmMotionManager accelerometerAvailableSelector

-- | @- accelerometerActive@
accelerometerActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
accelerometerActive cmMotionManager =
  sendMessage cmMotionManager accelerometerActiveSelector

-- | @- accelerometerData@
accelerometerData :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO (Id CMAccelerometerData)
accelerometerData cmMotionManager =
  sendMessage cmMotionManager accelerometerDataSelector

-- | @- gyroUpdateInterval@
gyroUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
gyroUpdateInterval cmMotionManager =
  sendMessage cmMotionManager gyroUpdateIntervalSelector

-- | @- setGyroUpdateInterval:@
setGyroUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setGyroUpdateInterval cmMotionManager value =
  sendMessage cmMotionManager setGyroUpdateIntervalSelector value

-- | @- gyroAvailable@
gyroAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
gyroAvailable cmMotionManager =
  sendMessage cmMotionManager gyroAvailableSelector

-- | @- gyroActive@
gyroActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
gyroActive cmMotionManager =
  sendMessage cmMotionManager gyroActiveSelector

-- | @- gyroData@
gyroData :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO (Id CMGyroData)
gyroData cmMotionManager =
  sendMessage cmMotionManager gyroDataSelector

-- | @- magnetometerUpdateInterval@
magnetometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
magnetometerUpdateInterval cmMotionManager =
  sendMessage cmMotionManager magnetometerUpdateIntervalSelector

-- | @- setMagnetometerUpdateInterval:@
setMagnetometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setMagnetometerUpdateInterval cmMotionManager value =
  sendMessage cmMotionManager setMagnetometerUpdateIntervalSelector value

-- | @- magnetometerAvailable@
magnetometerAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
magnetometerAvailable cmMotionManager =
  sendMessage cmMotionManager magnetometerAvailableSelector

-- | @- magnetometerActive@
magnetometerActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
magnetometerActive cmMotionManager =
  sendMessage cmMotionManager magnetometerActiveSelector

-- | @- magnetometerData@
magnetometerData :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO RawId
magnetometerData cmMotionManager =
  sendMessage cmMotionManager magnetometerDataSelector

-- | @- deviceMotionUpdateInterval@
deviceMotionUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
deviceMotionUpdateInterval cmMotionManager =
  sendMessage cmMotionManager deviceMotionUpdateIntervalSelector

-- | @- setDeviceMotionUpdateInterval:@
setDeviceMotionUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setDeviceMotionUpdateInterval cmMotionManager value =
  sendMessage cmMotionManager setDeviceMotionUpdateIntervalSelector value

-- | @- attitudeReferenceFrame@
attitudeReferenceFrame :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CMAttitudeReferenceFrame
attitudeReferenceFrame cmMotionManager =
  sendMessage cmMotionManager attitudeReferenceFrameSelector

-- | @- deviceMotionAvailable@
deviceMotionAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
deviceMotionAvailable cmMotionManager =
  sendMessage cmMotionManager deviceMotionAvailableSelector

-- | @- deviceMotionActive@
deviceMotionActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
deviceMotionActive cmMotionManager =
  sendMessage cmMotionManager deviceMotionActiveSelector

-- | @- deviceMotion@
deviceMotion :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO (Id CMDeviceMotion)
deviceMotion cmMotionManager =
  sendMessage cmMotionManager deviceMotionSelector

-- | @- showsDeviceMovementDisplay@
showsDeviceMovementDisplay :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
showsDeviceMovementDisplay cmMotionManager =
  sendMessage cmMotionManager showsDeviceMovementDisplaySelector

-- | @- setShowsDeviceMovementDisplay:@
setShowsDeviceMovementDisplay :: IsCMMotionManager cmMotionManager => cmMotionManager -> Bool -> IO ()
setShowsDeviceMovementDisplay cmMotionManager value =
  sendMessage cmMotionManager setShowsDeviceMovementDisplaySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startAccelerometerUpdates@
startAccelerometerUpdatesSelector :: Selector '[] ()
startAccelerometerUpdatesSelector = mkSelector "startAccelerometerUpdates"

-- | @Selector@ for @startAccelerometerUpdatesToQueue:withHandler:@
startAccelerometerUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startAccelerometerUpdatesToQueue_withHandlerSelector = mkSelector "startAccelerometerUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopAccelerometerUpdates@
stopAccelerometerUpdatesSelector :: Selector '[] ()
stopAccelerometerUpdatesSelector = mkSelector "stopAccelerometerUpdates"

-- | @Selector@ for @startGyroUpdates@
startGyroUpdatesSelector :: Selector '[] ()
startGyroUpdatesSelector = mkSelector "startGyroUpdates"

-- | @Selector@ for @startGyroUpdatesToQueue:withHandler:@
startGyroUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startGyroUpdatesToQueue_withHandlerSelector = mkSelector "startGyroUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopGyroUpdates@
stopGyroUpdatesSelector :: Selector '[] ()
stopGyroUpdatesSelector = mkSelector "stopGyroUpdates"

-- | @Selector@ for @startMagnetometerUpdates@
startMagnetometerUpdatesSelector :: Selector '[] ()
startMagnetometerUpdatesSelector = mkSelector "startMagnetometerUpdates"

-- | @Selector@ for @startMagnetometerUpdatesToQueue:withHandler:@
startMagnetometerUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startMagnetometerUpdatesToQueue_withHandlerSelector = mkSelector "startMagnetometerUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopMagnetometerUpdates@
stopMagnetometerUpdatesSelector :: Selector '[] ()
stopMagnetometerUpdatesSelector = mkSelector "stopMagnetometerUpdates"

-- | @Selector@ for @availableAttitudeReferenceFrames@
availableAttitudeReferenceFramesSelector :: Selector '[] CMAttitudeReferenceFrame
availableAttitudeReferenceFramesSelector = mkSelector "availableAttitudeReferenceFrames"

-- | @Selector@ for @startDeviceMotionUpdates@
startDeviceMotionUpdatesSelector :: Selector '[] ()
startDeviceMotionUpdatesSelector = mkSelector "startDeviceMotionUpdates"

-- | @Selector@ for @startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandlerSelector :: Selector '[Id NSOperationQueue, Ptr ()] ()
startDeviceMotionUpdatesToQueue_withHandlerSelector = mkSelector "startDeviceMotionUpdatesToQueue:withHandler:"

-- | @Selector@ for @startDeviceMotionUpdatesUsingReferenceFrame:@
startDeviceMotionUpdatesUsingReferenceFrameSelector :: Selector '[CMAttitudeReferenceFrame] ()
startDeviceMotionUpdatesUsingReferenceFrameSelector = mkSelector "startDeviceMotionUpdatesUsingReferenceFrame:"

-- | @Selector@ for @startDeviceMotionUpdatesUsingReferenceFrame:toQueue:withHandler:@
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandlerSelector :: Selector '[CMAttitudeReferenceFrame, Id NSOperationQueue, Ptr ()] ()
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandlerSelector = mkSelector "startDeviceMotionUpdatesUsingReferenceFrame:toQueue:withHandler:"

-- | @Selector@ for @stopDeviceMotionUpdates@
stopDeviceMotionUpdatesSelector :: Selector '[] ()
stopDeviceMotionUpdatesSelector = mkSelector "stopDeviceMotionUpdates"

-- | @Selector@ for @accelerometerUpdateInterval@
accelerometerUpdateIntervalSelector :: Selector '[] CDouble
accelerometerUpdateIntervalSelector = mkSelector "accelerometerUpdateInterval"

-- | @Selector@ for @setAccelerometerUpdateInterval:@
setAccelerometerUpdateIntervalSelector :: Selector '[CDouble] ()
setAccelerometerUpdateIntervalSelector = mkSelector "setAccelerometerUpdateInterval:"

-- | @Selector@ for @accelerometerAvailable@
accelerometerAvailableSelector :: Selector '[] Bool
accelerometerAvailableSelector = mkSelector "accelerometerAvailable"

-- | @Selector@ for @accelerometerActive@
accelerometerActiveSelector :: Selector '[] Bool
accelerometerActiveSelector = mkSelector "accelerometerActive"

-- | @Selector@ for @accelerometerData@
accelerometerDataSelector :: Selector '[] (Id CMAccelerometerData)
accelerometerDataSelector = mkSelector "accelerometerData"

-- | @Selector@ for @gyroUpdateInterval@
gyroUpdateIntervalSelector :: Selector '[] CDouble
gyroUpdateIntervalSelector = mkSelector "gyroUpdateInterval"

-- | @Selector@ for @setGyroUpdateInterval:@
setGyroUpdateIntervalSelector :: Selector '[CDouble] ()
setGyroUpdateIntervalSelector = mkSelector "setGyroUpdateInterval:"

-- | @Selector@ for @gyroAvailable@
gyroAvailableSelector :: Selector '[] Bool
gyroAvailableSelector = mkSelector "gyroAvailable"

-- | @Selector@ for @gyroActive@
gyroActiveSelector :: Selector '[] Bool
gyroActiveSelector = mkSelector "gyroActive"

-- | @Selector@ for @gyroData@
gyroDataSelector :: Selector '[] (Id CMGyroData)
gyroDataSelector = mkSelector "gyroData"

-- | @Selector@ for @magnetometerUpdateInterval@
magnetometerUpdateIntervalSelector :: Selector '[] CDouble
magnetometerUpdateIntervalSelector = mkSelector "magnetometerUpdateInterval"

-- | @Selector@ for @setMagnetometerUpdateInterval:@
setMagnetometerUpdateIntervalSelector :: Selector '[CDouble] ()
setMagnetometerUpdateIntervalSelector = mkSelector "setMagnetometerUpdateInterval:"

-- | @Selector@ for @magnetometerAvailable@
magnetometerAvailableSelector :: Selector '[] Bool
magnetometerAvailableSelector = mkSelector "magnetometerAvailable"

-- | @Selector@ for @magnetometerActive@
magnetometerActiveSelector :: Selector '[] Bool
magnetometerActiveSelector = mkSelector "magnetometerActive"

-- | @Selector@ for @magnetometerData@
magnetometerDataSelector :: Selector '[] RawId
magnetometerDataSelector = mkSelector "magnetometerData"

-- | @Selector@ for @deviceMotionUpdateInterval@
deviceMotionUpdateIntervalSelector :: Selector '[] CDouble
deviceMotionUpdateIntervalSelector = mkSelector "deviceMotionUpdateInterval"

-- | @Selector@ for @setDeviceMotionUpdateInterval:@
setDeviceMotionUpdateIntervalSelector :: Selector '[CDouble] ()
setDeviceMotionUpdateIntervalSelector = mkSelector "setDeviceMotionUpdateInterval:"

-- | @Selector@ for @attitudeReferenceFrame@
attitudeReferenceFrameSelector :: Selector '[] CMAttitudeReferenceFrame
attitudeReferenceFrameSelector = mkSelector "attitudeReferenceFrame"

-- | @Selector@ for @deviceMotionAvailable@
deviceMotionAvailableSelector :: Selector '[] Bool
deviceMotionAvailableSelector = mkSelector "deviceMotionAvailable"

-- | @Selector@ for @deviceMotionActive@
deviceMotionActiveSelector :: Selector '[] Bool
deviceMotionActiveSelector = mkSelector "deviceMotionActive"

-- | @Selector@ for @deviceMotion@
deviceMotionSelector :: Selector '[] (Id CMDeviceMotion)
deviceMotionSelector = mkSelector "deviceMotion"

-- | @Selector@ for @showsDeviceMovementDisplay@
showsDeviceMovementDisplaySelector :: Selector '[] Bool
showsDeviceMovementDisplaySelector = mkSelector "showsDeviceMovementDisplay"

-- | @Selector@ for @setShowsDeviceMovementDisplay:@
setShowsDeviceMovementDisplaySelector :: Selector '[Bool] ()
setShowsDeviceMovementDisplaySelector = mkSelector "setShowsDeviceMovementDisplay:"

