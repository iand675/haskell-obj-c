{-# LANGUAGE PatternSynonyms #-}
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
  , startAccelerometerUpdatesSelector
  , startAccelerometerUpdatesToQueue_withHandlerSelector
  , stopAccelerometerUpdatesSelector
  , startGyroUpdatesSelector
  , startGyroUpdatesToQueue_withHandlerSelector
  , stopGyroUpdatesSelector
  , startMagnetometerUpdatesSelector
  , startMagnetometerUpdatesToQueue_withHandlerSelector
  , stopMagnetometerUpdatesSelector
  , availableAttitudeReferenceFramesSelector
  , startDeviceMotionUpdatesSelector
  , startDeviceMotionUpdatesToQueue_withHandlerSelector
  , startDeviceMotionUpdatesUsingReferenceFrameSelector
  , startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandlerSelector
  , stopDeviceMotionUpdatesSelector
  , accelerometerUpdateIntervalSelector
  , setAccelerometerUpdateIntervalSelector
  , accelerometerAvailableSelector
  , accelerometerActiveSelector
  , accelerometerDataSelector
  , gyroUpdateIntervalSelector
  , setGyroUpdateIntervalSelector
  , gyroAvailableSelector
  , gyroActiveSelector
  , gyroDataSelector
  , magnetometerUpdateIntervalSelector
  , setMagnetometerUpdateIntervalSelector
  , magnetometerAvailableSelector
  , magnetometerActiveSelector
  , magnetometerDataSelector
  , deviceMotionUpdateIntervalSelector
  , setDeviceMotionUpdateIntervalSelector
  , attitudeReferenceFrameSelector
  , deviceMotionAvailableSelector
  , deviceMotionActiveSelector
  , deviceMotionSelector
  , showsDeviceMovementDisplaySelector
  , setShowsDeviceMovementDisplaySelector

  -- * Enum types
  , CMAttitudeReferenceFrame(CMAttitudeReferenceFrame)
  , pattern CMAttitudeReferenceFrameXArbitraryZVertical
  , pattern CMAttitudeReferenceFrameXArbitraryCorrectedZVertical
  , pattern CMAttitudeReferenceFrameXMagneticNorthZVertical
  , pattern CMAttitudeReferenceFrameXTrueNorthZVertical

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
startAccelerometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startAccelerometerUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "startAccelerometerUpdates") retVoid []

-- | @- startAccelerometerUpdatesToQueue:withHandler:@
startAccelerometerUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startAccelerometerUpdatesToQueue_withHandler cmMotionManager  queue handler =
  withObjCPtr queue $ \raw_queue ->
      sendMsg cmMotionManager (mkSelector "startAccelerometerUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopAccelerometerUpdates@
stopAccelerometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopAccelerometerUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "stopAccelerometerUpdates") retVoid []

-- | @- startGyroUpdates@
startGyroUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startGyroUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "startGyroUpdates") retVoid []

-- | @- startGyroUpdatesToQueue:withHandler:@
startGyroUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startGyroUpdatesToQueue_withHandler cmMotionManager  queue handler =
  withObjCPtr queue $ \raw_queue ->
      sendMsg cmMotionManager (mkSelector "startGyroUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopGyroUpdates@
stopGyroUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopGyroUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "stopGyroUpdates") retVoid []

-- | @- startMagnetometerUpdates@
startMagnetometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startMagnetometerUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "startMagnetometerUpdates") retVoid []

-- | @- startMagnetometerUpdatesToQueue:withHandler:@
startMagnetometerUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startMagnetometerUpdatesToQueue_withHandler cmMotionManager  queue handler =
  withObjCPtr queue $ \raw_queue ->
      sendMsg cmMotionManager (mkSelector "startMagnetometerUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopMagnetometerUpdates@
stopMagnetometerUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopMagnetometerUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "stopMagnetometerUpdates") retVoid []

-- | @+ availableAttitudeReferenceFrames@
availableAttitudeReferenceFrames :: IO CMAttitudeReferenceFrame
availableAttitudeReferenceFrames  =
  do
    cls' <- getRequiredClass "CMMotionManager"
    fmap (coerce :: CULong -> CMAttitudeReferenceFrame) $ sendClassMsg cls' (mkSelector "availableAttitudeReferenceFrames") retCULong []

-- | @- startDeviceMotionUpdates@
startDeviceMotionUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
startDeviceMotionUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "startDeviceMotionUpdates") retVoid []

-- | @- startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> queue -> Ptr () -> IO ()
startDeviceMotionUpdatesToQueue_withHandler cmMotionManager  queue handler =
  withObjCPtr queue $ \raw_queue ->
      sendMsg cmMotionManager (mkSelector "startDeviceMotionUpdatesToQueue:withHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- startDeviceMotionUpdatesUsingReferenceFrame:@
startDeviceMotionUpdatesUsingReferenceFrame :: IsCMMotionManager cmMotionManager => cmMotionManager -> CMAttitudeReferenceFrame -> IO ()
startDeviceMotionUpdatesUsingReferenceFrame cmMotionManager  referenceFrame =
    sendMsg cmMotionManager (mkSelector "startDeviceMotionUpdatesUsingReferenceFrame:") retVoid [argCULong (coerce referenceFrame)]

-- | @- startDeviceMotionUpdatesUsingReferenceFrame:toQueue:withHandler:@
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandler :: (IsCMMotionManager cmMotionManager, IsNSOperationQueue queue) => cmMotionManager -> CMAttitudeReferenceFrame -> queue -> Ptr () -> IO ()
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandler cmMotionManager  referenceFrame queue handler =
  withObjCPtr queue $ \raw_queue ->
      sendMsg cmMotionManager (mkSelector "startDeviceMotionUpdatesUsingReferenceFrame:toQueue:withHandler:") retVoid [argCULong (coerce referenceFrame), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- stopDeviceMotionUpdates@
stopDeviceMotionUpdates :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO ()
stopDeviceMotionUpdates cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "stopDeviceMotionUpdates") retVoid []

-- | @- accelerometerUpdateInterval@
accelerometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
accelerometerUpdateInterval cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "accelerometerUpdateInterval") retCDouble []

-- | @- setAccelerometerUpdateInterval:@
setAccelerometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setAccelerometerUpdateInterval cmMotionManager  value =
    sendMsg cmMotionManager (mkSelector "setAccelerometerUpdateInterval:") retVoid [argCDouble value]

-- | @- accelerometerAvailable@
accelerometerAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
accelerometerAvailable cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "accelerometerAvailable") retCULong []

-- | @- accelerometerActive@
accelerometerActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
accelerometerActive cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "accelerometerActive") retCULong []

-- | @- accelerometerData@
accelerometerData :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO (Id CMAccelerometerData)
accelerometerData cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "accelerometerData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- gyroUpdateInterval@
gyroUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
gyroUpdateInterval cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "gyroUpdateInterval") retCDouble []

-- | @- setGyroUpdateInterval:@
setGyroUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setGyroUpdateInterval cmMotionManager  value =
    sendMsg cmMotionManager (mkSelector "setGyroUpdateInterval:") retVoid [argCDouble value]

-- | @- gyroAvailable@
gyroAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
gyroAvailable cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "gyroAvailable") retCULong []

-- | @- gyroActive@
gyroActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
gyroActive cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "gyroActive") retCULong []

-- | @- gyroData@
gyroData :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO (Id CMGyroData)
gyroData cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "gyroData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- magnetometerUpdateInterval@
magnetometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
magnetometerUpdateInterval cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "magnetometerUpdateInterval") retCDouble []

-- | @- setMagnetometerUpdateInterval:@
setMagnetometerUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setMagnetometerUpdateInterval cmMotionManager  value =
    sendMsg cmMotionManager (mkSelector "setMagnetometerUpdateInterval:") retVoid [argCDouble value]

-- | @- magnetometerAvailable@
magnetometerAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
magnetometerAvailable cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "magnetometerAvailable") retCULong []

-- | @- magnetometerActive@
magnetometerActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
magnetometerActive cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "magnetometerActive") retCULong []

-- | @- magnetometerData@
magnetometerData :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO RawId
magnetometerData cmMotionManager  =
    fmap (RawId . castPtr) $ sendMsg cmMotionManager (mkSelector "magnetometerData") (retPtr retVoid) []

-- | @- deviceMotionUpdateInterval@
deviceMotionUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CDouble
deviceMotionUpdateInterval cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "deviceMotionUpdateInterval") retCDouble []

-- | @- setDeviceMotionUpdateInterval:@
setDeviceMotionUpdateInterval :: IsCMMotionManager cmMotionManager => cmMotionManager -> CDouble -> IO ()
setDeviceMotionUpdateInterval cmMotionManager  value =
    sendMsg cmMotionManager (mkSelector "setDeviceMotionUpdateInterval:") retVoid [argCDouble value]

-- | @- attitudeReferenceFrame@
attitudeReferenceFrame :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO CMAttitudeReferenceFrame
attitudeReferenceFrame cmMotionManager  =
    fmap (coerce :: CULong -> CMAttitudeReferenceFrame) $ sendMsg cmMotionManager (mkSelector "attitudeReferenceFrame") retCULong []

-- | @- deviceMotionAvailable@
deviceMotionAvailable :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
deviceMotionAvailable cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "deviceMotionAvailable") retCULong []

-- | @- deviceMotionActive@
deviceMotionActive :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
deviceMotionActive cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "deviceMotionActive") retCULong []

-- | @- deviceMotion@
deviceMotion :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO (Id CMDeviceMotion)
deviceMotion cmMotionManager  =
    sendMsg cmMotionManager (mkSelector "deviceMotion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- showsDeviceMovementDisplay@
showsDeviceMovementDisplay :: IsCMMotionManager cmMotionManager => cmMotionManager -> IO Bool
showsDeviceMovementDisplay cmMotionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmMotionManager (mkSelector "showsDeviceMovementDisplay") retCULong []

-- | @- setShowsDeviceMovementDisplay:@
setShowsDeviceMovementDisplay :: IsCMMotionManager cmMotionManager => cmMotionManager -> Bool -> IO ()
setShowsDeviceMovementDisplay cmMotionManager  value =
    sendMsg cmMotionManager (mkSelector "setShowsDeviceMovementDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startAccelerometerUpdates@
startAccelerometerUpdatesSelector :: Selector
startAccelerometerUpdatesSelector = mkSelector "startAccelerometerUpdates"

-- | @Selector@ for @startAccelerometerUpdatesToQueue:withHandler:@
startAccelerometerUpdatesToQueue_withHandlerSelector :: Selector
startAccelerometerUpdatesToQueue_withHandlerSelector = mkSelector "startAccelerometerUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopAccelerometerUpdates@
stopAccelerometerUpdatesSelector :: Selector
stopAccelerometerUpdatesSelector = mkSelector "stopAccelerometerUpdates"

-- | @Selector@ for @startGyroUpdates@
startGyroUpdatesSelector :: Selector
startGyroUpdatesSelector = mkSelector "startGyroUpdates"

-- | @Selector@ for @startGyroUpdatesToQueue:withHandler:@
startGyroUpdatesToQueue_withHandlerSelector :: Selector
startGyroUpdatesToQueue_withHandlerSelector = mkSelector "startGyroUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopGyroUpdates@
stopGyroUpdatesSelector :: Selector
stopGyroUpdatesSelector = mkSelector "stopGyroUpdates"

-- | @Selector@ for @startMagnetometerUpdates@
startMagnetometerUpdatesSelector :: Selector
startMagnetometerUpdatesSelector = mkSelector "startMagnetometerUpdates"

-- | @Selector@ for @startMagnetometerUpdatesToQueue:withHandler:@
startMagnetometerUpdatesToQueue_withHandlerSelector :: Selector
startMagnetometerUpdatesToQueue_withHandlerSelector = mkSelector "startMagnetometerUpdatesToQueue:withHandler:"

-- | @Selector@ for @stopMagnetometerUpdates@
stopMagnetometerUpdatesSelector :: Selector
stopMagnetometerUpdatesSelector = mkSelector "stopMagnetometerUpdates"

-- | @Selector@ for @availableAttitudeReferenceFrames@
availableAttitudeReferenceFramesSelector :: Selector
availableAttitudeReferenceFramesSelector = mkSelector "availableAttitudeReferenceFrames"

-- | @Selector@ for @startDeviceMotionUpdates@
startDeviceMotionUpdatesSelector :: Selector
startDeviceMotionUpdatesSelector = mkSelector "startDeviceMotionUpdates"

-- | @Selector@ for @startDeviceMotionUpdatesToQueue:withHandler:@
startDeviceMotionUpdatesToQueue_withHandlerSelector :: Selector
startDeviceMotionUpdatesToQueue_withHandlerSelector = mkSelector "startDeviceMotionUpdatesToQueue:withHandler:"

-- | @Selector@ for @startDeviceMotionUpdatesUsingReferenceFrame:@
startDeviceMotionUpdatesUsingReferenceFrameSelector :: Selector
startDeviceMotionUpdatesUsingReferenceFrameSelector = mkSelector "startDeviceMotionUpdatesUsingReferenceFrame:"

-- | @Selector@ for @startDeviceMotionUpdatesUsingReferenceFrame:toQueue:withHandler:@
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandlerSelector :: Selector
startDeviceMotionUpdatesUsingReferenceFrame_toQueue_withHandlerSelector = mkSelector "startDeviceMotionUpdatesUsingReferenceFrame:toQueue:withHandler:"

-- | @Selector@ for @stopDeviceMotionUpdates@
stopDeviceMotionUpdatesSelector :: Selector
stopDeviceMotionUpdatesSelector = mkSelector "stopDeviceMotionUpdates"

-- | @Selector@ for @accelerometerUpdateInterval@
accelerometerUpdateIntervalSelector :: Selector
accelerometerUpdateIntervalSelector = mkSelector "accelerometerUpdateInterval"

-- | @Selector@ for @setAccelerometerUpdateInterval:@
setAccelerometerUpdateIntervalSelector :: Selector
setAccelerometerUpdateIntervalSelector = mkSelector "setAccelerometerUpdateInterval:"

-- | @Selector@ for @accelerometerAvailable@
accelerometerAvailableSelector :: Selector
accelerometerAvailableSelector = mkSelector "accelerometerAvailable"

-- | @Selector@ for @accelerometerActive@
accelerometerActiveSelector :: Selector
accelerometerActiveSelector = mkSelector "accelerometerActive"

-- | @Selector@ for @accelerometerData@
accelerometerDataSelector :: Selector
accelerometerDataSelector = mkSelector "accelerometerData"

-- | @Selector@ for @gyroUpdateInterval@
gyroUpdateIntervalSelector :: Selector
gyroUpdateIntervalSelector = mkSelector "gyroUpdateInterval"

-- | @Selector@ for @setGyroUpdateInterval:@
setGyroUpdateIntervalSelector :: Selector
setGyroUpdateIntervalSelector = mkSelector "setGyroUpdateInterval:"

-- | @Selector@ for @gyroAvailable@
gyroAvailableSelector :: Selector
gyroAvailableSelector = mkSelector "gyroAvailable"

-- | @Selector@ for @gyroActive@
gyroActiveSelector :: Selector
gyroActiveSelector = mkSelector "gyroActive"

-- | @Selector@ for @gyroData@
gyroDataSelector :: Selector
gyroDataSelector = mkSelector "gyroData"

-- | @Selector@ for @magnetometerUpdateInterval@
magnetometerUpdateIntervalSelector :: Selector
magnetometerUpdateIntervalSelector = mkSelector "magnetometerUpdateInterval"

-- | @Selector@ for @setMagnetometerUpdateInterval:@
setMagnetometerUpdateIntervalSelector :: Selector
setMagnetometerUpdateIntervalSelector = mkSelector "setMagnetometerUpdateInterval:"

-- | @Selector@ for @magnetometerAvailable@
magnetometerAvailableSelector :: Selector
magnetometerAvailableSelector = mkSelector "magnetometerAvailable"

-- | @Selector@ for @magnetometerActive@
magnetometerActiveSelector :: Selector
magnetometerActiveSelector = mkSelector "magnetometerActive"

-- | @Selector@ for @magnetometerData@
magnetometerDataSelector :: Selector
magnetometerDataSelector = mkSelector "magnetometerData"

-- | @Selector@ for @deviceMotionUpdateInterval@
deviceMotionUpdateIntervalSelector :: Selector
deviceMotionUpdateIntervalSelector = mkSelector "deviceMotionUpdateInterval"

-- | @Selector@ for @setDeviceMotionUpdateInterval:@
setDeviceMotionUpdateIntervalSelector :: Selector
setDeviceMotionUpdateIntervalSelector = mkSelector "setDeviceMotionUpdateInterval:"

-- | @Selector@ for @attitudeReferenceFrame@
attitudeReferenceFrameSelector :: Selector
attitudeReferenceFrameSelector = mkSelector "attitudeReferenceFrame"

-- | @Selector@ for @deviceMotionAvailable@
deviceMotionAvailableSelector :: Selector
deviceMotionAvailableSelector = mkSelector "deviceMotionAvailable"

-- | @Selector@ for @deviceMotionActive@
deviceMotionActiveSelector :: Selector
deviceMotionActiveSelector = mkSelector "deviceMotionActive"

-- | @Selector@ for @deviceMotion@
deviceMotionSelector :: Selector
deviceMotionSelector = mkSelector "deviceMotion"

-- | @Selector@ for @showsDeviceMovementDisplay@
showsDeviceMovementDisplaySelector :: Selector
showsDeviceMovementDisplaySelector = mkSelector "showsDeviceMovementDisplay"

-- | @Selector@ for @setShowsDeviceMovementDisplay:@
setShowsDeviceMovementDisplaySelector :: Selector
setShowsDeviceMovementDisplaySelector = mkSelector "setShowsDeviceMovementDisplay:"

