{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that detects trajectories of shapes (even small ones) that follow a parabolic path in a sequence of images.
--
-- This request detects objects moving and (once their path follows the constraint of a parabola), a VNTrajectoryObservation will be returned with the detected points and the equation describing the parabola.
--
-- Generated bindings for @VNDetectTrajectoriesRequest@.
module ObjC.Vision.VNDetectTrajectoriesRequest
  ( VNDetectTrajectoriesRequest
  , IsVNDetectTrajectoriesRequest(..)
  , trajectoryLength
  , objectMinimumNormalizedRadius
  , setObjectMinimumNormalizedRadius
  , minimumObjectSize
  , setMinimumObjectSize
  , objectMaximumNormalizedRadius
  , setObjectMaximumNormalizedRadius
  , maximumObjectSize
  , setMaximumObjectSize
  , results
  , trajectoryLengthSelector
  , objectMinimumNormalizedRadiusSelector
  , setObjectMinimumNormalizedRadiusSelector
  , minimumObjectSizeSelector
  , setMinimumObjectSizeSelector
  , objectMaximumNormalizedRadiusSelector
  , setObjectMaximumNormalizedRadiusSelector
  , maximumObjectSizeSelector
  , setMaximumObjectSizeSelector
  , resultsSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The number of points required to analyze a parabola that indicates a trajectory.
--
-- ObjC selector: @- trajectoryLength@
trajectoryLength :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CLong
trajectoryLength vnDetectTrajectoriesRequest  =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "trajectoryLength") retCLong []

-- | Specifies the minimum radius of the bounding circle of the object to be tracked. This can be used to filter out noise and small objects. The default is 0.0, which means no filtering is applied. Changing the property from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The value is specified in normalized coordinates.
--
-- ObjC selector: @- objectMinimumNormalizedRadius@
objectMinimumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
objectMinimumNormalizedRadius vnDetectTrajectoriesRequest  =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "objectMinimumNormalizedRadius") retCFloat []

-- | Specifies the minimum radius of the bounding circle of the object to be tracked. This can be used to filter out noise and small objects. The default is 0.0, which means no filtering is applied. Changing the property from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The value is specified in normalized coordinates.
--
-- ObjC selector: @- setObjectMinimumNormalizedRadius:@
setObjectMinimumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setObjectMinimumNormalizedRadius vnDetectTrajectoriesRequest  value =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "setObjectMinimumNormalizedRadius:") retVoid [argCFloat (fromIntegral value)]

-- | @- minimumObjectSize@
minimumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
minimumObjectSize vnDetectTrajectoriesRequest  =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "minimumObjectSize") retCFloat []

-- | @- setMinimumObjectSize:@
setMinimumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setMinimumObjectSize vnDetectTrajectoriesRequest  value =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "setMinimumObjectSize:") retVoid [argCFloat (fromIntegral value)]

-- | Specifies the maximum radius of the bounding circle of the object to be tracked. This can be used to filter out unwanted trajectories from larger objects moving through the scene. The default is 1.0, which means no filtering is applied. Changing the maximum from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The size is specified in normalized coordinates.
--
-- ObjC selector: @- objectMaximumNormalizedRadius@
objectMaximumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
objectMaximumNormalizedRadius vnDetectTrajectoriesRequest  =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "objectMaximumNormalizedRadius") retCFloat []

-- | Specifies the maximum radius of the bounding circle of the object to be tracked. This can be used to filter out unwanted trajectories from larger objects moving through the scene. The default is 1.0, which means no filtering is applied. Changing the maximum from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The size is specified in normalized coordinates.
--
-- ObjC selector: @- setObjectMaximumNormalizedRadius:@
setObjectMaximumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setObjectMaximumNormalizedRadius vnDetectTrajectoriesRequest  value =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "setObjectMaximumNormalizedRadius:") retVoid [argCFloat (fromIntegral value)]

-- | @- maximumObjectSize@
maximumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
maximumObjectSize vnDetectTrajectoriesRequest  =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "maximumObjectSize") retCFloat []

-- | @- setMaximumObjectSize:@
setMaximumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setMaximumObjectSize vnDetectTrajectoriesRequest  value =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "setMaximumObjectSize:") retVoid [argCFloat (fromIntegral value)]

-- | Provides VNTrajectoryObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO (Id NSArray)
results vnDetectTrajectoriesRequest  =
  sendMsg vnDetectTrajectoriesRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trajectoryLength@
trajectoryLengthSelector :: Selector
trajectoryLengthSelector = mkSelector "trajectoryLength"

-- | @Selector@ for @objectMinimumNormalizedRadius@
objectMinimumNormalizedRadiusSelector :: Selector
objectMinimumNormalizedRadiusSelector = mkSelector "objectMinimumNormalizedRadius"

-- | @Selector@ for @setObjectMinimumNormalizedRadius:@
setObjectMinimumNormalizedRadiusSelector :: Selector
setObjectMinimumNormalizedRadiusSelector = mkSelector "setObjectMinimumNormalizedRadius:"

-- | @Selector@ for @minimumObjectSize@
minimumObjectSizeSelector :: Selector
minimumObjectSizeSelector = mkSelector "minimumObjectSize"

-- | @Selector@ for @setMinimumObjectSize:@
setMinimumObjectSizeSelector :: Selector
setMinimumObjectSizeSelector = mkSelector "setMinimumObjectSize:"

-- | @Selector@ for @objectMaximumNormalizedRadius@
objectMaximumNormalizedRadiusSelector :: Selector
objectMaximumNormalizedRadiusSelector = mkSelector "objectMaximumNormalizedRadius"

-- | @Selector@ for @setObjectMaximumNormalizedRadius:@
setObjectMaximumNormalizedRadiusSelector :: Selector
setObjectMaximumNormalizedRadiusSelector = mkSelector "setObjectMaximumNormalizedRadius:"

-- | @Selector@ for @maximumObjectSize@
maximumObjectSizeSelector :: Selector
maximumObjectSizeSelector = mkSelector "maximumObjectSize"

-- | @Selector@ for @setMaximumObjectSize:@
setMaximumObjectSizeSelector :: Selector
setMaximumObjectSizeSelector = mkSelector "setMaximumObjectSize:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

