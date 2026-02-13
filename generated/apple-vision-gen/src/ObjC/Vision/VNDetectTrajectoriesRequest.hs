{-# LANGUAGE DataKinds #-}
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
  , maximumObjectSizeSelector
  , minimumObjectSizeSelector
  , objectMaximumNormalizedRadiusSelector
  , objectMinimumNormalizedRadiusSelector
  , resultsSelector
  , setMaximumObjectSizeSelector
  , setMinimumObjectSizeSelector
  , setObjectMaximumNormalizedRadiusSelector
  , setObjectMinimumNormalizedRadiusSelector
  , trajectoryLengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The number of points required to analyze a parabola that indicates a trajectory.
--
-- ObjC selector: @- trajectoryLength@
trajectoryLength :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CLong
trajectoryLength vnDetectTrajectoriesRequest =
  sendMessage vnDetectTrajectoriesRequest trajectoryLengthSelector

-- | Specifies the minimum radius of the bounding circle of the object to be tracked. This can be used to filter out noise and small objects. The default is 0.0, which means no filtering is applied. Changing the property from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The value is specified in normalized coordinates.
--
-- ObjC selector: @- objectMinimumNormalizedRadius@
objectMinimumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
objectMinimumNormalizedRadius vnDetectTrajectoriesRequest =
  sendMessage vnDetectTrajectoriesRequest objectMinimumNormalizedRadiusSelector

-- | Specifies the minimum radius of the bounding circle of the object to be tracked. This can be used to filter out noise and small objects. The default is 0.0, which means no filtering is applied. Changing the property from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The value is specified in normalized coordinates.
--
-- ObjC selector: @- setObjectMinimumNormalizedRadius:@
setObjectMinimumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setObjectMinimumNormalizedRadius vnDetectTrajectoriesRequest value =
  sendMessage vnDetectTrajectoriesRequest setObjectMinimumNormalizedRadiusSelector value

-- | @- minimumObjectSize@
minimumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
minimumObjectSize vnDetectTrajectoriesRequest =
  sendMessage vnDetectTrajectoriesRequest minimumObjectSizeSelector

-- | @- setMinimumObjectSize:@
setMinimumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setMinimumObjectSize vnDetectTrajectoriesRequest value =
  sendMessage vnDetectTrajectoriesRequest setMinimumObjectSizeSelector value

-- | Specifies the maximum radius of the bounding circle of the object to be tracked. This can be used to filter out unwanted trajectories from larger objects moving through the scene. The default is 1.0, which means no filtering is applied. Changing the maximum from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The size is specified in normalized coordinates.
--
-- ObjC selector: @- objectMaximumNormalizedRadius@
objectMaximumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
objectMaximumNormalizedRadius vnDetectTrajectoriesRequest =
  sendMessage vnDetectTrajectoriesRequest objectMaximumNormalizedRadiusSelector

-- | Specifies the maximum radius of the bounding circle of the object to be tracked. This can be used to filter out unwanted trajectories from larger objects moving through the scene. The default is 1.0, which means no filtering is applied. Changing the maximum from frame to frame can produce eratic trajectories as objects will either disappear or be added to the tracking base on this filtering. The size is specified in normalized coordinates.
--
-- ObjC selector: @- setObjectMaximumNormalizedRadius:@
setObjectMaximumNormalizedRadius :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setObjectMaximumNormalizedRadius vnDetectTrajectoriesRequest value =
  sendMessage vnDetectTrajectoriesRequest setObjectMaximumNormalizedRadiusSelector value

-- | @- maximumObjectSize@
maximumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO CFloat
maximumObjectSize vnDetectTrajectoriesRequest =
  sendMessage vnDetectTrajectoriesRequest maximumObjectSizeSelector

-- | @- setMaximumObjectSize:@
setMaximumObjectSize :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> CFloat -> IO ()
setMaximumObjectSize vnDetectTrajectoriesRequest value =
  sendMessage vnDetectTrajectoriesRequest setMaximumObjectSizeSelector value

-- | Provides VNTrajectoryObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectTrajectoriesRequest vnDetectTrajectoriesRequest => vnDetectTrajectoriesRequest -> IO (Id NSArray)
results vnDetectTrajectoriesRequest =
  sendMessage vnDetectTrajectoriesRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trajectoryLength@
trajectoryLengthSelector :: Selector '[] CLong
trajectoryLengthSelector = mkSelector "trajectoryLength"

-- | @Selector@ for @objectMinimumNormalizedRadius@
objectMinimumNormalizedRadiusSelector :: Selector '[] CFloat
objectMinimumNormalizedRadiusSelector = mkSelector "objectMinimumNormalizedRadius"

-- | @Selector@ for @setObjectMinimumNormalizedRadius:@
setObjectMinimumNormalizedRadiusSelector :: Selector '[CFloat] ()
setObjectMinimumNormalizedRadiusSelector = mkSelector "setObjectMinimumNormalizedRadius:"

-- | @Selector@ for @minimumObjectSize@
minimumObjectSizeSelector :: Selector '[] CFloat
minimumObjectSizeSelector = mkSelector "minimumObjectSize"

-- | @Selector@ for @setMinimumObjectSize:@
setMinimumObjectSizeSelector :: Selector '[CFloat] ()
setMinimumObjectSizeSelector = mkSelector "setMinimumObjectSize:"

-- | @Selector@ for @objectMaximumNormalizedRadius@
objectMaximumNormalizedRadiusSelector :: Selector '[] CFloat
objectMaximumNormalizedRadiusSelector = mkSelector "objectMaximumNormalizedRadius"

-- | @Selector@ for @setObjectMaximumNormalizedRadius:@
setObjectMaximumNormalizedRadiusSelector :: Selector '[CFloat] ()
setObjectMaximumNormalizedRadiusSelector = mkSelector "setObjectMaximumNormalizedRadius:"

-- | @Selector@ for @maximumObjectSize@
maximumObjectSizeSelector :: Selector '[] CFloat
maximumObjectSizeSelector = mkSelector "maximumObjectSize"

-- | @Selector@ for @setMaximumObjectSize:@
setMaximumObjectSizeSelector :: Selector '[CFloat] ()
setMaximumObjectSizeSelector = mkSelector "setMaximumObjectSize:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

