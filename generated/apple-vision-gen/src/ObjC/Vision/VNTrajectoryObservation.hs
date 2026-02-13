{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNTrajectoryObservation
--
-- VNObservation
--
-- The VNTrajectoryObservation describes a detected trajectory with the points on the trajectory and the equation describing the trajectory. The observation also reprorts the duration describing when the trajectory was first detected (which will be in the past).
--
-- Generated bindings for @VNTrajectoryObservation@.
module ObjC.Vision.VNTrajectoryObservation
  ( VNTrajectoryObservation
  , IsVNTrajectoryObservation(..)
  , detectedPoints
  , projectedPoints
  , movingAverageRadius
  , detectedPointsSelector
  , movingAverageRadiusSelector
  , projectedPointsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The centroids of the contour being detected along the trajectory.
--
-- These are the unprocessed centroid points of the detected contour that is tracked on the trajectory. The points may be slightly off the ideal trajectory as these are the measured points that fall within the allowed tolerance. The maximum number or past points is limited by the maximum trajectory length set in the request.
--
-- ObjC selector: @- detectedPoints@
detectedPoints :: IsVNTrajectoryObservation vnTrajectoryObservation => vnTrajectoryObservation -> IO (Id NSArray)
detectedPoints vnTrajectoryObservation =
  sendMessage vnTrajectoryObservation detectedPointsSelector

-- | The centroids of  the calculated trajectory from the detected points.
--
-- These are the calculated centroid points along the ideal trajectory described by the parabolic equation. The equation and the projected points of the detected trajectory get refined over time. The maximum number of cached points is limited by the maximum points needed to describe the trajectory together with the parabolic equation.
--
-- ObjC selector: @- projectedPoints@
projectedPoints :: IsVNTrajectoryObservation vnTrajectoryObservation => vnTrajectoryObservation -> IO (Id NSArray)
projectedPoints vnTrajectoryObservation =
  sendMessage vnTrajectoryObservation projectedPointsSelector

-- | The moving average radius of the object being tracked.
--
-- This is the radius of the object at each detected point (used to determine the trajectory) averaged.
--
-- ObjC selector: @- movingAverageRadius@
movingAverageRadius :: IsVNTrajectoryObservation vnTrajectoryObservation => vnTrajectoryObservation -> IO CDouble
movingAverageRadius vnTrajectoryObservation =
  sendMessage vnTrajectoryObservation movingAverageRadiusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @detectedPoints@
detectedPointsSelector :: Selector '[] (Id NSArray)
detectedPointsSelector = mkSelector "detectedPoints"

-- | @Selector@ for @projectedPoints@
projectedPointsSelector :: Selector '[] (Id NSArray)
projectedPointsSelector = mkSelector "projectedPoints"

-- | @Selector@ for @movingAverageRadius@
movingAverageRadiusSelector :: Selector '[] CDouble
movingAverageRadiusSelector = mkSelector "movingAverageRadius"

