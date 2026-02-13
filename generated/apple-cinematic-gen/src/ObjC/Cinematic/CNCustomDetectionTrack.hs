{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A discrete detection track composed of individual detections.
--
-- Generated bindings for @CNCustomDetectionTrack@.
module ObjC.Cinematic.CNCustomDetectionTrack
  ( CNCustomDetectionTrack
  , IsCNCustomDetectionTrack(..)
  , initWithDetections_smooth
  , allDetections
  , allDetectionsSelector
  , initWithDetections_smoothSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a custom detection track with an array of detections, optionally applying smoothing.
--
-- The smoothing algorithm used is the same one that is used for built-in detections during recording. It compensates for some amount of jitter in the disparity measure by smoothing out variability.
--
-- ObjC selector: @- initWithDetections:smooth:@
initWithDetections_smooth :: (IsCNCustomDetectionTrack cnCustomDetectionTrack, IsNSArray detections) => cnCustomDetectionTrack -> detections -> Bool -> IO (Id CNCustomDetectionTrack)
initWithDetections_smooth cnCustomDetectionTrack detections applySmoothing =
  sendOwnedMessage cnCustomDetectionTrack initWithDetections_smoothSelector (toNSArray detections) applySmoothing

-- | @- allDetections@
allDetections :: IsCNCustomDetectionTrack cnCustomDetectionTrack => cnCustomDetectionTrack -> IO (Id NSArray)
allDetections cnCustomDetectionTrack =
  sendMessage cnCustomDetectionTrack allDetectionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDetections:smooth:@
initWithDetections_smoothSelector :: Selector '[Id NSArray, Bool] (Id CNCustomDetectionTrack)
initWithDetections_smoothSelector = mkSelector "initWithDetections:smooth:"

-- | @Selector@ for @allDetections@
allDetectionsSelector :: Selector '[] (Id NSArray)
allDetectionsSelector = mkSelector "allDetections"

