{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A continuous detection track representing focus at a fixed disparity.
--
-- Generated bindings for @CNFixedDetectionTrack@.
module ObjC.Cinematic.CNFixedDetectionTrack
  ( CNFixedDetectionTrack
  , IsCNFixedDetectionTrack(..)
  , initWithFocusDisparity
  , initWithOriginalDetection
  , focusDisparity
  , originalDetection
  , focusDisparitySelector
  , initWithFocusDisparitySelector
  , initWithOriginalDetectionSelector
  , originalDetectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a detection track with fixed focus at the given disparity.
--
-- ObjC selector: @- initWithFocusDisparity:@
initWithFocusDisparity :: IsCNFixedDetectionTrack cnFixedDetectionTrack => cnFixedDetectionTrack -> CFloat -> IO (Id CNFixedDetectionTrack)
initWithFocusDisparity cnFixedDetectionTrack focusDisparity =
  sendOwnedMessage cnFixedDetectionTrack initWithFocusDisparitySelector focusDisparity

-- | Create a detection track with fixed focus at the disparity of an existing detection.
--
-- ObjC selector: @- initWithOriginalDetection:@
initWithOriginalDetection :: (IsCNFixedDetectionTrack cnFixedDetectionTrack, IsCNDetection originalDetection) => cnFixedDetectionTrack -> originalDetection -> IO (Id CNFixedDetectionTrack)
initWithOriginalDetection cnFixedDetectionTrack originalDetection =
  sendOwnedMessage cnFixedDetectionTrack initWithOriginalDetectionSelector (toCNDetection originalDetection)

-- | @- focusDisparity@
focusDisparity :: IsCNFixedDetectionTrack cnFixedDetectionTrack => cnFixedDetectionTrack -> IO CFloat
focusDisparity cnFixedDetectionTrack =
  sendMessage cnFixedDetectionTrack focusDisparitySelector

-- | The original detection upon which this fixed detection track was based, if any.
--
-- This is the way to determine the time and rect from which fixed focus originated, if any. This detection is not part of the detection track and has a different detectionID or none.
--
-- - Important: To get a detection from the fixed detection track, use detectionAtOrBeforeTime: instead, which will return a properly time-stamped detection.
--
-- ObjC selector: @- originalDetection@
originalDetection :: IsCNFixedDetectionTrack cnFixedDetectionTrack => cnFixedDetectionTrack -> IO (Id CNDetection)
originalDetection cnFixedDetectionTrack =
  sendMessage cnFixedDetectionTrack originalDetectionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFocusDisparity:@
initWithFocusDisparitySelector :: Selector '[CFloat] (Id CNFixedDetectionTrack)
initWithFocusDisparitySelector = mkSelector "initWithFocusDisparity:"

-- | @Selector@ for @initWithOriginalDetection:@
initWithOriginalDetectionSelector :: Selector '[Id CNDetection] (Id CNFixedDetectionTrack)
initWithOriginalDetectionSelector = mkSelector "initWithOriginalDetection:"

-- | @Selector@ for @focusDisparity@
focusDisparitySelector :: Selector '[] CFloat
focusDisparitySelector = mkSelector "focusDisparity"

-- | @Selector@ for @originalDetection@
originalDetectionSelector :: Selector '[] (Id CNDetection)
originalDetectionSelector = mkSelector "originalDetection"

