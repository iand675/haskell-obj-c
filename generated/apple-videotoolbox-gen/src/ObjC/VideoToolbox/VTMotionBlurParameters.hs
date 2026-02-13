{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains both input and output parameters that the motion blur processor needs to run on a frame.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class. The output parameter for this class is @destinationFrame@ where the processor returns the output frame (as @VTFrameProcessorFrame@) back to you once the @processWithParameters@ completes.
--
-- @VTMotionBlurParameters@ are frame-level parameters.
--
-- Generated bindings for @VTMotionBlurParameters@.
module ObjC.VideoToolbox.VTMotionBlurParameters
  ( VTMotionBlurParameters
  , IsVTMotionBlurParameters(..)
  , initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrame
  , init_
  , new
  , sourceFrame
  , nextFrame
  , previousFrame
  , nextOpticalFlow
  , previousOpticalFlow
  , motionBlurStrength
  , submissionMode
  , destinationFrame
  , destinationFrameSelector
  , initSelector
  , initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrameSelector
  , motionBlurStrengthSelector
  , newSelector
  , nextFrameSelector
  , nextOpticalFlowSelector
  , previousFrameSelector
  , previousOpticalFlowSelector
  , sourceFrameSelector
  , submissionModeSelector

  -- * Enum types
  , VTMotionBlurParametersSubmissionMode(VTMotionBlurParametersSubmissionMode)
  , pattern VTMotionBlurParametersSubmissionModeRandom
  , pattern VTMotionBlurParametersSubmissionModeSequential

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoToolbox.Internal.Classes
import ObjC.VideoToolbox.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a new motion blur parameters object.
--
-- Returns @nil@ if @sourceFrame@ or @destinationFrame@ is @nil@, @sourceFrame@ and reference frames are different pixel formats, or @motionBlurStrength@ is out of range.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - nextFrame: Next source frame in presentation time order; for the last frame you can set this to @nil@.   - previousFrame: Previous source frame in presentation time order; for the first frame you can set this to @nil@.   - nextOpticalFlow: Optional @VTFrameProcessorOpticalFlow@ object that contains forward and backward optical flow   with @nextFrame@. You only need this object if optical flow is pre-computed. For the last frame this is always @nil@.   - previousOpticalFlow: Optional VTFrameProcessorOpticalFlow object that contains forward and backward optical flow   with @previousFrame@. You only need to use this if the optical flow is pre-computed. For the first frame this is always @nil@.   - motionBlurStrength: Number that indicates the strength of blur applied by the processor. Range is from 1 to 100. Default value is 50.   - submissionMode: Provides a hint to let the processor know whether you are submitting frames in presenatation   sequence. For more information about supported modes see ``VTMotionBlurParametersSubmissionMode``.   - destinationFrame: User-allocated pixel buffer that receives a frame with motion blur applied by the processor.
--
-- ObjC selector: @- initWithSourceFrame:nextFrame:previousFrame:nextOpticalFlow:previousOpticalFlow:motionBlurStrength:submissionMode:destinationFrame:@
initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrame :: (IsVTMotionBlurParameters vtMotionBlurParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame nextFrame, IsVTFrameProcessorFrame previousFrame, IsVTFrameProcessorOpticalFlow nextOpticalFlow, IsVTFrameProcessorOpticalFlow previousOpticalFlow, IsVTFrameProcessorFrame destinationFrame) => vtMotionBlurParameters -> sourceFrame -> nextFrame -> previousFrame -> nextOpticalFlow -> previousOpticalFlow -> CLong -> VTMotionBlurParametersSubmissionMode -> destinationFrame -> IO (Id VTMotionBlurParameters)
initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrame vtMotionBlurParameters sourceFrame nextFrame previousFrame nextOpticalFlow previousOpticalFlow motionBlurStrength submissionMode destinationFrame =
  sendOwnedMessage vtMotionBlurParameters initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrameSelector (toVTFrameProcessorFrame sourceFrame) (toVTFrameProcessorFrame nextFrame) (toVTFrameProcessorFrame previousFrame) (toVTFrameProcessorOpticalFlow nextOpticalFlow) (toVTFrameProcessorOpticalFlow previousOpticalFlow) motionBlurStrength submissionMode (toVTFrameProcessorFrame destinationFrame)

-- | @- init@
init_ :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTMotionBlurParameters)
init_ vtMotionBlurParameters =
  sendOwnedMessage vtMotionBlurParameters initSelector

-- | @+ new@
new :: IO (Id VTMotionBlurParameters)
new  =
  do
    cls' <- getRequiredClass "VTMotionBlurParameters"
    sendOwnedClassMessage cls' newSelector

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters sourceFrameSelector

-- | The next source frame in presentation time order, which is @nil@ for the last frame.
--
-- ObjC selector: @- nextFrame@
nextFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
nextFrame vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters nextFrameSelector

-- | Previous source frame in presentation time order, which is @nil@ for the first frame.
--
-- ObjC selector: @- previousFrame@
previousFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
previousFrame vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters previousFrameSelector

-- | Optional frame processor optical flow object that contains forward and backward optical flow with next frame.
--
-- You only need to use this object if the optical flow is pre-computed. For the last frame this is @nil@.
--
-- ObjC selector: @- nextOpticalFlow@
nextOpticalFlow :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorOpticalFlow)
nextOpticalFlow vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters nextOpticalFlowSelector

-- | Optional frame processor optical flow object that contains forward and backward optical flow with previous frame.
--
-- You only need to use this object if the optical flow is pre-computed. For the first frame this is @nil@.
--
-- ObjC selector: @- previousOpticalFlow@
previousOpticalFlow :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorOpticalFlow)
previousOpticalFlow vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters previousOpticalFlowSelector

-- | Number that indicates the strength of motion blur.
--
-- The range is from 1 to 100; the default value is 50.
--
-- ObjC selector: @- motionBlurStrength@
motionBlurStrength :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO CLong
motionBlurStrength vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters motionBlurStrengthSelector

-- | Ordering of the input frames this submission related to the previous submission.
--
-- ObjC selector: @- submissionMode@
submissionMode :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO VTMotionBlurParametersSubmissionMode
submissionMode vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters submissionModeSelector

-- | Destination frame that contains user-allocated pixel buffer that receive a frame with motion blur applied by the processor.
--
-- ObjC selector: @- destinationFrame@
destinationFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
destinationFrame vtMotionBlurParameters =
  sendMessage vtMotionBlurParameters destinationFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:nextFrame:previousFrame:nextOpticalFlow:previousOpticalFlow:motionBlurStrength:submissionMode:destinationFrame:@
initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrameSelector :: Selector '[Id VTFrameProcessorFrame, Id VTFrameProcessorFrame, Id VTFrameProcessorFrame, Id VTFrameProcessorOpticalFlow, Id VTFrameProcessorOpticalFlow, CLong, VTMotionBlurParametersSubmissionMode, Id VTFrameProcessorFrame] (Id VTMotionBlurParameters)
initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrameSelector = mkSelector "initWithSourceFrame:nextFrame:previousFrame:nextOpticalFlow:previousOpticalFlow:motionBlurStrength:submissionMode:destinationFrame:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTMotionBlurParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTMotionBlurParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @nextFrame@
nextFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
nextFrameSelector = mkSelector "nextFrame"

-- | @Selector@ for @previousFrame@
previousFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
previousFrameSelector = mkSelector "previousFrame"

-- | @Selector@ for @nextOpticalFlow@
nextOpticalFlowSelector :: Selector '[] (Id VTFrameProcessorOpticalFlow)
nextOpticalFlowSelector = mkSelector "nextOpticalFlow"

-- | @Selector@ for @previousOpticalFlow@
previousOpticalFlowSelector :: Selector '[] (Id VTFrameProcessorOpticalFlow)
previousOpticalFlowSelector = mkSelector "previousOpticalFlow"

-- | @Selector@ for @motionBlurStrength@
motionBlurStrengthSelector :: Selector '[] CLong
motionBlurStrengthSelector = mkSelector "motionBlurStrength"

-- | @Selector@ for @submissionMode@
submissionModeSelector :: Selector '[] VTMotionBlurParametersSubmissionMode
submissionModeSelector = mkSelector "submissionMode"

-- | @Selector@ for @destinationFrame@
destinationFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
destinationFrameSelector = mkSelector "destinationFrame"

