{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains both input and output parameters, which the frame-rate conversion processor needs to process a frame.
--
-- Use this object as a parameter to the ``VTFrameProcessor/processWithParameters`` method. The output parameter for this class is ``destinationFrame`` where the processor returns output frame (as mutable ``VTFrameProcessorFrame``) back to you once the @processWithParameters@ completes.
--
-- @VTFrameRateConversionParameters@ are frame-level parameters.
--
-- Generated bindings for @VTFrameRateConversionParameters@.
module ObjC.VideoToolbox.VTFrameRateConversionParameters
  ( VTFrameRateConversionParameters
  , IsVTFrameRateConversionParameters(..)
  , initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFrames
  , init_
  , new
  , sourceFrame
  , nextFrame
  , opticalFlow
  , interpolationPhase
  , submissionMode
  , destinationFrames
  , destinationFramesSelector
  , initSelector
  , initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFramesSelector
  , interpolationPhaseSelector
  , newSelector
  , nextFrameSelector
  , opticalFlowSelector
  , sourceFrameSelector
  , submissionModeSelector

  -- * Enum types
  , VTFrameRateConversionParametersSubmissionMode(VTFrameRateConversionParametersSubmissionMode)
  , pattern VTFrameRateConversionParametersSubmissionModeRandom
  , pattern VTFrameRateConversionParametersSubmissionModeSequential
  , pattern VTFrameRateConversionParametersSubmissionModeSequentialReferencesUnchanged

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

-- | Creates new frame rate conversion parameters.
--
-- Returns @nil@ if @sourceFrame@ or @nextFrame@ is @nil@, if @sourceFrame@ and reference frames don't have the same pixel format, or if @interpolationPhase@ array count does not match @destinationFrames@ array count.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - nextFrame: Next source frame in presentation time order; must be non @nil@.   - opticalFlow: Optional ``VTFrameProcessorOpticalFlow`` object that contains forward and backward optical flow with   next frame. You only need to use this if the optical flow is pre-computed. For the first frame this is always @nil@.   - interpolationPhase: Array of float numbers that indicate intervals at which the processor inserts a frame between   current and next frame. The array size indicates how many frames to interpolate and this size must match   @destinationFrames@ size, with one interval for each destination frame. Use float number values between 0 and 1,   for example, to insert one frame in the middle use a value of 0.5.   - submissionMode: Provides a hint to let the processor know whether you are submitting frames in presentation   sequence. For more information about supported modes see ``VTFrameRateConversionParametersSubmissionMode``.   - destinationFrames: Caller-allocated array of ``VTFrameProcessorFrame`` that contains pixel buffers to receive the results. Must contain the same number of elements as @interpolationPhase@.
--
-- ObjC selector: @- initWithSourceFrame:nextFrame:opticalFlow:interpolationPhase:submissionMode:destinationFrames:@
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFrames :: (IsVTFrameRateConversionParameters vtFrameRateConversionParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame nextFrame, IsVTFrameProcessorOpticalFlow opticalFlow, IsNSArray interpolationPhase, IsNSArray destinationFrame) => vtFrameRateConversionParameters -> sourceFrame -> nextFrame -> opticalFlow -> interpolationPhase -> VTFrameRateConversionParametersSubmissionMode -> destinationFrame -> IO (Id VTFrameRateConversionParameters)
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFrames vtFrameRateConversionParameters sourceFrame nextFrame opticalFlow interpolationPhase submissionMode destinationFrame =
  sendOwnedMessage vtFrameRateConversionParameters initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFramesSelector (toVTFrameProcessorFrame sourceFrame) (toVTFrameProcessorFrame nextFrame) (toVTFrameProcessorOpticalFlow opticalFlow) (toNSArray interpolationPhase) submissionMode (toNSArray destinationFrame)

-- | @- init@
init_ :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameRateConversionParameters)
init_ vtFrameRateConversionParameters =
  sendOwnedMessage vtFrameRateConversionParameters initSelector

-- | @+ new@
new :: IO (Id VTFrameRateConversionParameters)
new  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionParameters"
    sendOwnedClassMessage cls' newSelector

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtFrameRateConversionParameters =
  sendMessage vtFrameRateConversionParameters sourceFrameSelector

-- | The next source frame in presentation time order, which is @nil@ for the last frame.
--
-- ObjC selector: @- nextFrame@
nextFrame :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameProcessorFrame)
nextFrame vtFrameRateConversionParameters =
  sendMessage vtFrameRateConversionParameters nextFrameSelector

-- | An optional object that contains forward and backward optical flow with next frame.
--
-- Only needed if optical flow is pre-computed. For the last frame this is @nil@.
--
-- ObjC selector: @- opticalFlow@
opticalFlow :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameProcessorOpticalFlow)
opticalFlow vtFrameRateConversionParameters =
  sendMessage vtFrameRateConversionParameters opticalFlowSelector

-- | Array of float numbers that indicate intervals at which the processor inserts a frame between the current and next frame.
--
-- Array size indicates how many frames to interpolate and must match @destinationFrames@ size, one interval for each destination frame. Use float number values between 0 and 1, for example, to insert one frame in the middle use a value of 0.5.
--
-- ObjC selector: @- interpolationPhase@
interpolationPhase :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id NSArray)
interpolationPhase vtFrameRateConversionParameters =
  sendMessage vtFrameRateConversionParameters interpolationPhaseSelector

-- | Ordering of the input frames in this submission relative to the previous submission.
--
-- ObjC selector: @- submissionMode@
submissionMode :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO VTFrameRateConversionParametersSubmissionMode
submissionMode vtFrameRateConversionParameters =
  sendMessage vtFrameRateConversionParameters submissionModeSelector

-- | Caller-allocated array of video frame objects that contain pixel buffers to receive the results.
--
-- Must contain the same number of elements as @interpolationPhase@ NSArray.
--
-- ObjC selector: @- destinationFrames@
destinationFrames :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id NSArray)
destinationFrames vtFrameRateConversionParameters =
  sendMessage vtFrameRateConversionParameters destinationFramesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:nextFrame:opticalFlow:interpolationPhase:submissionMode:destinationFrames:@
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFramesSelector :: Selector '[Id VTFrameProcessorFrame, Id VTFrameProcessorFrame, Id VTFrameProcessorOpticalFlow, Id NSArray, VTFrameRateConversionParametersSubmissionMode, Id NSArray] (Id VTFrameRateConversionParameters)
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFramesSelector = mkSelector "initWithSourceFrame:nextFrame:opticalFlow:interpolationPhase:submissionMode:destinationFrames:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTFrameRateConversionParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTFrameRateConversionParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @nextFrame@
nextFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
nextFrameSelector = mkSelector "nextFrame"

-- | @Selector@ for @opticalFlow@
opticalFlowSelector :: Selector '[] (Id VTFrameProcessorOpticalFlow)
opticalFlowSelector = mkSelector "opticalFlow"

-- | @Selector@ for @interpolationPhase@
interpolationPhaseSelector :: Selector '[] (Id NSArray)
interpolationPhaseSelector = mkSelector "interpolationPhase"

-- | @Selector@ for @submissionMode@
submissionModeSelector :: Selector '[] VTFrameRateConversionParametersSubmissionMode
submissionModeSelector = mkSelector "submissionMode"

-- | @Selector@ for @destinationFrames@
destinationFramesSelector :: Selector '[] (Id NSArray)
destinationFramesSelector = mkSelector "destinationFrames"

