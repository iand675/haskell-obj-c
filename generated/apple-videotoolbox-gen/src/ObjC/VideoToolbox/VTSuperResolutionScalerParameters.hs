{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains both input and output parameters that the super-resolution processor needs to run on a frame.
--
-- Use this object in the @processWithParameters@ call of the @VTFrameProcessor@ class. The output parameter for this class is @destinationFrame@, where the processor returns the output frame (as @VTFrameProcessorFrame@) back to you once @processWithParameters@ completes.
--
-- @VTSuperResolutionScalerParameters@ are frame-level parameters.
--
-- Generated bindings for @VTSuperResolutionScalerParameters@.
module ObjC.VideoToolbox.VTSuperResolutionScalerParameters
  ( VTSuperResolutionScalerParameters
  , IsVTSuperResolutionScalerParameters(..)
  , initWithSourceFrame_previousFrame_previousOutputFrame_opticalFlow_submissionMode_destinationFrame
  , init_
  , new
  , sourceFrame
  , previousFrame
  , previousOutputFrame
  , opticalFlow
  , submissionMode
  , destinationFrame
  , destinationFrameSelector
  , initSelector
  , initWithSourceFrame_previousFrame_previousOutputFrame_opticalFlow_submissionMode_destinationFrameSelector
  , newSelector
  , opticalFlowSelector
  , previousFrameSelector
  , previousOutputFrameSelector
  , sourceFrameSelector
  , submissionModeSelector

  -- * Enum types
  , VTSuperResolutionScalerParametersSubmissionMode(VTSuperResolutionScalerParametersSubmissionMode)
  , pattern VTSuperResolutionScalerParametersSubmissionModeRandom
  , pattern VTSuperResolutionScalerParametersSubmissionModeSequential

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

-- | Creates a new super-resolution scaler parameters instance.
--
-- Returns @nil@ if @sourceFrame@ or @destinationFrame@ is @nil@, or if @sourceFrame@ and reference frames have different pixel formats.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - previousFrame: The previous source frame in presentation time order. For the first frame you can set this to @nil@.   - previousOutputFrame: The previous output frame in presentation time order. For the first frame you can set this to @nil@.   - opticalFlow: Optional @VTFrameProcessorOpticalFlow@ object that contains forward and backward optical flow between the @sourceFrame@ and @previousFrame@. You only need this if optical flow is pre-computed.   - submissionMode: Provides a hint to let the processor know whether you are submitting frames in presentation   sequence. For more information about supported modes see ``VTSuperResolutionScalerParametersSubmissionMode``.   - destinationFrame: User-allocated pixel buffer that receives the results.
--
-- ObjC selector: @- initWithSourceFrame:previousFrame:previousOutputFrame:opticalFlow:submissionMode:destinationFrame:@
initWithSourceFrame_previousFrame_previousOutputFrame_opticalFlow_submissionMode_destinationFrame :: (IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame previousFrame, IsVTFrameProcessorFrame previousOutputFrame, IsVTFrameProcessorOpticalFlow opticalFlow, IsVTFrameProcessorFrame destinationFrame) => vtSuperResolutionScalerParameters -> sourceFrame -> previousFrame -> previousOutputFrame -> opticalFlow -> VTSuperResolutionScalerParametersSubmissionMode -> destinationFrame -> IO (Id VTSuperResolutionScalerParameters)
initWithSourceFrame_previousFrame_previousOutputFrame_opticalFlow_submissionMode_destinationFrame vtSuperResolutionScalerParameters sourceFrame previousFrame previousOutputFrame opticalFlow submissionMode destinationFrame =
  sendOwnedMessage vtSuperResolutionScalerParameters initWithSourceFrame_previousFrame_previousOutputFrame_opticalFlow_submissionMode_destinationFrameSelector (toVTFrameProcessorFrame sourceFrame) (toVTFrameProcessorFrame previousFrame) (toVTFrameProcessorFrame previousOutputFrame) (toVTFrameProcessorOpticalFlow opticalFlow) submissionMode (toVTFrameProcessorFrame destinationFrame)

-- | @- init@
init_ :: IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters => vtSuperResolutionScalerParameters -> IO (Id VTSuperResolutionScalerParameters)
init_ vtSuperResolutionScalerParameters =
  sendOwnedMessage vtSuperResolutionScalerParameters initSelector

-- | @+ new@
new :: IO (Id VTSuperResolutionScalerParameters)
new  =
  do
    cls' <- getRequiredClass "VTSuperResolutionScalerParameters"
    sendOwnedClassMessage cls' newSelector

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters => vtSuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtSuperResolutionScalerParameters =
  sendMessage vtSuperResolutionScalerParameters sourceFrameSelector

-- | Previous source frame in presentation time order, which is @nil@ for the first frame.
--
-- ObjC selector: @- previousFrame@
previousFrame :: IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters => vtSuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
previousFrame vtSuperResolutionScalerParameters =
  sendMessage vtSuperResolutionScalerParameters previousFrameSelector

-- | Previous output frame in presentation time order, which is @nil@ for the first frame.
--
-- ObjC selector: @- previousOutputFrame@
previousOutputFrame :: IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters => vtSuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
previousOutputFrame vtSuperResolutionScalerParameters =
  sendMessage vtSuperResolutionScalerParameters previousOutputFrameSelector

-- | Optional object that contains forward and backward optical flow with the previous frame.
--
-- You only need this if optical flow is pre-computed. For the first frame this is @nil@.
--
-- ObjC selector: @- opticalFlow@
opticalFlow :: IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters => vtSuperResolutionScalerParameters -> IO (Id VTFrameProcessorOpticalFlow)
opticalFlow vtSuperResolutionScalerParameters =
  sendMessage vtSuperResolutionScalerParameters opticalFlowSelector

-- | Ordering of the input frames in this submission relative to the previous submission.
--
-- ObjC selector: @- submissionMode@
submissionMode :: IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters => vtSuperResolutionScalerParameters -> IO VTSuperResolutionScalerParametersSubmissionMode
submissionMode vtSuperResolutionScalerParameters =
  sendMessage vtSuperResolutionScalerParameters submissionModeSelector

-- | Destination frame that contains user-allocated pixel buffer that receives the results.
--
-- ObjC selector: @- destinationFrame@
destinationFrame :: IsVTSuperResolutionScalerParameters vtSuperResolutionScalerParameters => vtSuperResolutionScalerParameters -> IO (Id VTFrameProcessorFrame)
destinationFrame vtSuperResolutionScalerParameters =
  sendMessage vtSuperResolutionScalerParameters destinationFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:previousFrame:previousOutputFrame:opticalFlow:submissionMode:destinationFrame:@
initWithSourceFrame_previousFrame_previousOutputFrame_opticalFlow_submissionMode_destinationFrameSelector :: Selector '[Id VTFrameProcessorFrame, Id VTFrameProcessorFrame, Id VTFrameProcessorFrame, Id VTFrameProcessorOpticalFlow, VTSuperResolutionScalerParametersSubmissionMode, Id VTFrameProcessorFrame] (Id VTSuperResolutionScalerParameters)
initWithSourceFrame_previousFrame_previousOutputFrame_opticalFlow_submissionMode_destinationFrameSelector = mkSelector "initWithSourceFrame:previousFrame:previousOutputFrame:opticalFlow:submissionMode:destinationFrame:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTSuperResolutionScalerParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTSuperResolutionScalerParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @previousFrame@
previousFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
previousFrameSelector = mkSelector "previousFrame"

-- | @Selector@ for @previousOutputFrame@
previousOutputFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
previousOutputFrameSelector = mkSelector "previousOutputFrame"

-- | @Selector@ for @opticalFlow@
opticalFlowSelector :: Selector '[] (Id VTFrameProcessorOpticalFlow)
opticalFlowSelector = mkSelector "opticalFlow"

-- | @Selector@ for @submissionMode@
submissionModeSelector :: Selector '[] VTSuperResolutionScalerParametersSubmissionMode
submissionModeSelector = mkSelector "submissionMode"

-- | @Selector@ for @destinationFrame@
destinationFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
destinationFrameSelector = mkSelector "destinationFrame"

