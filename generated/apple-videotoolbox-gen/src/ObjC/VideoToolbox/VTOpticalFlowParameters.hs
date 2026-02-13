{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains both input and output parameters the frame processor needs to generate optical flow between two frames.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class. The output parameter for this class is @destinationOpticalFlow@ where the processor returns the output flow (as mutable @VTFrameProcessorOpticalFlow@) back to you once the @processWithParameters@ completes.
--
-- @VTOpticalFlowParameters@ are frame-level parameters.
--
-- Generated bindings for @VTOpticalFlowParameters@.
module ObjC.VideoToolbox.VTOpticalFlowParameters
  ( VTOpticalFlowParameters
  , IsVTOpticalFlowParameters(..)
  , initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlow
  , init_
  , new
  , sourceFrame
  , nextFrame
  , submissionMode
  , destinationOpticalFlow
  , destinationOpticalFlowSelector
  , initSelector
  , initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlowSelector
  , newSelector
  , nextFrameSelector
  , sourceFrameSelector
  , submissionModeSelector

  -- * Enum types
  , VTOpticalFlowParametersSubmissionMode(VTOpticalFlowParametersSubmissionMode)
  , pattern VTOpticalFlowParametersSubmissionModeRandom
  , pattern VTOpticalFlowParametersSubmissionModeSequential

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

-- | Creates a new optical flow parameters object.
--
-- Returns @nil@ if @sourceFrame@ or @nextFrame@ is @nil@, or if @sourceFrame@ and @nextFrame@ have different pixel formats.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - nextFrame: Next source frame in presentation time order.   - submissionMode: Provides a hint to let the processor know whether you are submitting frames in presentation   sequence. For more information about supported modes see ``VTOpticalFlowParametersSubmissionMode``.   - destinationOpticalFlow: User allocated @VTFrameProcessorOpticalFlow@ that receives the results.
--
-- ObjC selector: @- initWithSourceFrame:nextFrame:submissionMode:destinationOpticalFlow:@
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlow :: (IsVTOpticalFlowParameters vtOpticalFlowParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame nextFrame, IsVTFrameProcessorOpticalFlow destinationOpticalFlow) => vtOpticalFlowParameters -> sourceFrame -> nextFrame -> VTOpticalFlowParametersSubmissionMode -> destinationOpticalFlow -> IO (Id VTOpticalFlowParameters)
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlow vtOpticalFlowParameters sourceFrame nextFrame submissionMode destinationOpticalFlow =
  sendOwnedMessage vtOpticalFlowParameters initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlowSelector (toVTFrameProcessorFrame sourceFrame) (toVTFrameProcessorFrame nextFrame) submissionMode (toVTFrameProcessorOpticalFlow destinationOpticalFlow)

-- | @- init@
init_ :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTOpticalFlowParameters)
init_ vtOpticalFlowParameters =
  sendOwnedMessage vtOpticalFlowParameters initSelector

-- | @+ new@
new :: IO (Id VTOpticalFlowParameters)
new  =
  do
    cls' <- getRequiredClass "VTOpticalFlowParameters"
    sendOwnedClassMessage cls' newSelector

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtOpticalFlowParameters =
  sendMessage vtOpticalFlowParameters sourceFrameSelector

-- | The next source frame in presentation time order.
--
-- ObjC selector: @- nextFrame@
nextFrame :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTFrameProcessorFrame)
nextFrame vtOpticalFlowParameters =
  sendMessage vtOpticalFlowParameters nextFrameSelector

-- | Ordering of the input frames in this submission relative to the previous submission.
--
-- ObjC selector: @- submissionMode@
submissionMode :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO VTOpticalFlowParametersSubmissionMode
submissionMode vtOpticalFlowParameters =
  sendMessage vtOpticalFlowParameters submissionModeSelector

-- | Output optical flow calculated by the processor.
--
-- ObjC selector: @- destinationOpticalFlow@
destinationOpticalFlow :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTFrameProcessorOpticalFlow)
destinationOpticalFlow vtOpticalFlowParameters =
  sendMessage vtOpticalFlowParameters destinationOpticalFlowSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:nextFrame:submissionMode:destinationOpticalFlow:@
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlowSelector :: Selector '[Id VTFrameProcessorFrame, Id VTFrameProcessorFrame, VTOpticalFlowParametersSubmissionMode, Id VTFrameProcessorOpticalFlow] (Id VTOpticalFlowParameters)
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlowSelector = mkSelector "initWithSourceFrame:nextFrame:submissionMode:destinationOpticalFlow:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTOpticalFlowParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTOpticalFlowParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @nextFrame@
nextFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
nextFrameSelector = mkSelector "nextFrame"

-- | @Selector@ for @submissionMode@
submissionModeSelector :: Selector '[] VTOpticalFlowParametersSubmissionMode
submissionModeSelector = mkSelector "submissionMode"

-- | @Selector@ for @destinationOpticalFlow@
destinationOpticalFlowSelector :: Selector '[] (Id VTFrameProcessorOpticalFlow)
destinationOpticalFlowSelector = mkSelector "destinationOpticalFlow"

