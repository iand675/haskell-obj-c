{-# LANGUAGE PatternSynonyms #-}
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
  , initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrameSelector
  , initSelector
  , newSelector
  , sourceFrameSelector
  , nextFrameSelector
  , previousFrameSelector
  , nextOpticalFlowSelector
  , previousOpticalFlowSelector
  , motionBlurStrengthSelector
  , submissionModeSelector
  , destinationFrameSelector

  -- * Enum types
  , VTMotionBlurParametersSubmissionMode(VTMotionBlurParametersSubmissionMode)
  , pattern VTMotionBlurParametersSubmissionModeRandom
  , pattern VTMotionBlurParametersSubmissionModeSequential

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
initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrame vtMotionBlurParameters  sourceFrame nextFrame previousFrame nextOpticalFlow previousOpticalFlow motionBlurStrength submissionMode destinationFrame =
withObjCPtr sourceFrame $ \raw_sourceFrame ->
  withObjCPtr nextFrame $ \raw_nextFrame ->
    withObjCPtr previousFrame $ \raw_previousFrame ->
      withObjCPtr nextOpticalFlow $ \raw_nextOpticalFlow ->
        withObjCPtr previousOpticalFlow $ \raw_previousOpticalFlow ->
          withObjCPtr destinationFrame $ \raw_destinationFrame ->
              sendMsg vtMotionBlurParameters (mkSelector "initWithSourceFrame:nextFrame:previousFrame:nextOpticalFlow:previousOpticalFlow:motionBlurStrength:submissionMode:destinationFrame:") (retPtr retVoid) [argPtr (castPtr raw_sourceFrame :: Ptr ()), argPtr (castPtr raw_nextFrame :: Ptr ()), argPtr (castPtr raw_previousFrame :: Ptr ()), argPtr (castPtr raw_nextOpticalFlow :: Ptr ()), argPtr (castPtr raw_previousOpticalFlow :: Ptr ()), argCLong (fromIntegral motionBlurStrength), argCLong (coerce submissionMode), argPtr (castPtr raw_destinationFrame :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTMotionBlurParameters)
init_ vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTMotionBlurParameters)
new  =
  do
    cls' <- getRequiredClass "VTMotionBlurParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "sourceFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The next source frame in presentation time order, which is @nil@ for the last frame.
--
-- ObjC selector: @- nextFrame@
nextFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
nextFrame vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "nextFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Previous source frame in presentation time order, which is @nil@ for the first frame.
--
-- ObjC selector: @- previousFrame@
previousFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
previousFrame vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "previousFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional frame processor optical flow object that contains forward and backward optical flow with next frame.
--
-- You only need to use this object if the optical flow is pre-computed. For the last frame this is @nil@.
--
-- ObjC selector: @- nextOpticalFlow@
nextOpticalFlow :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorOpticalFlow)
nextOpticalFlow vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "nextOpticalFlow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional frame processor optical flow object that contains forward and backward optical flow with previous frame.
--
-- You only need to use this object if the optical flow is pre-computed. For the first frame this is @nil@.
--
-- ObjC selector: @- previousOpticalFlow@
previousOpticalFlow :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorOpticalFlow)
previousOpticalFlow vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "previousOpticalFlow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Number that indicates the strength of motion blur.
--
-- The range is from 1 to 100; the default value is 50.
--
-- ObjC selector: @- motionBlurStrength@
motionBlurStrength :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO CLong
motionBlurStrength vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "motionBlurStrength") retCLong []

-- | Ordering of the input frames this submission related to the previous submission.
--
-- ObjC selector: @- submissionMode@
submissionMode :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO VTMotionBlurParametersSubmissionMode
submissionMode vtMotionBlurParameters  =
  fmap (coerce :: CLong -> VTMotionBlurParametersSubmissionMode) $ sendMsg vtMotionBlurParameters (mkSelector "submissionMode") retCLong []

-- | Destination frame that contains user-allocated pixel buffer that receive a frame with motion blur applied by the processor.
--
-- ObjC selector: @- destinationFrame@
destinationFrame :: IsVTMotionBlurParameters vtMotionBlurParameters => vtMotionBlurParameters -> IO (Id VTFrameProcessorFrame)
destinationFrame vtMotionBlurParameters  =
  sendMsg vtMotionBlurParameters (mkSelector "destinationFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:nextFrame:previousFrame:nextOpticalFlow:previousOpticalFlow:motionBlurStrength:submissionMode:destinationFrame:@
initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrameSelector :: Selector
initWithSourceFrame_nextFrame_previousFrame_nextOpticalFlow_previousOpticalFlow_motionBlurStrength_submissionMode_destinationFrameSelector = mkSelector "initWithSourceFrame:nextFrame:previousFrame:nextOpticalFlow:previousOpticalFlow:motionBlurStrength:submissionMode:destinationFrame:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @nextFrame@
nextFrameSelector :: Selector
nextFrameSelector = mkSelector "nextFrame"

-- | @Selector@ for @previousFrame@
previousFrameSelector :: Selector
previousFrameSelector = mkSelector "previousFrame"

-- | @Selector@ for @nextOpticalFlow@
nextOpticalFlowSelector :: Selector
nextOpticalFlowSelector = mkSelector "nextOpticalFlow"

-- | @Selector@ for @previousOpticalFlow@
previousOpticalFlowSelector :: Selector
previousOpticalFlowSelector = mkSelector "previousOpticalFlow"

-- | @Selector@ for @motionBlurStrength@
motionBlurStrengthSelector :: Selector
motionBlurStrengthSelector = mkSelector "motionBlurStrength"

-- | @Selector@ for @submissionMode@
submissionModeSelector :: Selector
submissionModeSelector = mkSelector "submissionMode"

-- | @Selector@ for @destinationFrame@
destinationFrameSelector :: Selector
destinationFrameSelector = mkSelector "destinationFrame"

