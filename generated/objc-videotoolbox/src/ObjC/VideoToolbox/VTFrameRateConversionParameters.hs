{-# LANGUAGE PatternSynonyms #-}
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
  , submissionMode
  , destinationFrames
  , initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFramesSelector
  , initSelector
  , newSelector
  , sourceFrameSelector
  , nextFrameSelector
  , opticalFlowSelector
  , submissionModeSelector
  , destinationFramesSelector

  -- * Enum types
  , VTFrameRateConversionParametersSubmissionMode(VTFrameRateConversionParametersSubmissionMode)
  , pattern VTFrameRateConversionParametersSubmissionModeRandom
  , pattern VTFrameRateConversionParametersSubmissionModeSequential
  , pattern VTFrameRateConversionParametersSubmissionModeSequentialReferencesUnchanged

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

-- | Creates new frame rate conversion parameters.
--
-- Returns @nil@ if @sourceFrame@ or @nextFrame@ is @nil@, if @sourceFrame@ and reference frames don't have the same pixel format, or if @interpolationPhase@ array count does not match @destinationFrames@ array count.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - nextFrame: Next source frame in presentation time order; must be non @nil@.   - opticalFlow: Optional ``VTFrameProcessorOpticalFlow`` object that contains forward and backward optical flow with   next frame. You only need to use this if the optical flow is pre-computed. For the first frame this is always @nil@.   - interpolationPhase: Array of float numbers that indicate intervals at which the processor inserts a frame between   current and next frame. The array size indicates how many frames to interpolate and this size must match   @destinationFrames@ size, with one interval for each destination frame. Use float number values between 0 and 1,   for example, to insert one frame in the middle use a value of 0.5.   - submissionMode: Provides a hint to let the processor know whether you are submitting frames in presentation   sequence. For more information about supported modes see ``VTFrameRateConversionParametersSubmissionMode``.   - destinationFrames: Caller-allocated array of ``VTFrameProcessorFrame`` that contains pixel buffers to receive the results. Must contain the same number of elements as @interpolationPhase@.
--
-- ObjC selector: @- initWithSourceFrame:nextFrame:opticalFlow:interpolationPhase:submissionMode:destinationFrames:@
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFrames :: (IsVTFrameRateConversionParameters vtFrameRateConversionParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame nextFrame, IsVTFrameProcessorOpticalFlow opticalFlow, IsNSArray interpolationPhase, IsNSArray destinationFrame) => vtFrameRateConversionParameters -> sourceFrame -> nextFrame -> opticalFlow -> interpolationPhase -> VTFrameRateConversionParametersSubmissionMode -> destinationFrame -> IO (Id VTFrameRateConversionParameters)
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFrames vtFrameRateConversionParameters  sourceFrame nextFrame opticalFlow interpolationPhase submissionMode destinationFrame =
withObjCPtr sourceFrame $ \raw_sourceFrame ->
  withObjCPtr nextFrame $ \raw_nextFrame ->
    withObjCPtr opticalFlow $ \raw_opticalFlow ->
      withObjCPtr interpolationPhase $ \raw_interpolationPhase ->
        withObjCPtr destinationFrame $ \raw_destinationFrame ->
            sendMsg vtFrameRateConversionParameters (mkSelector "initWithSourceFrame:nextFrame:opticalFlow:interpolationPhase:submissionMode:destinationFrames:") (retPtr retVoid) [argPtr (castPtr raw_sourceFrame :: Ptr ()), argPtr (castPtr raw_nextFrame :: Ptr ()), argPtr (castPtr raw_opticalFlow :: Ptr ()), argPtr (castPtr raw_interpolationPhase :: Ptr ()), argCLong (coerce submissionMode), argPtr (castPtr raw_destinationFrame :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameRateConversionParameters)
init_ vtFrameRateConversionParameters  =
  sendMsg vtFrameRateConversionParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTFrameRateConversionParameters)
new  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtFrameRateConversionParameters  =
  sendMsg vtFrameRateConversionParameters (mkSelector "sourceFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The next source frame in presentation time order, which is @nil@ for the last frame.
--
-- ObjC selector: @- nextFrame@
nextFrame :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameProcessorFrame)
nextFrame vtFrameRateConversionParameters  =
  sendMsg vtFrameRateConversionParameters (mkSelector "nextFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional object that contains forward and backward optical flow with next frame.
--
-- Only needed if optical flow is pre-computed. For the last frame this is @nil@.
--
-- ObjC selector: @- opticalFlow@
opticalFlow :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id VTFrameProcessorOpticalFlow)
opticalFlow vtFrameRateConversionParameters  =
  sendMsg vtFrameRateConversionParameters (mkSelector "opticalFlow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Ordering of the input frames in this submission relative to the previous submission.
--
-- ObjC selector: @- submissionMode@
submissionMode :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO VTFrameRateConversionParametersSubmissionMode
submissionMode vtFrameRateConversionParameters  =
  fmap (coerce :: CLong -> VTFrameRateConversionParametersSubmissionMode) $ sendMsg vtFrameRateConversionParameters (mkSelector "submissionMode") retCLong []

-- | Caller-allocated array of video frame objects that contain pixel buffers to receive the results.
--
-- Must contain the same number of elements as @interpolationPhase@ NSArray.
--
-- ObjC selector: @- destinationFrames@
destinationFrames :: IsVTFrameRateConversionParameters vtFrameRateConversionParameters => vtFrameRateConversionParameters -> IO (Id NSArray)
destinationFrames vtFrameRateConversionParameters  =
  sendMsg vtFrameRateConversionParameters (mkSelector "destinationFrames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:nextFrame:opticalFlow:interpolationPhase:submissionMode:destinationFrames:@
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFramesSelector :: Selector
initWithSourceFrame_nextFrame_opticalFlow_interpolationPhase_submissionMode_destinationFramesSelector = mkSelector "initWithSourceFrame:nextFrame:opticalFlow:interpolationPhase:submissionMode:destinationFrames:"

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

-- | @Selector@ for @opticalFlow@
opticalFlowSelector :: Selector
opticalFlowSelector = mkSelector "opticalFlow"

-- | @Selector@ for @submissionMode@
submissionModeSelector :: Selector
submissionModeSelector = mkSelector "submissionMode"

-- | @Selector@ for @destinationFrames@
destinationFramesSelector :: Selector
destinationFramesSelector = mkSelector "destinationFrames"

