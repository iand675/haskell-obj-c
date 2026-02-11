{-# LANGUAGE PatternSynonyms #-}
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
  , initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlowSelector
  , initSelector
  , newSelector
  , sourceFrameSelector
  , nextFrameSelector
  , submissionModeSelector
  , destinationOpticalFlowSelector

  -- * Enum types
  , VTOpticalFlowParametersSubmissionMode(VTOpticalFlowParametersSubmissionMode)
  , pattern VTOpticalFlowParametersSubmissionModeRandom
  , pattern VTOpticalFlowParametersSubmissionModeSequential

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

-- | Creates a new optical flow parameters object.
--
-- Returns @nil@ if @sourceFrame@ or @nextFrame@ is @nil@, or if @sourceFrame@ and @nextFrame@ have different pixel formats.
--
-- - Parameters:   - sourceFrame: Current source frame; must be non @nil@.   - nextFrame: Next source frame in presentation time order.   - submissionMode: Provides a hint to let the processor know whether you are submitting frames in presentation   sequence. For more information about supported modes see ``VTOpticalFlowParametersSubmissionMode``.   - destinationOpticalFlow: User allocated @VTFrameProcessorOpticalFlow@ that receives the results.
--
-- ObjC selector: @- initWithSourceFrame:nextFrame:submissionMode:destinationOpticalFlow:@
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlow :: (IsVTOpticalFlowParameters vtOpticalFlowParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame nextFrame, IsVTFrameProcessorOpticalFlow destinationOpticalFlow) => vtOpticalFlowParameters -> sourceFrame -> nextFrame -> VTOpticalFlowParametersSubmissionMode -> destinationOpticalFlow -> IO (Id VTOpticalFlowParameters)
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlow vtOpticalFlowParameters  sourceFrame nextFrame submissionMode destinationOpticalFlow =
withObjCPtr sourceFrame $ \raw_sourceFrame ->
  withObjCPtr nextFrame $ \raw_nextFrame ->
    withObjCPtr destinationOpticalFlow $ \raw_destinationOpticalFlow ->
        sendMsg vtOpticalFlowParameters (mkSelector "initWithSourceFrame:nextFrame:submissionMode:destinationOpticalFlow:") (retPtr retVoid) [argPtr (castPtr raw_sourceFrame :: Ptr ()), argPtr (castPtr raw_nextFrame :: Ptr ()), argCLong (coerce submissionMode), argPtr (castPtr raw_destinationOpticalFlow :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTOpticalFlowParameters)
init_ vtOpticalFlowParameters  =
  sendMsg vtOpticalFlowParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTOpticalFlowParameters)
new  =
  do
    cls' <- getRequiredClass "VTOpticalFlowParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Current source frame, which must be non @nil@.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtOpticalFlowParameters  =
  sendMsg vtOpticalFlowParameters (mkSelector "sourceFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The next source frame in presentation time order.
--
-- ObjC selector: @- nextFrame@
nextFrame :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTFrameProcessorFrame)
nextFrame vtOpticalFlowParameters  =
  sendMsg vtOpticalFlowParameters (mkSelector "nextFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Ordering of the input frames in this submission relative to the previous submission.
--
-- ObjC selector: @- submissionMode@
submissionMode :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO VTOpticalFlowParametersSubmissionMode
submissionMode vtOpticalFlowParameters  =
  fmap (coerce :: CLong -> VTOpticalFlowParametersSubmissionMode) $ sendMsg vtOpticalFlowParameters (mkSelector "submissionMode") retCLong []

-- | Output optical flow calculated by the processor.
--
-- ObjC selector: @- destinationOpticalFlow@
destinationOpticalFlow :: IsVTOpticalFlowParameters vtOpticalFlowParameters => vtOpticalFlowParameters -> IO (Id VTFrameProcessorOpticalFlow)
destinationOpticalFlow vtOpticalFlowParameters  =
  sendMsg vtOpticalFlowParameters (mkSelector "destinationOpticalFlow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:nextFrame:submissionMode:destinationOpticalFlow:@
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlowSelector :: Selector
initWithSourceFrame_nextFrame_submissionMode_destinationOpticalFlowSelector = mkSelector "initWithSourceFrame:nextFrame:submissionMode:destinationOpticalFlow:"

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

-- | @Selector@ for @submissionMode@
submissionModeSelector :: Selector
submissionModeSelector = mkSelector "submissionMode"

-- | @Selector@ for @destinationOpticalFlow@
destinationOpticalFlowSelector :: Selector
destinationOpticalFlowSelector = mkSelector "destinationOpticalFlow"

