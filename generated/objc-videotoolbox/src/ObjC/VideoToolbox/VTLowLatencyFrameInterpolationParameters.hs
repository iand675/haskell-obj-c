{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains both input and output parameters that the low-latency frame interpolation processor needs.
--
-- Use this object in the @processWithParameters@ call of @VTFrameProcessor@ class.
--
-- @VTLowLatencyFrameInterpolationParameters@ are frame-level parameters.
--
-- Generated bindings for @VTLowLatencyFrameInterpolationParameters@.
module ObjC.VideoToolbox.VTLowLatencyFrameInterpolationParameters
  ( VTLowLatencyFrameInterpolationParameters
  , IsVTLowLatencyFrameInterpolationParameters(..)
  , initWithSourceFrame_previousFrame_interpolationPhase_destinationFrames
  , init_
  , new
  , sourceFrame
  , previousFrame
  , destinationFrames
  , initWithSourceFrame_previousFrame_interpolationPhase_destinationFramesSelector
  , initSelector
  , newSelector
  , sourceFrameSelector
  , previousFrameSelector
  , destinationFramesSelector


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
import ObjC.Foundation.Internal.Classes

-- | Creates a new low-latency frame interpolation parameters object.
--
-- - Parameters:   - sourceFrame: Current frame to use for interpolation; must be non @nil@.   - previousFrame: Previous frame used for interpolation; must be non @nil@.   - interpolationPhase: Array of float numbers that indicate interpolation phase locations at which the processor   interpolates the frames. Must be greater than 0 and less than 1.0; for example 0.5 is midway between the previous   frame and the source frame. If you enable spatial scaling, the only supported interpolation phase is 0.5.   - destinationFrames: Caller-allocated array of @VTFrameProcessorFrame@ to receive the interpolated frames. This   must have the same number of elements as the the @interpolationPhase@. If you enable spatial scaling, it must also   contain an element to hold the scaled version of sourceFrame.
--
-- ObjC selector: @- initWithSourceFrame:previousFrame:interpolationPhase:destinationFrames:@
initWithSourceFrame_previousFrame_interpolationPhase_destinationFrames :: (IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters, IsVTFrameProcessorFrame sourceFrame, IsVTFrameProcessorFrame previousFrame, IsNSArray interpolationPhase, IsNSArray destinationFrames) => vtLowLatencyFrameInterpolationParameters -> sourceFrame -> previousFrame -> interpolationPhase -> destinationFrames -> IO (Id VTLowLatencyFrameInterpolationParameters)
initWithSourceFrame_previousFrame_interpolationPhase_destinationFrames vtLowLatencyFrameInterpolationParameters  sourceFrame previousFrame interpolationPhase destinationFrames =
withObjCPtr sourceFrame $ \raw_sourceFrame ->
  withObjCPtr previousFrame $ \raw_previousFrame ->
    withObjCPtr interpolationPhase $ \raw_interpolationPhase ->
      withObjCPtr destinationFrames $ \raw_destinationFrames ->
          sendMsg vtLowLatencyFrameInterpolationParameters (mkSelector "initWithSourceFrame:previousFrame:interpolationPhase:destinationFrames:") (retPtr retVoid) [argPtr (castPtr raw_sourceFrame :: Ptr ()), argPtr (castPtr raw_previousFrame :: Ptr ()), argPtr (castPtr raw_interpolationPhase :: Ptr ()), argPtr (castPtr raw_destinationFrames :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id VTLowLatencyFrameInterpolationParameters)
init_ vtLowLatencyFrameInterpolationParameters  =
  sendMsg vtLowLatencyFrameInterpolationParameters (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTLowLatencyFrameInterpolationParameters)
new  =
  do
    cls' <- getRequiredClass "VTLowLatencyFrameInterpolationParameters"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Source frame that you provided when creating the low-latency frame interpolation parameters object.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtLowLatencyFrameInterpolationParameters  =
  sendMsg vtLowLatencyFrameInterpolationParameters (mkSelector "sourceFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Previous frame that you provided when creating the low-latency frame interpolation parameters object.
--
-- ObjC selector: @- previousFrame@
previousFrame :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id VTFrameProcessorFrame)
previousFrame vtLowLatencyFrameInterpolationParameters  =
  sendMsg vtLowLatencyFrameInterpolationParameters (mkSelector "previousFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of destination frames that you provided when creating the low-latency frame interpolation parameters object.
--
-- ObjC selector: @- destinationFrames@
destinationFrames :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id NSArray)
destinationFrames vtLowLatencyFrameInterpolationParameters  =
  sendMsg vtLowLatencyFrameInterpolationParameters (mkSelector "destinationFrames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:previousFrame:interpolationPhase:destinationFrames:@
initWithSourceFrame_previousFrame_interpolationPhase_destinationFramesSelector :: Selector
initWithSourceFrame_previousFrame_interpolationPhase_destinationFramesSelector = mkSelector "initWithSourceFrame:previousFrame:interpolationPhase:destinationFrames:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @previousFrame@
previousFrameSelector :: Selector
previousFrameSelector = mkSelector "previousFrame"

-- | @Selector@ for @destinationFrames@
destinationFramesSelector :: Selector
destinationFramesSelector = mkSelector "destinationFrames"

