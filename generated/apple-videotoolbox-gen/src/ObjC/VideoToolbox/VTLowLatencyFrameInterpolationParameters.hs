{-# LANGUAGE DataKinds #-}
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
  , interpolationPhase
  , destinationFrames
  , destinationFramesSelector
  , initSelector
  , initWithSourceFrame_previousFrame_interpolationPhase_destinationFramesSelector
  , interpolationPhaseSelector
  , newSelector
  , previousFrameSelector
  , sourceFrameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithSourceFrame_previousFrame_interpolationPhase_destinationFrames vtLowLatencyFrameInterpolationParameters sourceFrame previousFrame interpolationPhase destinationFrames =
  sendOwnedMessage vtLowLatencyFrameInterpolationParameters initWithSourceFrame_previousFrame_interpolationPhase_destinationFramesSelector (toVTFrameProcessorFrame sourceFrame) (toVTFrameProcessorFrame previousFrame) (toNSArray interpolationPhase) (toNSArray destinationFrames)

-- | @- init@
init_ :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id VTLowLatencyFrameInterpolationParameters)
init_ vtLowLatencyFrameInterpolationParameters =
  sendOwnedMessage vtLowLatencyFrameInterpolationParameters initSelector

-- | @+ new@
new :: IO (Id VTLowLatencyFrameInterpolationParameters)
new  =
  do
    cls' <- getRequiredClass "VTLowLatencyFrameInterpolationParameters"
    sendOwnedClassMessage cls' newSelector

-- | Source frame that you provided when creating the low-latency frame interpolation parameters object.
--
-- ObjC selector: @- sourceFrame@
sourceFrame :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id VTFrameProcessorFrame)
sourceFrame vtLowLatencyFrameInterpolationParameters =
  sendMessage vtLowLatencyFrameInterpolationParameters sourceFrameSelector

-- | Previous frame that you provided when creating the low-latency frame interpolation parameters object.
--
-- ObjC selector: @- previousFrame@
previousFrame :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id VTFrameProcessorFrame)
previousFrame vtLowLatencyFrameInterpolationParameters =
  sendMessage vtLowLatencyFrameInterpolationParameters previousFrameSelector

-- | Array of interpolation phases that you provided when creating the low-latency frame interpolation parameters object.
--
-- ObjC selector: @- interpolationPhase@
interpolationPhase :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id NSArray)
interpolationPhase vtLowLatencyFrameInterpolationParameters =
  sendMessage vtLowLatencyFrameInterpolationParameters interpolationPhaseSelector

-- | Array of destination frames that you provided when creating the low-latency frame interpolation parameters object.
--
-- ObjC selector: @- destinationFrames@
destinationFrames :: IsVTLowLatencyFrameInterpolationParameters vtLowLatencyFrameInterpolationParameters => vtLowLatencyFrameInterpolationParameters -> IO (Id NSArray)
destinationFrames vtLowLatencyFrameInterpolationParameters =
  sendMessage vtLowLatencyFrameInterpolationParameters destinationFramesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSourceFrame:previousFrame:interpolationPhase:destinationFrames:@
initWithSourceFrame_previousFrame_interpolationPhase_destinationFramesSelector :: Selector '[Id VTFrameProcessorFrame, Id VTFrameProcessorFrame, Id NSArray, Id NSArray] (Id VTLowLatencyFrameInterpolationParameters)
initWithSourceFrame_previousFrame_interpolationPhase_destinationFramesSelector = mkSelector "initWithSourceFrame:previousFrame:interpolationPhase:destinationFrames:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTLowLatencyFrameInterpolationParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTLowLatencyFrameInterpolationParameters)
newSelector = mkSelector "new"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @previousFrame@
previousFrameSelector :: Selector '[] (Id VTFrameProcessorFrame)
previousFrameSelector = mkSelector "previousFrame"

-- | @Selector@ for @interpolationPhase@
interpolationPhaseSelector :: Selector '[] (Id NSArray)
interpolationPhaseSelector = mkSelector "interpolationPhase"

-- | @Selector@ for @destinationFrames@
destinationFramesSelector :: Selector '[] (Id NSArray)
destinationFramesSelector = mkSelector "destinationFrames"

