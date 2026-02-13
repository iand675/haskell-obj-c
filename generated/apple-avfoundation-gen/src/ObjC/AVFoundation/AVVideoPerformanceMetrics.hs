{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVVideoPerformanceMetrics
--
-- [SPI] An instance of AVVideoPerformanceMetrics provides current performance metrics.
--
-- An instance of AVVideoPerformanceMetrics provides named properties for accessing the video playback quality metrics.				Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVVideoPerformanceMetrics@.
module ObjC.AVFoundation.AVVideoPerformanceMetrics
  ( AVVideoPerformanceMetrics
  , IsAVVideoPerformanceMetrics(..)
  , init_
  , new
  , totalNumberOfFrames
  , numberOfDroppedFrames
  , numberOfCorruptedFrames
  , numberOfFramesDisplayedUsingOptimizedCompositing
  , totalAccumulatedFrameDelay
  , totalNumberOfVideoFrames
  , numberOfDroppedVideoFrames
  , numberOfCorruptedVideoFrames
  , numberOfDisplayCompositedVideoFrames
  , numberOfNonDisplayCompositedVideoFrames
  , totalFrameDelay
  , initSelector
  , newSelector
  , numberOfCorruptedFramesSelector
  , numberOfCorruptedVideoFramesSelector
  , numberOfDisplayCompositedVideoFramesSelector
  , numberOfDroppedFramesSelector
  , numberOfDroppedVideoFramesSelector
  , numberOfFramesDisplayedUsingOptimizedCompositingSelector
  , numberOfNonDisplayCompositedVideoFramesSelector
  , totalAccumulatedFrameDelaySelector
  , totalFrameDelaySelector
  , totalNumberOfFramesSelector
  , totalNumberOfVideoFramesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO (Id AVVideoPerformanceMetrics)
init_ avVideoPerformanceMetrics =
  sendOwnedMessage avVideoPerformanceMetrics initSelector

-- | @+ new@
new :: IO (Id AVVideoPerformanceMetrics)
new  =
  do
    cls' <- getRequiredClass "AVVideoPerformanceMetrics"
    sendOwnedClassMessage cls' newSelector

-- | totalNumberOfFrames
--
-- [SPI] The total number of frames that would have been displayed if no frames are dropped.
--
-- ObjC selector: @- totalNumberOfFrames@
totalNumberOfFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
totalNumberOfFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics totalNumberOfFramesSelector

-- | numberOfDroppedFrames
--
-- [SPI] The total number of frames dropped prior to decoding or dropped because a frame missed its display deadline.
--
-- ObjC selector: @- numberOfDroppedFrames@
numberOfDroppedFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
numberOfDroppedFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics numberOfDroppedFramesSelector

-- | numberOfCorruptedFrames
--
-- [SPI] The total number of corrupted frames that have been detected.
--
-- ObjC selector: @- numberOfCorruptedFrames@
numberOfCorruptedFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
numberOfCorruptedFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics numberOfCorruptedFramesSelector

-- | numberOfFramesDisplayedUsingOptimizedCompositing
--
-- [SPI] The total number of full screen frames that were rendered in a special power-efficient mode that didn't require the frame to be composited with other UI elements.
--
-- ObjC selector: @- numberOfFramesDisplayedUsingOptimizedCompositing@
numberOfFramesDisplayedUsingOptimizedCompositing :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
numberOfFramesDisplayedUsingOptimizedCompositing avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics numberOfFramesDisplayedUsingOptimizedCompositingSelector

-- | totalAccumulatedFrameDelay
--
-- [SPI] The accumulated amount of time between the prescribed presentation times of displayed video frames and the actual time at which they were displayed.
--
-- This delay is always greater than or equal to zero since frames must never be displayed before their presentation time. Non-zero delays are a sign of playback jitter and possible loss of A/V sync.
--
-- ObjC selector: @- totalAccumulatedFrameDelay@
totalAccumulatedFrameDelay :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CDouble
totalAccumulatedFrameDelay avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics totalAccumulatedFrameDelaySelector

-- | totalNumberOfVideoFrames
--
-- [SPI] The total number of frames that would have been displayed if no frames are dropped. Same as totalNumberOfFrames.
--
-- ObjC selector: @- totalNumberOfVideoFrames@
totalNumberOfVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
totalNumberOfVideoFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics totalNumberOfVideoFramesSelector

-- | numberOfDroppedVideoFrames
--
-- [SPI] The total number of frames dropped prior to decoding or dropped because a frame missed its display deadline. Same as numberOfDroppedFrames.
--
-- ObjC selector: @- numberOfDroppedVideoFrames@
numberOfDroppedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfDroppedVideoFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics numberOfDroppedVideoFramesSelector

-- | numberOfCorruptedVideoFrames
--
-- [SPI] The total number of corrupted frames that have been detected. Same as numberOfCorruptedFrames.
--
-- ObjC selector: @- numberOfCorruptedVideoFrames@
numberOfCorruptedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfCorruptedVideoFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics numberOfCorruptedVideoFramesSelector

-- | numberOfDisplayCompositedFrames
--
-- [SPI] The total number of frames that were composited in detached mode.  Same as numberOfFramesDisplayedUsingOptimizedCompositing.
--
-- ObjC selector: @- numberOfDisplayCompositedVideoFrames@
numberOfDisplayCompositedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfDisplayCompositedVideoFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics numberOfDisplayCompositedVideoFramesSelector

-- | numberOfNonDisplayCompositedFrames
--
-- [SPI] The total number of frames that were composited in undetached mode.
--
-- ObjC selector: @- numberOfNonDisplayCompositedVideoFrames@
numberOfNonDisplayCompositedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfNonDisplayCompositedVideoFrames avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics numberOfNonDisplayCompositedVideoFramesSelector

-- | totalFrameDelay
--
-- [SPI] The accumulated amount of time, in microseconds, between the prescribed presentation times of displayed video frames and the actual time at which they were displayed.
--
-- This delay is always greater than or equal to zero since frames must never be displayed before their presentation time. Non-zero delays are a sign of playback jitter and possible loss of A/V sync. Same as totalAccumulatedFrameDelay.
--
-- ObjC selector: @- totalFrameDelay@
totalFrameDelay :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CDouble
totalFrameDelay avVideoPerformanceMetrics =
  sendMessage avVideoPerformanceMetrics totalFrameDelaySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVVideoPerformanceMetrics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVVideoPerformanceMetrics)
newSelector = mkSelector "new"

-- | @Selector@ for @totalNumberOfFrames@
totalNumberOfFramesSelector :: Selector '[] CLong
totalNumberOfFramesSelector = mkSelector "totalNumberOfFrames"

-- | @Selector@ for @numberOfDroppedFrames@
numberOfDroppedFramesSelector :: Selector '[] CLong
numberOfDroppedFramesSelector = mkSelector "numberOfDroppedFrames"

-- | @Selector@ for @numberOfCorruptedFrames@
numberOfCorruptedFramesSelector :: Selector '[] CLong
numberOfCorruptedFramesSelector = mkSelector "numberOfCorruptedFrames"

-- | @Selector@ for @numberOfFramesDisplayedUsingOptimizedCompositing@
numberOfFramesDisplayedUsingOptimizedCompositingSelector :: Selector '[] CLong
numberOfFramesDisplayedUsingOptimizedCompositingSelector = mkSelector "numberOfFramesDisplayedUsingOptimizedCompositing"

-- | @Selector@ for @totalAccumulatedFrameDelay@
totalAccumulatedFrameDelaySelector :: Selector '[] CDouble
totalAccumulatedFrameDelaySelector = mkSelector "totalAccumulatedFrameDelay"

-- | @Selector@ for @totalNumberOfVideoFrames@
totalNumberOfVideoFramesSelector :: Selector '[] CULong
totalNumberOfVideoFramesSelector = mkSelector "totalNumberOfVideoFrames"

-- | @Selector@ for @numberOfDroppedVideoFrames@
numberOfDroppedVideoFramesSelector :: Selector '[] CULong
numberOfDroppedVideoFramesSelector = mkSelector "numberOfDroppedVideoFrames"

-- | @Selector@ for @numberOfCorruptedVideoFrames@
numberOfCorruptedVideoFramesSelector :: Selector '[] CULong
numberOfCorruptedVideoFramesSelector = mkSelector "numberOfCorruptedVideoFrames"

-- | @Selector@ for @numberOfDisplayCompositedVideoFrames@
numberOfDisplayCompositedVideoFramesSelector :: Selector '[] CULong
numberOfDisplayCompositedVideoFramesSelector = mkSelector "numberOfDisplayCompositedVideoFrames"

-- | @Selector@ for @numberOfNonDisplayCompositedVideoFrames@
numberOfNonDisplayCompositedVideoFramesSelector :: Selector '[] CULong
numberOfNonDisplayCompositedVideoFramesSelector = mkSelector "numberOfNonDisplayCompositedVideoFrames"

-- | @Selector@ for @totalFrameDelay@
totalFrameDelaySelector :: Selector '[] CDouble
totalFrameDelaySelector = mkSelector "totalFrameDelay"

