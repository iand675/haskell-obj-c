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
  , totalNumberOfFramesSelector
  , numberOfDroppedFramesSelector
  , numberOfCorruptedFramesSelector
  , numberOfFramesDisplayedUsingOptimizedCompositingSelector
  , totalAccumulatedFrameDelaySelector
  , totalNumberOfVideoFramesSelector
  , numberOfDroppedVideoFramesSelector
  , numberOfCorruptedVideoFramesSelector
  , numberOfDisplayCompositedVideoFramesSelector
  , numberOfNonDisplayCompositedVideoFramesSelector
  , totalFrameDelaySelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO (Id AVVideoPerformanceMetrics)
init_ avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVVideoPerformanceMetrics)
new  =
  do
    cls' <- getRequiredClass "AVVideoPerformanceMetrics"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | totalNumberOfFrames
--
-- [SPI] The total number of frames that would have been displayed if no frames are dropped.
--
-- ObjC selector: @- totalNumberOfFrames@
totalNumberOfFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
totalNumberOfFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "totalNumberOfFrames") retCLong []

-- | numberOfDroppedFrames
--
-- [SPI] The total number of frames dropped prior to decoding or dropped because a frame missed its display deadline.
--
-- ObjC selector: @- numberOfDroppedFrames@
numberOfDroppedFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
numberOfDroppedFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "numberOfDroppedFrames") retCLong []

-- | numberOfCorruptedFrames
--
-- [SPI] The total number of corrupted frames that have been detected.
--
-- ObjC selector: @- numberOfCorruptedFrames@
numberOfCorruptedFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
numberOfCorruptedFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "numberOfCorruptedFrames") retCLong []

-- | numberOfFramesDisplayedUsingOptimizedCompositing
--
-- [SPI] The total number of full screen frames that were rendered in a special power-efficient mode that didn't require the frame to be composited with other UI elements.
--
-- ObjC selector: @- numberOfFramesDisplayedUsingOptimizedCompositing@
numberOfFramesDisplayedUsingOptimizedCompositing :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CLong
numberOfFramesDisplayedUsingOptimizedCompositing avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "numberOfFramesDisplayedUsingOptimizedCompositing") retCLong []

-- | totalAccumulatedFrameDelay
--
-- [SPI] The accumulated amount of time between the prescribed presentation times of displayed video frames and the actual time at which they were displayed.
--
-- This delay is always greater than or equal to zero since frames must never be displayed before their presentation time. Non-zero delays are a sign of playback jitter and possible loss of A/V sync.
--
-- ObjC selector: @- totalAccumulatedFrameDelay@
totalAccumulatedFrameDelay :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CDouble
totalAccumulatedFrameDelay avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "totalAccumulatedFrameDelay") retCDouble []

-- | totalNumberOfVideoFrames
--
-- [SPI] The total number of frames that would have been displayed if no frames are dropped. Same as totalNumberOfFrames.
--
-- ObjC selector: @- totalNumberOfVideoFrames@
totalNumberOfVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
totalNumberOfVideoFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "totalNumberOfVideoFrames") retCULong []

-- | numberOfDroppedVideoFrames
--
-- [SPI] The total number of frames dropped prior to decoding or dropped because a frame missed its display deadline. Same as numberOfDroppedFrames.
--
-- ObjC selector: @- numberOfDroppedVideoFrames@
numberOfDroppedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfDroppedVideoFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "numberOfDroppedVideoFrames") retCULong []

-- | numberOfCorruptedVideoFrames
--
-- [SPI] The total number of corrupted frames that have been detected. Same as numberOfCorruptedFrames.
--
-- ObjC selector: @- numberOfCorruptedVideoFrames@
numberOfCorruptedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfCorruptedVideoFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "numberOfCorruptedVideoFrames") retCULong []

-- | numberOfDisplayCompositedFrames
--
-- [SPI] The total number of frames that were composited in detached mode.  Same as numberOfFramesDisplayedUsingOptimizedCompositing.
--
-- ObjC selector: @- numberOfDisplayCompositedVideoFrames@
numberOfDisplayCompositedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfDisplayCompositedVideoFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "numberOfDisplayCompositedVideoFrames") retCULong []

-- | numberOfNonDisplayCompositedFrames
--
-- [SPI] The total number of frames that were composited in undetached mode.
--
-- ObjC selector: @- numberOfNonDisplayCompositedVideoFrames@
numberOfNonDisplayCompositedVideoFrames :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CULong
numberOfNonDisplayCompositedVideoFrames avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "numberOfNonDisplayCompositedVideoFrames") retCULong []

-- | totalFrameDelay
--
-- [SPI] The accumulated amount of time, in microseconds, between the prescribed presentation times of displayed video frames and the actual time at which they were displayed.
--
-- This delay is always greater than or equal to zero since frames must never be displayed before their presentation time. Non-zero delays are a sign of playback jitter and possible loss of A/V sync. Same as totalAccumulatedFrameDelay.
--
-- ObjC selector: @- totalFrameDelay@
totalFrameDelay :: IsAVVideoPerformanceMetrics avVideoPerformanceMetrics => avVideoPerformanceMetrics -> IO CDouble
totalFrameDelay avVideoPerformanceMetrics  =
  sendMsg avVideoPerformanceMetrics (mkSelector "totalFrameDelay") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @totalNumberOfFrames@
totalNumberOfFramesSelector :: Selector
totalNumberOfFramesSelector = mkSelector "totalNumberOfFrames"

-- | @Selector@ for @numberOfDroppedFrames@
numberOfDroppedFramesSelector :: Selector
numberOfDroppedFramesSelector = mkSelector "numberOfDroppedFrames"

-- | @Selector@ for @numberOfCorruptedFrames@
numberOfCorruptedFramesSelector :: Selector
numberOfCorruptedFramesSelector = mkSelector "numberOfCorruptedFrames"

-- | @Selector@ for @numberOfFramesDisplayedUsingOptimizedCompositing@
numberOfFramesDisplayedUsingOptimizedCompositingSelector :: Selector
numberOfFramesDisplayedUsingOptimizedCompositingSelector = mkSelector "numberOfFramesDisplayedUsingOptimizedCompositing"

-- | @Selector@ for @totalAccumulatedFrameDelay@
totalAccumulatedFrameDelaySelector :: Selector
totalAccumulatedFrameDelaySelector = mkSelector "totalAccumulatedFrameDelay"

-- | @Selector@ for @totalNumberOfVideoFrames@
totalNumberOfVideoFramesSelector :: Selector
totalNumberOfVideoFramesSelector = mkSelector "totalNumberOfVideoFrames"

-- | @Selector@ for @numberOfDroppedVideoFrames@
numberOfDroppedVideoFramesSelector :: Selector
numberOfDroppedVideoFramesSelector = mkSelector "numberOfDroppedVideoFrames"

-- | @Selector@ for @numberOfCorruptedVideoFrames@
numberOfCorruptedVideoFramesSelector :: Selector
numberOfCorruptedVideoFramesSelector = mkSelector "numberOfCorruptedVideoFrames"

-- | @Selector@ for @numberOfDisplayCompositedVideoFrames@
numberOfDisplayCompositedVideoFramesSelector :: Selector
numberOfDisplayCompositedVideoFramesSelector = mkSelector "numberOfDisplayCompositedVideoFrames"

-- | @Selector@ for @numberOfNonDisplayCompositedVideoFrames@
numberOfNonDisplayCompositedVideoFramesSelector :: Selector
numberOfNonDisplayCompositedVideoFramesSelector = mkSelector "numberOfNonDisplayCompositedVideoFrames"

-- | @Selector@ for @totalFrameDelay@
totalFrameDelaySelector :: Selector
totalFrameDelaySelector = mkSelector "totalFrameDelay"

