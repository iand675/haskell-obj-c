{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVPlayerItemVideoOutput@.
module ObjC.AVFoundation.AVPlayerItemVideoOutput
  ( AVPlayerItemVideoOutput
  , IsAVPlayerItemVideoOutput(..)
  , initWithPixelBufferAttributes
  , initWithOutputSettings
  , setDelegate_queue
  , requestNotificationOfMediaDataChangeWithAdvanceInterval
  , delegate
  , delegateQueue
  , delegateQueueSelector
  , delegateSelector
  , initWithOutputSettingsSelector
  , initWithPixelBufferAttributesSelector
  , requestNotificationOfMediaDataChangeWithAdvanceIntervalSelector
  , setDelegate_queueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithPixelBufferAttributes:
--
-- Returns an instance of AVPlayerItemVideoOutput, initialized with the specified pixel buffer attributes, for video image output.
--
-- @pixelBufferAttributes@ — The client requirements for output CVPixelBuffers, expressed using the constants in <CoreVideo/CVPixelBuffer.h>.
--
-- Returns: An instance of AVPlayerItemVideoOutput.
--
-- ObjC selector: @- initWithPixelBufferAttributes:@
initWithPixelBufferAttributes :: (IsAVPlayerItemVideoOutput avPlayerItemVideoOutput, IsNSDictionary pixelBufferAttributes) => avPlayerItemVideoOutput -> pixelBufferAttributes -> IO (Id AVPlayerItemVideoOutput)
initWithPixelBufferAttributes avPlayerItemVideoOutput pixelBufferAttributes =
  sendOwnedMessage avPlayerItemVideoOutput initWithPixelBufferAttributesSelector (toNSDictionary pixelBufferAttributes)

-- | initWithOutputSettings:
--
-- Returns an instance of AVPlayerItemVideoOutput, initialized with the specified output settings, for video image output.
--
-- @outputSettings@ — The client requirements for output CVPixelBuffers, expressed using the constants in AVVideoSettings.h.
--
-- For uncompressed video output, start with kCVPixelBuffer* keys in <CoreVideo/CVPixelBuffer.h>.
--
-- In addition to the keys in CVPixelBuffer.h, uncompressed video settings dictionaries may also contain the following keys:
--
-- AVVideoAllowWideColorKey
--
-- Returns: An instance of AVPlayerItemVideoOutput.
--
-- This method throws an exception for any of the following reasons:					- the output settings dictionary is empty					- the settings will yield compressed output					- the settings do not honor the requirements listed above for outputSettings
--
-- ObjC selector: @- initWithOutputSettings:@
initWithOutputSettings :: (IsAVPlayerItemVideoOutput avPlayerItemVideoOutput, IsNSDictionary outputSettings) => avPlayerItemVideoOutput -> outputSettings -> IO (Id AVPlayerItemVideoOutput)
initWithOutputSettings avPlayerItemVideoOutput outputSettings =
  sendOwnedMessage avPlayerItemVideoOutput initWithOutputSettingsSelector (toNSDictionary outputSettings)

-- | setDelegate:queue:
--
-- Sets the receiver's delegate and a dispatch queue on which the delegate will be called.
--
-- @delegate@ — An object conforming to AVPlayerItemOutputPullDelegate protocol.
--
-- @delegateQueue@ — A dispatch queue on which all delegate methods will be called.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVPlayerItemVideoOutput avPlayerItemVideoOutput, IsNSObject delegateQueue) => avPlayerItemVideoOutput -> RawId -> delegateQueue -> IO ()
setDelegate_queue avPlayerItemVideoOutput delegate delegateQueue =
  sendMessage avPlayerItemVideoOutput setDelegate_queueSelector delegate (toNSObject delegateQueue)

-- | requestNotificationOfMediaDataChangeWithAdvanceInterval:
--
-- Informs the receiver that the AVPlayerItemVideoOutput client is entering a quiescent state.
--
-- @interval@ — A wall clock time interval.
--
-- Message this method before you suspend your use of a CADisplayLink. The interval you provide will be used to message your delegate, in advance, that it should resume the display link. If the interval you provide is large, effectively requesting wakeup earlier than the AVPlayerItemVideoOutput is prepared to act, the delegate will be invoked as soon as possible. Do not use this method to force a delegate invocation for each sample.
--
-- ObjC selector: @- requestNotificationOfMediaDataChangeWithAdvanceInterval:@
requestNotificationOfMediaDataChangeWithAdvanceInterval :: IsAVPlayerItemVideoOutput avPlayerItemVideoOutput => avPlayerItemVideoOutput -> CDouble -> IO ()
requestNotificationOfMediaDataChangeWithAdvanceInterval avPlayerItemVideoOutput interval =
  sendMessage avPlayerItemVideoOutput requestNotificationOfMediaDataChangeWithAdvanceIntervalSelector interval

-- | delegate
--
-- The receiver's delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsAVPlayerItemVideoOutput avPlayerItemVideoOutput => avPlayerItemVideoOutput -> IO RawId
delegate avPlayerItemVideoOutput =
  sendMessage avPlayerItemVideoOutput delegateSelector

-- | delegateQueue
--
-- The dispatch queue where the delegate is messaged.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemVideoOutput avPlayerItemVideoOutput => avPlayerItemVideoOutput -> IO (Id NSObject)
delegateQueue avPlayerItemVideoOutput =
  sendMessage avPlayerItemVideoOutput delegateQueueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPixelBufferAttributes:@
initWithPixelBufferAttributesSelector :: Selector '[Id NSDictionary] (Id AVPlayerItemVideoOutput)
initWithPixelBufferAttributesSelector = mkSelector "initWithPixelBufferAttributes:"

-- | @Selector@ for @initWithOutputSettings:@
initWithOutputSettingsSelector :: Selector '[Id NSDictionary] (Id AVPlayerItemVideoOutput)
initWithOutputSettingsSelector = mkSelector "initWithOutputSettings:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @requestNotificationOfMediaDataChangeWithAdvanceInterval:@
requestNotificationOfMediaDataChangeWithAdvanceIntervalSelector :: Selector '[CDouble] ()
requestNotificationOfMediaDataChangeWithAdvanceIntervalSelector = mkSelector "requestNotificationOfMediaDataChangeWithAdvanceInterval:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector '[] (Id NSObject)
delegateQueueSelector = mkSelector "delegateQueue"

