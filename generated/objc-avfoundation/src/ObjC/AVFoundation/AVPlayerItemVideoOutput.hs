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
  , delegateQueue
  , initWithPixelBufferAttributesSelector
  , initWithOutputSettingsSelector
  , setDelegate_queueSelector
  , requestNotificationOfMediaDataChangeWithAdvanceIntervalSelector
  , delegateQueueSelector


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
initWithPixelBufferAttributes avPlayerItemVideoOutput  pixelBufferAttributes =
withObjCPtr pixelBufferAttributes $ \raw_pixelBufferAttributes ->
    sendMsg avPlayerItemVideoOutput (mkSelector "initWithPixelBufferAttributes:") (retPtr retVoid) [argPtr (castPtr raw_pixelBufferAttributes :: Ptr ())] >>= ownedObject . castPtr

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
initWithOutputSettings avPlayerItemVideoOutput  outputSettings =
withObjCPtr outputSettings $ \raw_outputSettings ->
    sendMsg avPlayerItemVideoOutput (mkSelector "initWithOutputSettings:") (retPtr retVoid) [argPtr (castPtr raw_outputSettings :: Ptr ())] >>= ownedObject . castPtr

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
setDelegate_queue avPlayerItemVideoOutput  delegate delegateQueue =
withObjCPtr delegateQueue $ \raw_delegateQueue ->
    sendMsg avPlayerItemVideoOutput (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateQueue :: Ptr ())]

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
requestNotificationOfMediaDataChangeWithAdvanceInterval avPlayerItemVideoOutput  interval =
  sendMsg avPlayerItemVideoOutput (mkSelector "requestNotificationOfMediaDataChangeWithAdvanceInterval:") retVoid [argCDouble (fromIntegral interval)]

-- | delegateQueue
--
-- The dispatch queue where the delegate is messaged.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVPlayerItemVideoOutput avPlayerItemVideoOutput => avPlayerItemVideoOutput -> IO (Id NSObject)
delegateQueue avPlayerItemVideoOutput  =
  sendMsg avPlayerItemVideoOutput (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPixelBufferAttributes:@
initWithPixelBufferAttributesSelector :: Selector
initWithPixelBufferAttributesSelector = mkSelector "initWithPixelBufferAttributes:"

-- | @Selector@ for @initWithOutputSettings:@
initWithOutputSettingsSelector :: Selector
initWithOutputSettingsSelector = mkSelector "initWithOutputSettings:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @requestNotificationOfMediaDataChangeWithAdvanceInterval:@
requestNotificationOfMediaDataChangeWithAdvanceIntervalSelector :: Selector
requestNotificationOfMediaDataChangeWithAdvanceIntervalSelector = mkSelector "requestNotificationOfMediaDataChangeWithAdvanceInterval:"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

