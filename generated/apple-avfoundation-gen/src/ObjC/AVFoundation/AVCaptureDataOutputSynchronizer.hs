{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDataOutputSynchronizer
--
-- AVCaptureDataOutputSynchronizer synchronizes the delivery of data from multiple capture data outputs (AVCaptureVideoDataOutput, AVCaptureDepthDataOutput, AVCaptureMetadataOutput, AVCaptureAudioDataOutput) to a single delegate callback.
--
-- AVCaptureDataOutputSynchronizer is initialized with an array of data outputs (AVCaptureVideoDataOutput, AVCaptureDepthDataOutput, AVCaptureMetadataOutput, or AVCaptureAudioDataOutput) from which you'd like to receive a single, synchronized delegate callback. The first output in the array acts as the primary data output and determines when the synchronized callback is delivered. When data is received for the primary data output, it is held until all other data outputs have received data with an equal or later presentation time stamp, or it has been determined that there is no data for a particular output at the primary data output's pts. Once all other outputs are ready, a single delegate callback is sent with all the data aligned with the primary data output's data. Separate delegate callbacks are sent for any other data received with presentation time stamps earlier than the next primary data output time.
--
-- For instance, if you specify a video data output as your first (primary) output and a metadata output for detected faces as your second output, your data callback will not be called until there is face data ready for a video frame, or it is assured that there is no face metadata for that particular video frame.
--
-- Note that the AVCaptureDataOutputSynchronizer overrides each data output's -setSampleBufferDelegate:queue:, -setDepthDataDelegate:queue:, or -setMetadataObjectsDelegate:queue: method call. -[AVCaptureVideoDataOutput alwaysDiscardsLateVideoFrames] and -[AVCaptureDepthDataOutput alwaysDiscardsLateDepthData] properties are honored.
--
-- Generated bindings for @AVCaptureDataOutputSynchronizer@.
module ObjC.AVFoundation.AVCaptureDataOutputSynchronizer
  ( AVCaptureDataOutputSynchronizer
  , IsAVCaptureDataOutputSynchronizer(..)
  , init_
  , new
  , initWithDataOutputs
  , setDelegate_queue
  , dataOutputs
  , delegate
  , delegateCallbackQueue
  , dataOutputsSelector
  , delegateCallbackQueueSelector
  , delegateSelector
  , initSelector
  , initWithDataOutputsSelector
  , newSelector
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

-- | @- init@
init_ :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO (Id AVCaptureDataOutputSynchronizer)
init_ avCaptureDataOutputSynchronizer =
  sendOwnedMessage avCaptureDataOutputSynchronizer initSelector

-- | @+ new@
new :: IO (Id AVCaptureDataOutputSynchronizer)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDataOutputSynchronizer"
    sendOwnedClassMessage cls' newSelector

-- | initWithDataOutputs:
--
-- Instantiates an AVCaptureDataOutputSynchronizer from one or more capture data outputs.
--
-- @dataOutputs@ — An array of capture data outputs where the first is the primary output.
--
-- Returns: A newly initialized AVCaptureDataOutputSynchronizer instance.
--
-- ObjC selector: @- initWithDataOutputs:@
initWithDataOutputs :: (IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer, IsNSArray dataOutputs) => avCaptureDataOutputSynchronizer -> dataOutputs -> IO (Id AVCaptureDataOutputSynchronizer)
initWithDataOutputs avCaptureDataOutputSynchronizer dataOutputs =
  sendOwnedMessage avCaptureDataOutputSynchronizer initWithDataOutputsSelector (toNSArray dataOutputs)

-- | setDelegate:queue:
--
-- Sets the receiver's delegate that will accept synchronized data and the dispatch queue on which the delegate will be called.
--
-- @delegate@ — An object conforming to the AVCaptureDataOutputSynchronizerDelegate protocol that will receive synchronized data from the provided data outputs.
--
-- @delegateCallbackQueue@ — A dispatch queue on which all AVCaptureDataOutputSynchronizerDelegate methods will be called.
--
-- AVCaptureDataOutputSynchronizer gathers data from its dataOutputs, and when it determines that all data has been received for a given timestamp, it calls the specified delegate on the specified delegateCallbackQueue. AVCaptureDataOutputSynchronizer overrides all the data outputs' delegates and callbacks. Data outputs under the control of AVCaptureDataOutputSynchronizer do not fire delegate callbacks. Delegate callbacks are restored to individual data outputs when you call this method with nil as your delegate and NULL as your delegateCallbackQueue.
--
-- A serial dispatch queue must be used to guarantee that synchronized data will be delivered in order. The delegateCallbackQueue parameter may not be NULL, except when setting the delegate to nil otherwise -setDelegate:queue: throws an NSInvalidArgumentException.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer, IsNSObject delegateCallbackQueue) => avCaptureDataOutputSynchronizer -> RawId -> delegateCallbackQueue -> IO ()
setDelegate_queue avCaptureDataOutputSynchronizer delegate delegateCallbackQueue =
  sendMessage avCaptureDataOutputSynchronizer setDelegate_queueSelector delegate (toNSObject delegateCallbackQueue)

-- | dataOutputs
--
-- The data outputs provided in the initializer method.
--
-- ObjC selector: @- dataOutputs@
dataOutputs :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO (Id NSArray)
dataOutputs avCaptureDataOutputSynchronizer =
  sendMessage avCaptureDataOutputSynchronizer dataOutputsSelector

-- | delegate
--
-- The receiver's delegate.
--
-- The value of this property is an object conforming to the AVCaptureDataOutputSynchronizerDelegate protocol that will receive synchronized data output. The delegate is set using the -setDelegate:queue: method. This property is key-value observable.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO RawId
delegate avCaptureDataOutputSynchronizer =
  sendMessage avCaptureDataOutputSynchronizer delegateSelector

-- | delegateCallbackQueue
--
-- The dispatch queue on which all AVCaptureDataOutputSynchronizerDelegate methods will be called.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the -setDelegate:queue: method.
--
-- ObjC selector: @- delegateCallbackQueue@
delegateCallbackQueue :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO (Id NSObject)
delegateCallbackQueue avCaptureDataOutputSynchronizer =
  sendMessage avCaptureDataOutputSynchronizer delegateCallbackQueueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureDataOutputSynchronizer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureDataOutputSynchronizer)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDataOutputs:@
initWithDataOutputsSelector :: Selector '[Id NSArray] (Id AVCaptureDataOutputSynchronizer)
initWithDataOutputsSelector = mkSelector "initWithDataOutputs:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @dataOutputs@
dataOutputsSelector :: Selector '[] (Id NSArray)
dataOutputsSelector = mkSelector "dataOutputs"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateCallbackQueue@
delegateCallbackQueueSelector :: Selector '[] (Id NSObject)
delegateCallbackQueueSelector = mkSelector "delegateCallbackQueue"

