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
  , initSelector
  , newSelector
  , initWithDataOutputsSelector
  , setDelegate_queueSelector
  , dataOutputsSelector
  , delegateSelector
  , delegateCallbackQueueSelector


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
init_ :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO (Id AVCaptureDataOutputSynchronizer)
init_ avCaptureDataOutputSynchronizer  =
    sendMsg avCaptureDataOutputSynchronizer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureDataOutputSynchronizer)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDataOutputSynchronizer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithDataOutputs avCaptureDataOutputSynchronizer  dataOutputs =
  withObjCPtr dataOutputs $ \raw_dataOutputs ->
      sendMsg avCaptureDataOutputSynchronizer (mkSelector "initWithDataOutputs:") (retPtr retVoid) [argPtr (castPtr raw_dataOutputs :: Ptr ())] >>= ownedObject . castPtr

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
setDelegate_queue avCaptureDataOutputSynchronizer  delegate delegateCallbackQueue =
  withObjCPtr delegateCallbackQueue $ \raw_delegateCallbackQueue ->
      sendMsg avCaptureDataOutputSynchronizer (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateCallbackQueue :: Ptr ())]

-- | dataOutputs
--
-- The data outputs provided in the initializer method.
--
-- ObjC selector: @- dataOutputs@
dataOutputs :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO (Id NSArray)
dataOutputs avCaptureDataOutputSynchronizer  =
    sendMsg avCaptureDataOutputSynchronizer (mkSelector "dataOutputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | delegate
--
-- The receiver's delegate.
--
-- The value of this property is an object conforming to the AVCaptureDataOutputSynchronizerDelegate protocol that will receive synchronized data output. The delegate is set using the -setDelegate:queue: method. This property is key-value observable.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO RawId
delegate avCaptureDataOutputSynchronizer  =
    fmap (RawId . castPtr) $ sendMsg avCaptureDataOutputSynchronizer (mkSelector "delegate") (retPtr retVoid) []

-- | delegateCallbackQueue
--
-- The dispatch queue on which all AVCaptureDataOutputSynchronizerDelegate methods will be called.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the -setDelegate:queue: method.
--
-- ObjC selector: @- delegateCallbackQueue@
delegateCallbackQueue :: IsAVCaptureDataOutputSynchronizer avCaptureDataOutputSynchronizer => avCaptureDataOutputSynchronizer -> IO (Id NSObject)
delegateCallbackQueue avCaptureDataOutputSynchronizer  =
    sendMsg avCaptureDataOutputSynchronizer (mkSelector "delegateCallbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDataOutputs:@
initWithDataOutputsSelector :: Selector
initWithDataOutputsSelector = mkSelector "initWithDataOutputs:"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @dataOutputs@
dataOutputsSelector :: Selector
dataOutputsSelector = mkSelector "dataOutputs"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateCallbackQueue@
delegateCallbackQueueSelector :: Selector
delegateCallbackQueueSelector = mkSelector "delegateCallbackQueue"

