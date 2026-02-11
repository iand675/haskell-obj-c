{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generates and synchronizes timecode data from various sources for precise video and audio synchronization.
--
-- The ``AVCaptureTimecodeGenerator`` class supports multiple timecode sources, including frame counting, system clock synchronization, and MIDI timecode input (MTC). Suitable for playback, recording, or other time-sensitive operations where precise timecode metadata is required.
--
-- Use the ``startSynchronizationWithTimecodeSource:`` method to set up the desired timecode source.
--
-- Generated bindings for @AVCaptureTimecodeGenerator@.
module ObjC.AVFoundation.AVCaptureTimecodeGenerator
  ( AVCaptureTimecodeGenerator
  , IsAVCaptureTimecodeGenerator(..)
  , setDelegate_queue
  , startSynchronizationWithTimecodeSource
  , availableSources
  , currentSource
  , delegate
  , delegateCallbackQueue
  , synchronizationTimeout
  , setSynchronizationTimeout
  , timecodeAlignmentOffset
  , setTimecodeAlignmentOffset
  , frameCountSource
  , realTimeClockSource
  , setDelegate_queueSelector
  , startSynchronizationWithTimecodeSourceSelector
  , availableSourcesSelector
  , currentSourceSelector
  , delegateSelector
  , delegateCallbackQueueSelector
  , synchronizationTimeoutSelector
  , setSynchronizationTimeoutSelector
  , timecodeAlignmentOffsetSelector
  , setTimecodeAlignmentOffsetSelector
  , frameCountSourceSelector
  , realTimeClockSourceSelector


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

-- | Assigns a delegate to receive real-time timecode updates and specifies a queue for callbacks.
--
-- - Parameter delegate: An object conforming to the ``AVCaptureTimecodeGeneratorDelegate`` protocol. - Parameter callbackQueue: The dispatch queue on which the delegate methods are invoked. The @callbackQueue@ parameter may not be @nil@, except when setting the ``AVCaptureTimecodeGeneratorDelegate`` to @nil@, otherwise ``setDelegate:queue:`` throws an @NSInvalidArgumentException@.
--
-- Use this method to configure a delegate that handles timecode updates. The specified @queue@ ensures thread-safe invocation of delegate methods.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator, IsNSObject callbackQueue) => avCaptureTimecodeGenerator -> RawId -> callbackQueue -> IO ()
setDelegate_queue avCaptureTimecodeGenerator  delegate callbackQueue =
  withObjCPtr callbackQueue $ \raw_callbackQueue ->
      sendMsg avCaptureTimecodeGenerator (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_callbackQueue :: Ptr ())]

-- | Synchronizes the generator with the specified timecode source.
--
-- - Parameter source: The timecode source for synchronization.
--
-- ObjC selector: @- startSynchronizationWithTimecodeSource:@
startSynchronizationWithTimecodeSource :: (IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator, IsAVCaptureTimecodeSource source) => avCaptureTimecodeGenerator -> source -> IO ()
startSynchronizationWithTimecodeSource avCaptureTimecodeGenerator  source =
  withObjCPtr source $ \raw_source ->
      sendMsg avCaptureTimecodeGenerator (mkSelector "startSynchronizationWithTimecodeSource:") retVoid [argPtr (castPtr raw_source :: Ptr ())]

-- | An array of available timecode synchronization sources that can be used by the timecode generator.
--
-- This property provides a list of ``AVCaptureTimecodeSource`` objects representing the available timecode sources with which the generator can synchronize. The sources may include built-in options such as the frame counter and real-time clock, as well as dynamically detected sources such as connected MIDI or HID devices.
--
-- This array is key-value observable, allowing you to monitor changes in real-time. For example, when a new MIDI device is connected, the array is updated to include the corresponding timecode source.
--
-- - Returns: A read-only array of ``AVCaptureTimecodeSource`` objects representing the available timecode synchronization sources.
--
-- ObjC selector: @- availableSources@
availableSources :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> IO (Id NSArray)
availableSources avCaptureTimecodeGenerator  =
    sendMsg avCaptureTimecodeGenerator (mkSelector "availableSources") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The active timecode source used by ``AVCaptureTimecodeGenerator`` to maintain clock synchronization for accurate timecode generation.
--
-- Indicates the active timecode source, as defined in the ``AVCaptureTimecodeSynchronizationSourceType`` enum. If an ``AVCaptureTimecodeGenerator`` becomes disconnected from its source, it continues generating timecodes using historical data from its ring buffer. This approach allows the generator to maintain synchronization during brief disruptions, as is common in cinema workflows where timecode signals may experience discontinuities.
--
-- ObjC selector: @- currentSource@
currentSource :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> IO (Id AVCaptureTimecodeSource)
currentSource avCaptureTimecodeGenerator  =
    sendMsg avCaptureTimecodeGenerator (mkSelector "currentSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The delegate that receives timecode updates from the timecode generator.
--
-- You can use your ``delegate`` to receive real-time timecode updates. Implement the ``timecodeGenerator:didReceiveUpdate:`` method in your delegate to handle updates.
--
-- ObjC selector: @- delegate@
delegate :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> IO RawId
delegate avCaptureTimecodeGenerator  =
    fmap (RawId . castPtr) $ sendMsg avCaptureTimecodeGenerator (mkSelector "delegate") (retPtr retVoid) []

-- | The dispatch queue on which delegate callbacks are invoked.
--
-- Provides the queue set in ``setDelegate:queue:``. If no delegate is assigned, this property is @nil@.
--
-- ObjC selector: @- delegateCallbackQueue@
delegateCallbackQueue :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> IO (Id NSObject)
delegateCallbackQueue avCaptureTimecodeGenerator  =
    sendMsg avCaptureTimecodeGenerator (mkSelector "delegateCallbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The maximum time interval allowed for source synchronization attempts before timing out.
--
-- This property specifies the duration, in seconds, that the ``AVCaptureTimecodeGenerator`` will attempt to synchronize with a timecode source before timing out if synchronization cannot be achieved. If this threshold is exceeded, the synchronization status updates to reflect a timeout, and your ``AVCaptureTimecodeGeneratorDelegate/timecodeGenerator:transitionedToSynchronizationStatus:forSource:`` delegate method fires, informing you of the event. The default value is 15 seconds.
--
-- ObjC selector: @- synchronizationTimeout@
synchronizationTimeout :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> IO CDouble
synchronizationTimeout avCaptureTimecodeGenerator  =
    sendMsg avCaptureTimecodeGenerator (mkSelector "synchronizationTimeout") retCDouble []

-- | The maximum time interval allowed for source synchronization attempts before timing out.
--
-- This property specifies the duration, in seconds, that the ``AVCaptureTimecodeGenerator`` will attempt to synchronize with a timecode source before timing out if synchronization cannot be achieved. If this threshold is exceeded, the synchronization status updates to reflect a timeout, and your ``AVCaptureTimecodeGeneratorDelegate/timecodeGenerator:transitionedToSynchronizationStatus:forSource:`` delegate method fires, informing you of the event. The default value is 15 seconds.
--
-- ObjC selector: @- setSynchronizationTimeout:@
setSynchronizationTimeout :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> CDouble -> IO ()
setSynchronizationTimeout avCaptureTimecodeGenerator  value =
    sendMsg avCaptureTimecodeGenerator (mkSelector "setSynchronizationTimeout:") retVoid [argCDouble value]

-- | The time offset, in seconds, applied to the generated timecode.
--
-- This offset allows fine-tuning of time alignment for synchronization with external sources or to accommodate any intentional delay. The default value is 0 seconds.
--
-- ObjC selector: @- timecodeAlignmentOffset@
timecodeAlignmentOffset :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> IO CDouble
timecodeAlignmentOffset avCaptureTimecodeGenerator  =
    sendMsg avCaptureTimecodeGenerator (mkSelector "timecodeAlignmentOffset") retCDouble []

-- | The time offset, in seconds, applied to the generated timecode.
--
-- This offset allows fine-tuning of time alignment for synchronization with external sources or to accommodate any intentional delay. The default value is 0 seconds.
--
-- ObjC selector: @- setTimecodeAlignmentOffset:@
setTimecodeAlignmentOffset :: IsAVCaptureTimecodeGenerator avCaptureTimecodeGenerator => avCaptureTimecodeGenerator -> CDouble -> IO ()
setTimecodeAlignmentOffset avCaptureTimecodeGenerator  value =
    sendMsg avCaptureTimecodeGenerator (mkSelector "setTimecodeAlignmentOffset:") retVoid [argCDouble value]

-- | A frame counter timecode source that operates independently of any internal or external synchronization.
--
-- This class property represents a standalone timecode source that advances based purely on frame count, independent of any real-time or external synchronization. It is ideal for scenarios where a simple, self-contained timing reference is sufficient, without requiring alignment to system clocks or external devices.
--
-- ObjC selector: @+ frameCountSource@
frameCountSource :: IO (Id AVCaptureTimecodeSource)
frameCountSource  =
  do
    cls' <- getRequiredClass "AVCaptureTimecodeGenerator"
    sendClassMsg cls' (mkSelector "frameCountSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A predefined timecode source synchronized to the real-time system clock.
--
-- This class property provides a default timecode source based on the real-time system clock, requiring no external device. It is ideal for live events or scenarios where alignment with the current time of day is necessary.
--
-- ObjC selector: @+ realTimeClockSource@
realTimeClockSource :: IO (Id AVCaptureTimecodeSource)
realTimeClockSource  =
  do
    cls' <- getRequiredClass "AVCaptureTimecodeGenerator"
    sendClassMsg cls' (mkSelector "realTimeClockSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @startSynchronizationWithTimecodeSource:@
startSynchronizationWithTimecodeSourceSelector :: Selector
startSynchronizationWithTimecodeSourceSelector = mkSelector "startSynchronizationWithTimecodeSource:"

-- | @Selector@ for @availableSources@
availableSourcesSelector :: Selector
availableSourcesSelector = mkSelector "availableSources"

-- | @Selector@ for @currentSource@
currentSourceSelector :: Selector
currentSourceSelector = mkSelector "currentSource"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateCallbackQueue@
delegateCallbackQueueSelector :: Selector
delegateCallbackQueueSelector = mkSelector "delegateCallbackQueue"

-- | @Selector@ for @synchronizationTimeout@
synchronizationTimeoutSelector :: Selector
synchronizationTimeoutSelector = mkSelector "synchronizationTimeout"

-- | @Selector@ for @setSynchronizationTimeout:@
setSynchronizationTimeoutSelector :: Selector
setSynchronizationTimeoutSelector = mkSelector "setSynchronizationTimeout:"

-- | @Selector@ for @timecodeAlignmentOffset@
timecodeAlignmentOffsetSelector :: Selector
timecodeAlignmentOffsetSelector = mkSelector "timecodeAlignmentOffset"

-- | @Selector@ for @setTimecodeAlignmentOffset:@
setTimecodeAlignmentOffsetSelector :: Selector
setTimecodeAlignmentOffsetSelector = mkSelector "setTimecodeAlignmentOffset:"

-- | @Selector@ for @frameCountSource@
frameCountSourceSelector :: Selector
frameCountSourceSelector = mkSelector "frameCountSource"

-- | @Selector@ for @realTimeClockSource@
realTimeClockSourceSelector :: Selector
realTimeClockSourceSelector = mkSelector "realTimeClockSource"

