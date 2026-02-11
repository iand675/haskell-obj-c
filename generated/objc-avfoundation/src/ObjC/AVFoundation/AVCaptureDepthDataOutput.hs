{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDepthDataOutput
--
-- AVCaptureDepthDataOutput is a concrete subclass of AVCaptureOutput that can be used to process depth data in a streaming fashion.
--
-- Instances of AVCaptureDepthDataOutput capture AVDepthData objects expressing disparity/depth. Applications can access the frames with the depthDataOutput:didOutputDepthData:fromConnection: delegate method.
--
-- AVCaptureDepthDataOutput always provides depth data in the format expressed by its source's -[AVCaptureDevice activeDepthDataFormat] property. If you wish to receive depth data in another format, you may choose from the -[AVCaptureDevice activeFormat]'s -[AVCaptureDeviceFormat supportedDepthDataFormats], and set it using -[AVCaptureDevice setActiveDepthDataFormat:].
--
-- Generated bindings for @AVCaptureDepthDataOutput@.
module ObjC.AVFoundation.AVCaptureDepthDataOutput
  ( AVCaptureDepthDataOutput
  , IsAVCaptureDepthDataOutput(..)
  , init_
  , new
  , setDelegate_callbackQueue
  , delegateCallbackQueue
  , alwaysDiscardsLateDepthData
  , setAlwaysDiscardsLateDepthData
  , filteringEnabled
  , setFilteringEnabled
  , initSelector
  , newSelector
  , setDelegate_callbackQueueSelector
  , delegateCallbackQueueSelector
  , alwaysDiscardsLateDepthDataSelector
  , setAlwaysDiscardsLateDepthDataSelector
  , filteringEnabledSelector
  , setFilteringEnabledSelector


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
init_ :: IsAVCaptureDepthDataOutput avCaptureDepthDataOutput => avCaptureDepthDataOutput -> IO (Id AVCaptureDepthDataOutput)
init_ avCaptureDepthDataOutput  =
  sendMsg avCaptureDepthDataOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureDepthDataOutput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDepthDataOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | setDelegate:queue:
--
-- Sets the receiver's delegate that receives captured depth data and the dispatch queue on which the delegate is called.
--
-- @delegate@ — An object conforming to the AVCaptureDepthDataOutputDelegate protocol that receives depth data in a streaming fashion.
--
-- @callbackQueue@ — A dispatch queue on which all delegate methods are called.
--
-- The depth data output vends captured depth data to its delegate using the methods specified in the AVCaptureDepthOutputDelegate protocol. All delegate methods are called on the specified dispatch queue. If the callback queue is blocked when new depth data is captured, that depth data is automatically dropped at a time determined by the value of the alwaysDiscardsLateDepthData property. This allows clients to process existing depth data on the same queue without having to manage the potential memory usage increases that would otherwise occur when that processing is unable to keep up with the rate of incoming depth data.
--
-- Clients who need to minimize the chances of depth data being dropped should provide a dedicated queue and not share it with other data outputs. Processing of depth data may be deferred to another queue, but beware that the depth data pixel buffer maps may come from a finite buffer pool, which may be starved if your deferred processing fails to keep up.
--
-- A serial dispatch queue must be used to guarantee that depth data will be delivered in order. The callbackQueue parameter may not be NULL, except when setting the delegate to nil otherwise -setDelegate:callbackQueue: throws an NSInvalidArgumentException.
--
-- ObjC selector: @- setDelegate:callbackQueue:@
setDelegate_callbackQueue :: (IsAVCaptureDepthDataOutput avCaptureDepthDataOutput, IsNSObject callbackQueue) => avCaptureDepthDataOutput -> RawId -> callbackQueue -> IO ()
setDelegate_callbackQueue avCaptureDepthDataOutput  delegate callbackQueue =
withObjCPtr callbackQueue $ \raw_callbackQueue ->
    sendMsg avCaptureDepthDataOutput (mkSelector "setDelegate:callbackQueue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_callbackQueue :: Ptr ())]

-- | delegateCallbackQueue
--
-- The dispatch queue on which all delegate methods are called.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the setDelegate:queue: method.
--
-- ObjC selector: @- delegateCallbackQueue@
delegateCallbackQueue :: IsAVCaptureDepthDataOutput avCaptureDepthDataOutput => avCaptureDepthDataOutput -> IO (Id NSObject)
delegateCallbackQueue avCaptureDepthDataOutput  =
  sendMsg avCaptureDepthDataOutput (mkSelector "delegateCallbackQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | alwaysDiscardsLateDepthData
--
-- Specifies whether the receiver should always discard any depth data that is not processed before the next depth data is captured.
--
-- When the value of this property is YES, the receiver will immediately discard depth data that are captured while the delegateCallbackQueue is blocked. When the value of this property is NO, delegates will be allowed more time to process old depth data before new depth data are discarded, but application memory usage may increase as a result. The default value is YES.
--
-- ObjC selector: @- alwaysDiscardsLateDepthData@
alwaysDiscardsLateDepthData :: IsAVCaptureDepthDataOutput avCaptureDepthDataOutput => avCaptureDepthDataOutput -> IO Bool
alwaysDiscardsLateDepthData avCaptureDepthDataOutput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDepthDataOutput (mkSelector "alwaysDiscardsLateDepthData") retCULong []

-- | alwaysDiscardsLateDepthData
--
-- Specifies whether the receiver should always discard any depth data that is not processed before the next depth data is captured.
--
-- When the value of this property is YES, the receiver will immediately discard depth data that are captured while the delegateCallbackQueue is blocked. When the value of this property is NO, delegates will be allowed more time to process old depth data before new depth data are discarded, but application memory usage may increase as a result. The default value is YES.
--
-- ObjC selector: @- setAlwaysDiscardsLateDepthData:@
setAlwaysDiscardsLateDepthData :: IsAVCaptureDepthDataOutput avCaptureDepthDataOutput => avCaptureDepthDataOutput -> Bool -> IO ()
setAlwaysDiscardsLateDepthData avCaptureDepthDataOutput  value =
  sendMsg avCaptureDepthDataOutput (mkSelector "setAlwaysDiscardsLateDepthData:") retVoid [argCULong (if value then 1 else 0)]

-- | filteringEnabled
--
-- Specifies whether the depth data output should filter depth data to smooth out noise and fill invalid values.
--
-- When the value of this property is YES, the receiver temporally filters the stream of AVDepthData objects to reduce noise, as well as fill invalid values. Invalid values (NaN) may be present in AVDepthData pixel buffer maps due to factors such as low light or lens occlusion. When filtering is enabled, the depth data output interpolates missing depth data values. Filtering should be disabled if you desire the raw depth data values. The default value is YES.
--
-- ObjC selector: @- filteringEnabled@
filteringEnabled :: IsAVCaptureDepthDataOutput avCaptureDepthDataOutput => avCaptureDepthDataOutput -> IO Bool
filteringEnabled avCaptureDepthDataOutput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureDepthDataOutput (mkSelector "filteringEnabled") retCULong []

-- | filteringEnabled
--
-- Specifies whether the depth data output should filter depth data to smooth out noise and fill invalid values.
--
-- When the value of this property is YES, the receiver temporally filters the stream of AVDepthData objects to reduce noise, as well as fill invalid values. Invalid values (NaN) may be present in AVDepthData pixel buffer maps due to factors such as low light or lens occlusion. When filtering is enabled, the depth data output interpolates missing depth data values. Filtering should be disabled if you desire the raw depth data values. The default value is YES.
--
-- ObjC selector: @- setFilteringEnabled:@
setFilteringEnabled :: IsAVCaptureDepthDataOutput avCaptureDepthDataOutput => avCaptureDepthDataOutput -> Bool -> IO ()
setFilteringEnabled avCaptureDepthDataOutput  value =
  sendMsg avCaptureDepthDataOutput (mkSelector "setFilteringEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @setDelegate:callbackQueue:@
setDelegate_callbackQueueSelector :: Selector
setDelegate_callbackQueueSelector = mkSelector "setDelegate:callbackQueue:"

-- | @Selector@ for @delegateCallbackQueue@
delegateCallbackQueueSelector :: Selector
delegateCallbackQueueSelector = mkSelector "delegateCallbackQueue"

-- | @Selector@ for @alwaysDiscardsLateDepthData@
alwaysDiscardsLateDepthDataSelector :: Selector
alwaysDiscardsLateDepthDataSelector = mkSelector "alwaysDiscardsLateDepthData"

-- | @Selector@ for @setAlwaysDiscardsLateDepthData:@
setAlwaysDiscardsLateDepthDataSelector :: Selector
setAlwaysDiscardsLateDepthDataSelector = mkSelector "setAlwaysDiscardsLateDepthData:"

-- | @Selector@ for @filteringEnabled@
filteringEnabledSelector :: Selector
filteringEnabledSelector = mkSelector "filteringEnabled"

-- | @Selector@ for @setFilteringEnabled:@
setFilteringEnabledSelector :: Selector
setFilteringEnabledSelector = mkSelector "setFilteringEnabled:"

