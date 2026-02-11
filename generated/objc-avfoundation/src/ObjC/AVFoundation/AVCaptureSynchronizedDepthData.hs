{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSynchronizedDepthData
--
-- An concrete subclass of AVCaptureSynchronizedData representing the data delivered by an AVCaptureDepthDataOutput.
--
-- Depth data, like video, may be dropped if not serviced in a timely fashion.
--
-- Generated bindings for @AVCaptureSynchronizedDepthData@.
module ObjC.AVFoundation.AVCaptureSynchronizedDepthData
  ( AVCaptureSynchronizedDepthData
  , IsAVCaptureSynchronizedDepthData(..)
  , depthData
  , depthDataWasDropped
  , droppedReason
  , depthDataSelector
  , depthDataWasDroppedSelector
  , droppedReasonSelector

  -- * Enum types
  , AVCaptureOutputDataDroppedReason(AVCaptureOutputDataDroppedReason)
  , pattern AVCaptureOutputDataDroppedReasonNone
  , pattern AVCaptureOutputDataDroppedReasonLateData
  , pattern AVCaptureOutputDataDroppedReasonOutOfBuffers
  , pattern AVCaptureOutputDataDroppedReasonDiscontinuity

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | depthData
--
-- An instance of AVDepthData.
--
-- If depthDataWasDropped is YES, the returned depthData was dropped before it could be delivered to you, and thus this AVDepthData is a shell containing format information and calibration data, but no actual pixel map data. This property is never nil. If a data output has no data to return, it is simply not present in the dictionary of synchronized data returned by AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedData: delegate callback.
--
-- ObjC selector: @- depthData@
depthData :: IsAVCaptureSynchronizedDepthData avCaptureSynchronizedDepthData => avCaptureSynchronizedDepthData -> IO (Id AVDepthData)
depthData avCaptureSynchronizedDepthData  =
  sendMsg avCaptureSynchronizedDepthData (mkSelector "depthData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | depthDataWasDropped
--
-- YES if the depth data was dropped.
--
-- If YES, inspect -droppedReason for the reason.
--
-- ObjC selector: @- depthDataWasDropped@
depthDataWasDropped :: IsAVCaptureSynchronizedDepthData avCaptureSynchronizedDepthData => avCaptureSynchronizedDepthData -> IO Bool
depthDataWasDropped avCaptureSynchronizedDepthData  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureSynchronizedDepthData (mkSelector "depthDataWasDropped") retCULong []

-- | droppedReason
--
-- If depthDataWasDropped is YES, the reason for the drop, otherwise AVCaptureOutputDataDroppedReasonNone.
--
-- AVCaptureOutputDataDroppedReasons are defined in AVCaptureOutputBase.h.
--
-- ObjC selector: @- droppedReason@
droppedReason :: IsAVCaptureSynchronizedDepthData avCaptureSynchronizedDepthData => avCaptureSynchronizedDepthData -> IO AVCaptureOutputDataDroppedReason
droppedReason avCaptureSynchronizedDepthData  =
  fmap (coerce :: CLong -> AVCaptureOutputDataDroppedReason) $ sendMsg avCaptureSynchronizedDepthData (mkSelector "droppedReason") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @depthData@
depthDataSelector :: Selector
depthDataSelector = mkSelector "depthData"

-- | @Selector@ for @depthDataWasDropped@
depthDataWasDroppedSelector :: Selector
depthDataWasDroppedSelector = mkSelector "depthDataWasDropped"

-- | @Selector@ for @droppedReason@
droppedReasonSelector :: Selector
droppedReasonSelector = mkSelector "droppedReason"

