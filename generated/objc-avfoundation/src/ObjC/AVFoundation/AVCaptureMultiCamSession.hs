{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureMultiCamSession
--
-- A subclass of AVCaptureSession which supports simultaneous capture from multiple inputs of the same media type.
--
-- AVCaptureMultiCamSession's sessionPreset is always AVCaptureSessionPresetInputPriority and may not be set to any other value. Each input's device.activeFormat must be set manually to achieve the desired quality of service.
--
-- AVCaptureMultiCamSession supports dynamic enabling and disabling of individual camera inputs without interrupting preview. In order to stop an individual camera input, set the enabled property on all of its connections or connected ports to NO. When the last active connection or port is disabled, the source camera stops streaming to save power and bandwidth. Other inputs streaming data through the session are unaffected.
--
-- Prior to iOS 26, AVCaptureMultiCamSession requires all input devices to have an activeFormat where multiCamSupported returns YES. In applications linked on or after iOS 26, this requirement is not enforced when only a single input device is used.
--
-- Generated bindings for @AVCaptureMultiCamSession@.
module ObjC.AVFoundation.AVCaptureMultiCamSession
  ( AVCaptureMultiCamSession
  , IsAVCaptureMultiCamSession(..)
  , multiCamSupported
  , hardwareCost
  , systemPressureCost
  , multiCamSupportedSelector
  , hardwareCostSelector
  , systemPressureCostSelector


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

-- | multiCamSupported
--
-- Indicates whether multicam session is supported on this platform.
--
-- AVCaptureMultiCamSession is intended to be used with multiple cameras and is only supported on platforms with sufficient hardware bandwidth, system memory, and thermal performance. For single-camera use cases, AVCaptureSession should be used instead.
--
-- ObjC selector: @+ multiCamSupported@
multiCamSupported :: IO Bool
multiCamSupported  =
  do
    cls' <- getRequiredClass "AVCaptureMultiCamSession"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "multiCamSupported") retCULong []

-- | hardwareCost
--
-- Indicates the percentage of the session's available hardware budget currently in use.
--
-- The value of this property is a float from 0.0 => 1.0 indicating how much of the session's available hardware is in use as a percentage, given the currently connected inputs and outputs and the features for which you've opted in. When your hardwareCost is greater than 1.0, the capture session cannot run your desired configuration due to hardware constraints, so you receive an AVCaptureSessionRuntimeErrorNotification when attempting to start it running. Default value is 0.
--
-- Contributors to hardwareCost include:        - Whether the source devices' active formats use the full sensor (4:3) or a crop (16:9). Cropped formats require lower hardware bandwidth, and therefore lower the cost.        - The max frame rate supported by the source devices' active formats. The higher the max frame rate, the higher the cost.        - Whether the source devices' active formats are binned or not. Binned formats require substantially less hardware bandwidth, and therefore result in a lower cost.        - The number of sources configured to deliver streaming disparity / depth via AVCaptureDepthDataOutput. The higher the number of cameras configured to produce depth, the higher the cost.    In order to reduce hardwareCost, consider picking a sensor-cropped activeFormat, or a binned format. You may also use AVCaptureDeviceInput's videoMinFrameDurationOverride property to artificially limit the max frame rate (which is the reciprocal of the min frame duration) of a source device to a lower value. By doing so, you only pay the hardware cost for the max frame rate you intend to use.
--
-- ObjC selector: @- hardwareCost@
hardwareCost :: IsAVCaptureMultiCamSession avCaptureMultiCamSession => avCaptureMultiCamSession -> IO CFloat
hardwareCost avCaptureMultiCamSession  =
  sendMsg avCaptureMultiCamSession (mkSelector "hardwareCost") retCFloat []

-- | systemPressureCost
--
-- Indicates the system pressure cost of your current configuration.
--
-- The value of this property is a float whose nominal range is 0.0 => 1.0 indicating the system pressure cost of your current configuration. When your systemPressureCost is greater than 1.0, the capture session cannot run sustainably. It may be able to run for a brief period before needing to stop due to high system pressure. While running in an unsustainable configuration, you may monitor the session's systemPressureState and reduce pressure by reducing the frame rate, throttling your use of the GPU, etc. When the session reaches critical system pressure state, it must temporarily shut down, and you receive an AVCaptureSessionWasInterruptedNotification indicating the reason your session needed to stop. When system pressure alleviates, the session interruption ends.
--
-- ObjC selector: @- systemPressureCost@
systemPressureCost :: IsAVCaptureMultiCamSession avCaptureMultiCamSession => avCaptureMultiCamSession -> IO CFloat
systemPressureCost avCaptureMultiCamSession  =
  sendMsg avCaptureMultiCamSession (mkSelector "systemPressureCost") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @multiCamSupported@
multiCamSupportedSelector :: Selector
multiCamSupportedSelector = mkSelector "multiCamSupported"

-- | @Selector@ for @hardwareCost@
hardwareCostSelector :: Selector
hardwareCostSelector = mkSelector "hardwareCost"

-- | @Selector@ for @systemPressureCost@
systemPressureCostSelector :: Selector
systemPressureCostSelector = mkSelector "systemPressureCost"

