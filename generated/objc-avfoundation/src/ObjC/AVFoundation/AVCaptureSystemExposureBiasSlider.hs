{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSystemExposureBiasSlider
--
-- The system's recommended continuous exposure bias control for @-[AVCaptureDevice exposureTargetBias]@.
--
-- @AVCaptureSystemExposureBiasSlider@ uses the range specified by @systemRecommendedExposureBiasRange@ on the @activeFormat@ from the @AVCaptureDevice@ specified during initialization. As the device's @activeFormat@ changes, the slider updates its range with the new format's @systemRecommendedExposureBiasRange@.
--
-- Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
--
-- Generated bindings for @AVCaptureSystemExposureBiasSlider@.
module ObjC.AVFoundation.AVCaptureSystemExposureBiasSlider
  ( AVCaptureSystemExposureBiasSlider
  , IsAVCaptureSystemExposureBiasSlider(..)
  , initWithDevice
  , initWithDevice_action
  , initWithDeviceSelector
  , initWithDevice_actionSelector


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

-- | initWithDevice:
--
-- Initializes an @AVCaptureSystemExposureBiasSlider@ for controlling @device@.
--
-- @device@ — The device to control.
--
-- @AVCaptureSystemExposureBiasSlider@ may only be initialized with @AVCaptureDevice@ instances that support setting @exposureTargetBias@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: (IsAVCaptureSystemExposureBiasSlider avCaptureSystemExposureBiasSlider, IsAVCaptureDevice device) => avCaptureSystemExposureBiasSlider -> device -> IO (Id AVCaptureSystemExposureBiasSlider)
initWithDevice avCaptureSystemExposureBiasSlider  device =
withObjCPtr device $ \raw_device ->
    sendMsg avCaptureSystemExposureBiasSlider (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ())] >>= ownedObject . castPtr

-- | initWithDevice:action
--
-- Initializes an @AVCaptureSystemExposureBiasSlider@ for controlling @device@ with a `@ @action@ for handling @exposureTargetBias` changes.
--
-- @device@ — The device to control.
--
-- @action@ — An action called on `@ to handle @exposureTargetBias@ changes by @AVCaptureSystemExposureBiasSlider`.
--
-- @action@ is **only** called when @exposureTargetBias@ is changed by this control. Clients should not change @exposureTargetBias@ on the device when @action@ is called.
--
-- If you need to react to other sources of @exposureTargetBias@ changes, you will still need to use key-value observation.
--
-- @AVCaptureSystemExposureBiasSlider@ may only be initialized with @AVCaptureDevice@ instances that support setting @exposureTargetBias@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- ObjC selector: @- initWithDevice:action:@
initWithDevice_action :: (IsAVCaptureSystemExposureBiasSlider avCaptureSystemExposureBiasSlider, IsAVCaptureDevice device) => avCaptureSystemExposureBiasSlider -> device -> Ptr () -> IO (Id AVCaptureSystemExposureBiasSlider)
initWithDevice_action avCaptureSystemExposureBiasSlider  device action =
withObjCPtr device $ \raw_device ->
    sendMsg avCaptureSystemExposureBiasSlider (mkSelector "initWithDevice:action:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr action :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:action:@
initWithDevice_actionSelector :: Selector
initWithDevice_actionSelector = mkSelector "initWithDevice:action:"

