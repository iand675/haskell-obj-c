{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSystemZoomSlider
--
-- The system's recommended continuous zoom control for @-[AVCaptureDevice videoZoomFactor]@.
--
-- @AVCaptureSystemZoomSlider@ uses the range specified by the @systemRecommendedVideoZoomRange@ on the @activeFormat@ from the @AVCaptureDevice@ specified during initialization. As the device's @activeFormat@ changes, the slider updates its range with the new format's @systemRecommendedVideoZoomRange@.
--
-- Controls may be added to an @AVCaptureSession@ using @-[AVCaptureSession addControl:]@.
--
-- Generated bindings for @AVCaptureSystemZoomSlider@.
module ObjC.AVFoundation.AVCaptureSystemZoomSlider
  ( AVCaptureSystemZoomSlider
  , IsAVCaptureSystemZoomSlider(..)
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
-- Initializes an @AVCaptureSystemZoomSlider@ for controlling @device@.
--
-- @device@ — The device to control.
--
-- @AVCaptureSystemZoomSlider@ may only be initialized with @AVCaptureDevice@ instances that support setting @videoZoomFactor@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: (IsAVCaptureSystemZoomSlider avCaptureSystemZoomSlider, IsAVCaptureDevice device) => avCaptureSystemZoomSlider -> device -> IO (Id AVCaptureSystemZoomSlider)
initWithDevice avCaptureSystemZoomSlider  device =
withObjCPtr device $ \raw_device ->
    sendMsg avCaptureSystemZoomSlider (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ())] >>= ownedObject . castPtr

-- | initWithDevice:action
--
-- Initializes an @AVCaptureSystemZoomSlider@ for controlling @device@ with a `@ @action@ for handling @videoZoomFactor` changes.
--
-- @device@ — The device to control.
--
-- @action@ — An action called on `@ to handle @videoZoomFactor@ changes by @AVCaptureSystemZoomSlider`.
--
-- @action@ is **only** called when @videoZoomFactor@ is changed by this control. Clients should not change @videoZoomFactor@ on the device when @action@ is called.
--
-- If you need to react to other sources of @videoZoomFactor@ changes like @rampToVideoZoomFactor:withRate:@ you will still need to use key-value observation.
--
-- @AVCaptureSystemZoomSlider@ may only be initialized with @AVCaptureDevice@ instances that support setting @videoZoomFactor@, otherwise an @NSInvalidArgumentException@ is thrown.
--
-- ObjC selector: @- initWithDevice:action:@
initWithDevice_action :: (IsAVCaptureSystemZoomSlider avCaptureSystemZoomSlider, IsAVCaptureDevice device) => avCaptureSystemZoomSlider -> device -> Ptr () -> IO (Id AVCaptureSystemZoomSlider)
initWithDevice_action avCaptureSystemZoomSlider  device action =
withObjCPtr device $ \raw_device ->
    sendMsg avCaptureSystemZoomSlider (mkSelector "initWithDevice:action:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr action :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:action:@
initWithDevice_actionSelector :: Selector
initWithDevice_actionSelector = mkSelector "initWithDevice:action:"

