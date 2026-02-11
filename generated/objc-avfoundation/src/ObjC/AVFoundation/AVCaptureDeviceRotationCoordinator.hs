{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDeviceRotationCoordinator
--
-- The AVCaptureDeviceRotationCoordinator allows clients to monitor rotations of a given AVCaptureDevice instance and be provided the video rotation angle that should be applied for horizon-level preview and capture relative to gravity.
--
-- Each instance of AVCaptureDeviceRotationCoordinator allows a client to coordinate with changes to the rotation of an AVCaptureDevice to ensure the camera's video preview and captured output are horizon-level. The coordinator delivers key-value updates on the main queue.
--
-- Generated bindings for @AVCaptureDeviceRotationCoordinator@.
module ObjC.AVFoundation.AVCaptureDeviceRotationCoordinator
  ( AVCaptureDeviceRotationCoordinator
  , IsAVCaptureDeviceRotationCoordinator(..)
  , init_
  , new
  , initWithDevice_previewLayer
  , device
  , previewLayer
  , videoRotationAngleForHorizonLevelPreview
  , videoRotationAngleForHorizonLevelCapture
  , initSelector
  , newSelector
  , initWithDevice_previewLayerSelector
  , deviceSelector
  , previewLayerSelector
  , videoRotationAngleForHorizonLevelPreviewSelector
  , videoRotationAngleForHorizonLevelCaptureSelector


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
import ObjC.QuartzCore.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureDeviceRotationCoordinator avCaptureDeviceRotationCoordinator => avCaptureDeviceRotationCoordinator -> IO (Id AVCaptureDeviceRotationCoordinator)
init_ avCaptureDeviceRotationCoordinator  =
  sendMsg avCaptureDeviceRotationCoordinator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureDeviceRotationCoordinator)
new  =
  do
    cls' <- getRequiredClass "AVCaptureDeviceRotationCoordinator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithDevice:previewLayer:
--
-- Returns an AVCaptureDeviceRotationCoordinator instance that provides updates to the amount of rotation that should be applied for horizon-level preview and capture relative to gravity.
--
-- @device@ — The device for which to monitor rotation.
--
-- @previewLayer@ — A layer displaying the camera's video preview. If nil, the coordinator will return 0 degrees of rotation for horizon-level preview.
--
-- Returns: An AVCaptureDeviceRotationCoordinator from which rotation angles for preview and capture can be obtained.
--
-- An AVCaptureDeviceRotationCoordinator is only applicable to video devices. The given device and layer determine the amount of rotation that should be applied for horizon-level preview and capture.
--
-- ObjC selector: @- initWithDevice:previewLayer:@
initWithDevice_previewLayer :: (IsAVCaptureDeviceRotationCoordinator avCaptureDeviceRotationCoordinator, IsAVCaptureDevice device, IsCALayer previewLayer) => avCaptureDeviceRotationCoordinator -> device -> previewLayer -> IO (Id AVCaptureDeviceRotationCoordinator)
initWithDevice_previewLayer avCaptureDeviceRotationCoordinator  device previewLayer =
withObjCPtr device $ \raw_device ->
  withObjCPtr previewLayer $ \raw_previewLayer ->
      sendMsg avCaptureDeviceRotationCoordinator (mkSelector "initWithDevice:previewLayer:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_previewLayer :: Ptr ())] >>= ownedObject . castPtr

-- | device
--
-- The the device for which the coordinator provides video rotation angles.
--
-- The value of this property is the AVCaptureDevice instance that was used to create the coordinator. The coordinator holds a weak reference to the device.
--
-- ObjC selector: @- device@
device :: IsAVCaptureDeviceRotationCoordinator avCaptureDeviceRotationCoordinator => avCaptureDeviceRotationCoordinator -> IO (Id AVCaptureDevice)
device avCaptureDeviceRotationCoordinator  =
  sendMsg avCaptureDeviceRotationCoordinator (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | previewLayer
--
-- The CALayer for which the coordinator calculates video rotation angles for horizon-level preview.
--
-- The value of this property is the CALayer instance that was used to create the coordinator. Clients may specify an AVCaptureVideoPreviewLayer or other CALayer instance that displays a camera's video preview. The coordinator holds a weak reference to the layer. The coordinator will return 0 degrees of rotation from -videoRotationAngleForHorizonLevelPreview if a layer was not specified at initialization, the layer is not in a view hierarchy, or the layer has been deallocated.
--
-- ObjC selector: @- previewLayer@
previewLayer :: IsAVCaptureDeviceRotationCoordinator avCaptureDeviceRotationCoordinator => avCaptureDeviceRotationCoordinator -> IO (Id CALayer)
previewLayer avCaptureDeviceRotationCoordinator  =
  sendMsg avCaptureDeviceRotationCoordinator (mkSelector "previewLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | videoRotationAngleForHorizonLevelPreview
--
-- Returns a video rotation angle in degrees for displaying the camera's video preview in the given CALayer.
--
-- The video rotation angle represents by how much the camera's video preview should be rotated for display in the CALayer to be horizon-level relative to gravity. An angle of 0 degrees means that video will be output in the camera's unrotated, native sensor orientation. The video rotation angle for preview may differ between cameras at different positions. For example when an iOS device is held in portrait orientation, the video preview for built-in cameras may need to be rotated by 90 degrees while the video preview for an external camera should not be rotated. External cameras return 0 degrees of rotation even if they physically rotate when their position in physical space is unknown. This property is key-value observable and delivers updates on the main queue.
--
-- ObjC selector: @- videoRotationAngleForHorizonLevelPreview@
videoRotationAngleForHorizonLevelPreview :: IsAVCaptureDeviceRotationCoordinator avCaptureDeviceRotationCoordinator => avCaptureDeviceRotationCoordinator -> IO CDouble
videoRotationAngleForHorizonLevelPreview avCaptureDeviceRotationCoordinator  =
  sendMsg avCaptureDeviceRotationCoordinator (mkSelector "videoRotationAngleForHorizonLevelPreview") retCDouble []

-- | videoRotationAngleForHorizonLevelCapture
--
-- Returns a video rotation angle in degrees for horizon-level capture from this camera.
--
-- The video rotation angle represents by how much the photos or movies captured from the camera should be rotated to be horizon-level relative to gravity. A video rotation angle of 0 degrees means that the output will be in the camera's unrotated, native sensor orientation. The video rotation angle for capture may differ between cameras. For example when an iOS device is held in portrait orientation, photos and movies captured from built-in cameras may need to be rotated by 90 degrees while the photos and movies from an external camera should not be rotated. External cameras return 0 degrees of rotation even if they physically rotate when their position in physical space is unknown. The video rotation angle returned from this property is distinct from the angle returned by -videoRotationAngleForHorizonLevelPreview because in certain combinations of device and interface orientations, the video rotation angle needed for horizon-level preview may not match the amount of rotation needed for horizon-level capture. This property is key-value observable and delivers updates on the main queue.
--
-- ObjC selector: @- videoRotationAngleForHorizonLevelCapture@
videoRotationAngleForHorizonLevelCapture :: IsAVCaptureDeviceRotationCoordinator avCaptureDeviceRotationCoordinator => avCaptureDeviceRotationCoordinator -> IO CDouble
videoRotationAngleForHorizonLevelCapture avCaptureDeviceRotationCoordinator  =
  sendMsg avCaptureDeviceRotationCoordinator (mkSelector "videoRotationAngleForHorizonLevelCapture") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:previewLayer:@
initWithDevice_previewLayerSelector :: Selector
initWithDevice_previewLayerSelector = mkSelector "initWithDevice:previewLayer:"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @previewLayer@
previewLayerSelector :: Selector
previewLayerSelector = mkSelector "previewLayer"

-- | @Selector@ for @videoRotationAngleForHorizonLevelPreview@
videoRotationAngleForHorizonLevelPreviewSelector :: Selector
videoRotationAngleForHorizonLevelPreviewSelector = mkSelector "videoRotationAngleForHorizonLevelPreview"

-- | @Selector@ for @videoRotationAngleForHorizonLevelCapture@
videoRotationAngleForHorizonLevelCaptureSelector :: Selector
videoRotationAngleForHorizonLevelCaptureSelector = mkSelector "videoRotationAngleForHorizonLevelCapture"

