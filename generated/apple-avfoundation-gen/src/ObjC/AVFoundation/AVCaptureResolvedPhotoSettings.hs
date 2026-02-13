{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureResolvedPhotoSettings
--
-- An immutable object produced by callbacks in each and every AVCapturePhotoCaptureDelegate protocol method.
--
-- When you initiate a photo capture request using -capturePhotoWithSettings:delegate:, some of your settings are not yet certain. For instance, auto flash and auto still image stabilization allow the AVCapturePhotoOutput to decide just in time whether to employ flash or still image stabilization, depending on the current scene. Once the request is issued, AVCapturePhotoOutput begins the capture, resolves the uncertain settings, and in its first callback informs you of its choices through an AVCaptureResolvedPhotoSettings object. This same object is presented to all the callbacks fired for a particular photo capture request. Its uniqueID property matches that of the AVCapturePhotoSettings instance you used to initiate the photo request.
--
-- Generated bindings for @AVCaptureResolvedPhotoSettings@.
module ObjC.AVFoundation.AVCaptureResolvedPhotoSettings
  ( AVCaptureResolvedPhotoSettings
  , IsAVCaptureResolvedPhotoSettings(..)
  , init_
  , new
  , uniqueID
  , flashEnabled
  , redEyeReductionEnabled
  , stillImageStabilizationEnabled
  , virtualDeviceFusionEnabled
  , dualCameraFusionEnabled
  , expectedPhotoCount
  , contentAwareDistortionCorrectionEnabled
  , fastCapturePrioritizationEnabled
  , contentAwareDistortionCorrectionEnabledSelector
  , dualCameraFusionEnabledSelector
  , expectedPhotoCountSelector
  , fastCapturePrioritizationEnabledSelector
  , flashEnabledSelector
  , initSelector
  , newSelector
  , redEyeReductionEnabledSelector
  , stillImageStabilizationEnabledSelector
  , uniqueIDSelector
  , virtualDeviceFusionEnabledSelector


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
init_ :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO (Id AVCaptureResolvedPhotoSettings)
init_ avCaptureResolvedPhotoSettings =
  sendOwnedMessage avCaptureResolvedPhotoSettings initSelector

-- | @+ new@
new :: IO (Id AVCaptureResolvedPhotoSettings)
new  =
  do
    cls' <- getRequiredClass "AVCaptureResolvedPhotoSettings"
    sendOwnedClassMessage cls' newSelector

-- | uniqueID
--
-- uniqueID matches that of the AVCapturePhotoSettings instance you passed to -capturePhotoWithSettings:delegate:.
--
-- ObjC selector: @- uniqueID@
uniqueID :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO CLong
uniqueID avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings uniqueIDSelector

-- | flashEnabled
--
-- Indicates whether the flash will fire when capturing the photo.
--
-- When you specify AVCaptureFlashModeAuto as your AVCapturePhotoSettings.flashMode, you don't know if flash capture will be chosen until you inspect the AVCaptureResolvedPhotoSettings flashEnabled property. If the device becomes too hot, the flash becomes temporarily unavailable. You can key-value observe AVCaptureDevice's flashAvailable property to know when this occurs. If the flash is unavailable due to thermal issues, and you specify a flashMode of AVCaptureFlashModeOn, flashEnabled still resolves to NO until the device has sufficiently cooled off.
--
-- ObjC selector: @- flashEnabled@
flashEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
flashEnabled avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings flashEnabledSelector

-- | redEyeReductionEnabled
--
-- Indicates whether red-eye reduction will be applied as necessary when capturing the photo if flashEnabled is YES.
--
-- ObjC selector: @- redEyeReductionEnabled@
redEyeReductionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
redEyeReductionEnabled avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings redEyeReductionEnabledSelector

-- | stillImageStabilizationEnabled
--
-- Indicates whether still image stabilization will be employed when capturing the photo.
--
-- As of iOS 13 hardware, the AVCapturePhotoOutput is capable of applying a variety of multi-image fusion techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), all of which have been previously lumped under the stillImageStabilization moniker. This property should no longer be used as it no longer provides meaningful information about the techniques used to improve quality in a photo capture. Instead, you should use -photoQualityPrioritization to indicate your preferred quality vs speed when configuring your AVCapturePhotoSettings. You may query -photoProcessingTimeRange to get an indication of how long the photo will take to process before delivery to your delegate.
--
-- ObjC selector: @- stillImageStabilizationEnabled@
stillImageStabilizationEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
stillImageStabilizationEnabled avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings stillImageStabilizationEnabledSelector

-- | virtualDeviceFusionEnabled
--
-- Indicates whether fusion of virtual device constituent camera images will be used when capturing the photo, such as the wide-angle and telephoto images on a DualCamera.
--
-- ObjC selector: @- virtualDeviceFusionEnabled@
virtualDeviceFusionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
virtualDeviceFusionEnabled avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings virtualDeviceFusionEnabledSelector

-- | dualCameraFusionEnabled
--
-- Indicates whether DualCamera wide-angle and telephoto image fusion will be employed when capturing the photo. As of iOS 13, this property is deprecated in favor of virtualDeviceFusionEnabled.
--
-- ObjC selector: @- dualCameraFusionEnabled@
dualCameraFusionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
dualCameraFusionEnabled avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings dualCameraFusionEnabledSelector

-- | expectedPhotoCount
--
-- Indicates the number of times your -captureOutput:didFinishProcessingPhoto:error: callback will be called. For instance, if you've requested an auto exposure bracket of 3 with JPEG and RAW, the expectedPhotoCount is 6.
--
-- ObjC selector: @- expectedPhotoCount@
expectedPhotoCount :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO CULong
expectedPhotoCount avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings expectedPhotoCountSelector

-- | contentAwareDistortionCorrectionEnabled
--
-- Indicates whether content aware distortion correction will be employed when capturing the photo.
--
-- ObjC selector: @- contentAwareDistortionCorrectionEnabled@
contentAwareDistortionCorrectionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
contentAwareDistortionCorrectionEnabled avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings contentAwareDistortionCorrectionEnabledSelector

-- | fastCapturePrioritizationEnabled
--
-- Indicates whether fast capture prioritization will be employed when capturing the photo.
--
-- ObjC selector: @- fastCapturePrioritizationEnabled@
fastCapturePrioritizationEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
fastCapturePrioritizationEnabled avCaptureResolvedPhotoSettings =
  sendMessage avCaptureResolvedPhotoSettings fastCapturePrioritizationEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureResolvedPhotoSettings)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureResolvedPhotoSettings)
newSelector = mkSelector "new"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] CLong
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @flashEnabled@
flashEnabledSelector :: Selector '[] Bool
flashEnabledSelector = mkSelector "flashEnabled"

-- | @Selector@ for @redEyeReductionEnabled@
redEyeReductionEnabledSelector :: Selector '[] Bool
redEyeReductionEnabledSelector = mkSelector "redEyeReductionEnabled"

-- | @Selector@ for @stillImageStabilizationEnabled@
stillImageStabilizationEnabledSelector :: Selector '[] Bool
stillImageStabilizationEnabledSelector = mkSelector "stillImageStabilizationEnabled"

-- | @Selector@ for @virtualDeviceFusionEnabled@
virtualDeviceFusionEnabledSelector :: Selector '[] Bool
virtualDeviceFusionEnabledSelector = mkSelector "virtualDeviceFusionEnabled"

-- | @Selector@ for @dualCameraFusionEnabled@
dualCameraFusionEnabledSelector :: Selector '[] Bool
dualCameraFusionEnabledSelector = mkSelector "dualCameraFusionEnabled"

-- | @Selector@ for @expectedPhotoCount@
expectedPhotoCountSelector :: Selector '[] CULong
expectedPhotoCountSelector = mkSelector "expectedPhotoCount"

-- | @Selector@ for @contentAwareDistortionCorrectionEnabled@
contentAwareDistortionCorrectionEnabledSelector :: Selector '[] Bool
contentAwareDistortionCorrectionEnabledSelector = mkSelector "contentAwareDistortionCorrectionEnabled"

-- | @Selector@ for @fastCapturePrioritizationEnabled@
fastCapturePrioritizationEnabledSelector :: Selector '[] Bool
fastCapturePrioritizationEnabledSelector = mkSelector "fastCapturePrioritizationEnabled"

