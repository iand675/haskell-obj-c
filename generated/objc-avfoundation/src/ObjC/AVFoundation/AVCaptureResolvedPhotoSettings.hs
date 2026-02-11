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
  , initSelector
  , newSelector
  , uniqueIDSelector
  , flashEnabledSelector
  , redEyeReductionEnabledSelector
  , stillImageStabilizationEnabledSelector
  , virtualDeviceFusionEnabledSelector
  , dualCameraFusionEnabledSelector
  , expectedPhotoCountSelector
  , contentAwareDistortionCorrectionEnabledSelector
  , fastCapturePrioritizationEnabledSelector


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
init_ :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO (Id AVCaptureResolvedPhotoSettings)
init_ avCaptureResolvedPhotoSettings  =
  sendMsg avCaptureResolvedPhotoSettings (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureResolvedPhotoSettings)
new  =
  do
    cls' <- getRequiredClass "AVCaptureResolvedPhotoSettings"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | uniqueID
--
-- uniqueID matches that of the AVCapturePhotoSettings instance you passed to -capturePhotoWithSettings:delegate:.
--
-- ObjC selector: @- uniqueID@
uniqueID :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO CLong
uniqueID avCaptureResolvedPhotoSettings  =
  sendMsg avCaptureResolvedPhotoSettings (mkSelector "uniqueID") retCLong []

-- | flashEnabled
--
-- Indicates whether the flash will fire when capturing the photo.
--
-- When you specify AVCaptureFlashModeAuto as your AVCapturePhotoSettings.flashMode, you don't know if flash capture will be chosen until you inspect the AVCaptureResolvedPhotoSettings flashEnabled property. If the device becomes too hot, the flash becomes temporarily unavailable. You can key-value observe AVCaptureDevice's flashAvailable property to know when this occurs. If the flash is unavailable due to thermal issues, and you specify a flashMode of AVCaptureFlashModeOn, flashEnabled still resolves to NO until the device has sufficiently cooled off.
--
-- ObjC selector: @- flashEnabled@
flashEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
flashEnabled avCaptureResolvedPhotoSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureResolvedPhotoSettings (mkSelector "flashEnabled") retCULong []

-- | redEyeReductionEnabled
--
-- Indicates whether red-eye reduction will be applied as necessary when capturing the photo if flashEnabled is YES.
--
-- ObjC selector: @- redEyeReductionEnabled@
redEyeReductionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
redEyeReductionEnabled avCaptureResolvedPhotoSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureResolvedPhotoSettings (mkSelector "redEyeReductionEnabled") retCULong []

-- | stillImageStabilizationEnabled
--
-- Indicates whether still image stabilization will be employed when capturing the photo.
--
-- As of iOS 13 hardware, the AVCapturePhotoOutput is capable of applying a variety of multi-image fusion techniques to improve photo quality (reduce noise, preserve detail in low light, freeze motion, etc), all of which have been previously lumped under the stillImageStabilization moniker. This property should no longer be used as it no longer provides meaningful information about the techniques used to improve quality in a photo capture. Instead, you should use -photoQualityPrioritization to indicate your preferred quality vs speed when configuring your AVCapturePhotoSettings. You may query -photoProcessingTimeRange to get an indication of how long the photo will take to process before delivery to your delegate.
--
-- ObjC selector: @- stillImageStabilizationEnabled@
stillImageStabilizationEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
stillImageStabilizationEnabled avCaptureResolvedPhotoSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureResolvedPhotoSettings (mkSelector "stillImageStabilizationEnabled") retCULong []

-- | virtualDeviceFusionEnabled
--
-- Indicates whether fusion of virtual device constituent camera images will be used when capturing the photo, such as the wide-angle and telephoto images on a DualCamera.
--
-- ObjC selector: @- virtualDeviceFusionEnabled@
virtualDeviceFusionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
virtualDeviceFusionEnabled avCaptureResolvedPhotoSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureResolvedPhotoSettings (mkSelector "virtualDeviceFusionEnabled") retCULong []

-- | dualCameraFusionEnabled
--
-- Indicates whether DualCamera wide-angle and telephoto image fusion will be employed when capturing the photo. As of iOS 13, this property is deprecated in favor of virtualDeviceFusionEnabled.
--
-- ObjC selector: @- dualCameraFusionEnabled@
dualCameraFusionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
dualCameraFusionEnabled avCaptureResolvedPhotoSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureResolvedPhotoSettings (mkSelector "dualCameraFusionEnabled") retCULong []

-- | expectedPhotoCount
--
-- Indicates the number of times your -captureOutput:didFinishProcessingPhoto:error: callback will be called. For instance, if you've requested an auto exposure bracket of 3 with JPEG and RAW, the expectedPhotoCount is 6.
--
-- ObjC selector: @- expectedPhotoCount@
expectedPhotoCount :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO CULong
expectedPhotoCount avCaptureResolvedPhotoSettings  =
  sendMsg avCaptureResolvedPhotoSettings (mkSelector "expectedPhotoCount") retCULong []

-- | contentAwareDistortionCorrectionEnabled
--
-- Indicates whether content aware distortion correction will be employed when capturing the photo.
--
-- ObjC selector: @- contentAwareDistortionCorrectionEnabled@
contentAwareDistortionCorrectionEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
contentAwareDistortionCorrectionEnabled avCaptureResolvedPhotoSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureResolvedPhotoSettings (mkSelector "contentAwareDistortionCorrectionEnabled") retCULong []

-- | fastCapturePrioritizationEnabled
--
-- Indicates whether fast capture prioritization will be employed when capturing the photo.
--
-- ObjC selector: @- fastCapturePrioritizationEnabled@
fastCapturePrioritizationEnabled :: IsAVCaptureResolvedPhotoSettings avCaptureResolvedPhotoSettings => avCaptureResolvedPhotoSettings -> IO Bool
fastCapturePrioritizationEnabled avCaptureResolvedPhotoSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureResolvedPhotoSettings (mkSelector "fastCapturePrioritizationEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @flashEnabled@
flashEnabledSelector :: Selector
flashEnabledSelector = mkSelector "flashEnabled"

-- | @Selector@ for @redEyeReductionEnabled@
redEyeReductionEnabledSelector :: Selector
redEyeReductionEnabledSelector = mkSelector "redEyeReductionEnabled"

-- | @Selector@ for @stillImageStabilizationEnabled@
stillImageStabilizationEnabledSelector :: Selector
stillImageStabilizationEnabledSelector = mkSelector "stillImageStabilizationEnabled"

-- | @Selector@ for @virtualDeviceFusionEnabled@
virtualDeviceFusionEnabledSelector :: Selector
virtualDeviceFusionEnabledSelector = mkSelector "virtualDeviceFusionEnabled"

-- | @Selector@ for @dualCameraFusionEnabled@
dualCameraFusionEnabledSelector :: Selector
dualCameraFusionEnabledSelector = mkSelector "dualCameraFusionEnabled"

-- | @Selector@ for @expectedPhotoCount@
expectedPhotoCountSelector :: Selector
expectedPhotoCountSelector = mkSelector "expectedPhotoCount"

-- | @Selector@ for @contentAwareDistortionCorrectionEnabled@
contentAwareDistortionCorrectionEnabledSelector :: Selector
contentAwareDistortionCorrectionEnabledSelector = mkSelector "contentAwareDistortionCorrectionEnabled"

-- | @Selector@ for @fastCapturePrioritizationEnabled@
fastCapturePrioritizationEnabledSelector :: Selector
fastCapturePrioritizationEnabledSelector = mkSelector "fastCapturePrioritizationEnabled"

