{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNCamera
--
-- SCNCamera represents a camera that can be attached to a SCNNode.
--
-- A node with a camera can be used as a point of view to visualize a 3D scene.
--
-- Generated bindings for @SCNCamera@.
module ObjC.SceneKit.SCNCamera
  ( SCNCamera
  , IsSCNCamera(..)
  , camera
  , projectionTransform
  , setProjectionTransform
  , name
  , setName
  , fieldOfView
  , setFieldOfView
  , projectionDirection
  , setProjectionDirection
  , focalLength
  , setFocalLength
  , sensorHeight
  , setSensorHeight
  , zNear
  , setZNear
  , zFar
  , setZFar
  , automaticallyAdjustsZRange
  , setAutomaticallyAdjustsZRange
  , usesOrthographicProjection
  , setUsesOrthographicProjection
  , orthographicScale
  , setOrthographicScale
  , wantsDepthOfField
  , setWantsDepthOfField
  , focusDistance
  , setFocusDistance
  , focalBlurSampleCount
  , setFocalBlurSampleCount
  , fStop
  , setFStop
  , apertureBladeCount
  , setApertureBladeCount
  , motionBlurIntensity
  , setMotionBlurIntensity
  , screenSpaceAmbientOcclusionIntensity
  , setScreenSpaceAmbientOcclusionIntensity
  , screenSpaceAmbientOcclusionRadius
  , setScreenSpaceAmbientOcclusionRadius
  , screenSpaceAmbientOcclusionBias
  , setScreenSpaceAmbientOcclusionBias
  , screenSpaceAmbientOcclusionDepthThreshold
  , setScreenSpaceAmbientOcclusionDepthThreshold
  , screenSpaceAmbientOcclusionNormalThreshold
  , setScreenSpaceAmbientOcclusionNormalThreshold
  , wantsHDR
  , setWantsHDR
  , exposureOffset
  , setExposureOffset
  , averageGray
  , setAverageGray
  , whitePoint
  , setWhitePoint
  , wantsExposureAdaptation
  , setWantsExposureAdaptation
  , exposureAdaptationBrighteningSpeedFactor
  , setExposureAdaptationBrighteningSpeedFactor
  , exposureAdaptationDarkeningSpeedFactor
  , setExposureAdaptationDarkeningSpeedFactor
  , minimumExposure
  , setMinimumExposure
  , maximumExposure
  , setMaximumExposure
  , bloomThreshold
  , setBloomThreshold
  , bloomIterationCount
  , setBloomIterationCount
  , bloomIterationSpread
  , setBloomIterationSpread
  , bloomIntensity
  , setBloomIntensity
  , bloomBlurRadius
  , setBloomBlurRadius
  , vignettingPower
  , setVignettingPower
  , vignettingIntensity
  , setVignettingIntensity
  , colorFringeStrength
  , setColorFringeStrength
  , colorFringeIntensity
  , setColorFringeIntensity
  , saturation
  , setSaturation
  , contrast
  , setContrast
  , grainIntensity
  , setGrainIntensity
  , grainScale
  , setGrainScale
  , grainIsColored
  , setGrainIsColored
  , whiteBalanceTemperature
  , setWhiteBalanceTemperature
  , whiteBalanceTint
  , setWhiteBalanceTint
  , colorGrading
  , categoryBitMask
  , setCategoryBitMask
  , focalBlurRadius
  , setFocalBlurRadius
  , xFov
  , setXFov
  , yFov
  , setYFov
  , aperture
  , setAperture
  , focalSize
  , setFocalSize
  , focalDistance
  , setFocalDistance
  , apertureBladeCountSelector
  , apertureSelector
  , automaticallyAdjustsZRangeSelector
  , averageGraySelector
  , bloomBlurRadiusSelector
  , bloomIntensitySelector
  , bloomIterationCountSelector
  , bloomIterationSpreadSelector
  , bloomThresholdSelector
  , cameraSelector
  , categoryBitMaskSelector
  , colorFringeIntensitySelector
  , colorFringeStrengthSelector
  , colorGradingSelector
  , contrastSelector
  , exposureAdaptationBrighteningSpeedFactorSelector
  , exposureAdaptationDarkeningSpeedFactorSelector
  , exposureOffsetSelector
  , fStopSelector
  , fieldOfViewSelector
  , focalBlurRadiusSelector
  , focalBlurSampleCountSelector
  , focalDistanceSelector
  , focalLengthSelector
  , focalSizeSelector
  , focusDistanceSelector
  , grainIntensitySelector
  , grainIsColoredSelector
  , grainScaleSelector
  , maximumExposureSelector
  , minimumExposureSelector
  , motionBlurIntensitySelector
  , nameSelector
  , orthographicScaleSelector
  , projectionDirectionSelector
  , projectionTransformSelector
  , saturationSelector
  , screenSpaceAmbientOcclusionBiasSelector
  , screenSpaceAmbientOcclusionDepthThresholdSelector
  , screenSpaceAmbientOcclusionIntensitySelector
  , screenSpaceAmbientOcclusionNormalThresholdSelector
  , screenSpaceAmbientOcclusionRadiusSelector
  , sensorHeightSelector
  , setApertureBladeCountSelector
  , setApertureSelector
  , setAutomaticallyAdjustsZRangeSelector
  , setAverageGraySelector
  , setBloomBlurRadiusSelector
  , setBloomIntensitySelector
  , setBloomIterationCountSelector
  , setBloomIterationSpreadSelector
  , setBloomThresholdSelector
  , setCategoryBitMaskSelector
  , setColorFringeIntensitySelector
  , setColorFringeStrengthSelector
  , setContrastSelector
  , setExposureAdaptationBrighteningSpeedFactorSelector
  , setExposureAdaptationDarkeningSpeedFactorSelector
  , setExposureOffsetSelector
  , setFStopSelector
  , setFieldOfViewSelector
  , setFocalBlurRadiusSelector
  , setFocalBlurSampleCountSelector
  , setFocalDistanceSelector
  , setFocalLengthSelector
  , setFocalSizeSelector
  , setFocusDistanceSelector
  , setGrainIntensitySelector
  , setGrainIsColoredSelector
  , setGrainScaleSelector
  , setMaximumExposureSelector
  , setMinimumExposureSelector
  , setMotionBlurIntensitySelector
  , setNameSelector
  , setOrthographicScaleSelector
  , setProjectionDirectionSelector
  , setProjectionTransformSelector
  , setSaturationSelector
  , setScreenSpaceAmbientOcclusionBiasSelector
  , setScreenSpaceAmbientOcclusionDepthThresholdSelector
  , setScreenSpaceAmbientOcclusionIntensitySelector
  , setScreenSpaceAmbientOcclusionNormalThresholdSelector
  , setScreenSpaceAmbientOcclusionRadiusSelector
  , setSensorHeightSelector
  , setUsesOrthographicProjectionSelector
  , setVignettingIntensitySelector
  , setVignettingPowerSelector
  , setWantsDepthOfFieldSelector
  , setWantsExposureAdaptationSelector
  , setWantsHDRSelector
  , setWhiteBalanceTemperatureSelector
  , setWhiteBalanceTintSelector
  , setWhitePointSelector
  , setXFovSelector
  , setYFovSelector
  , setZFarSelector
  , setZNearSelector
  , usesOrthographicProjectionSelector
  , vignettingIntensitySelector
  , vignettingPowerSelector
  , wantsDepthOfFieldSelector
  , wantsExposureAdaptationSelector
  , wantsHDRSelector
  , whiteBalanceTemperatureSelector
  , whiteBalanceTintSelector
  , whitePointSelector
  , xFovSelector
  , yFovSelector
  , zFarSelector
  , zNearSelector

  -- * Enum types
  , SCNCameraProjectionDirection(SCNCameraProjectionDirection)
  , pattern SCNCameraProjectionDirectionVertical
  , pattern SCNCameraProjectionDirectionHorizontal

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | camera
--
-- Creates and returns a camera instance.
--
-- ObjC selector: @+ camera@
camera :: IO (Id SCNCamera)
camera  =
  do
    cls' <- getRequiredClass "SCNCamera"
    sendClassMessage cls' cameraSelector

-- | @- projectionTransform@
projectionTransform :: IsSCNCamera scnCamera => scnCamera -> IO SCNMatrix4
projectionTransform scnCamera =
  sendMessage scnCamera projectionTransformSelector

-- | @- setProjectionTransform:@
setProjectionTransform :: IsSCNCamera scnCamera => scnCamera -> SCNMatrix4 -> IO ()
setProjectionTransform scnCamera projectionTransform =
  sendMessage scnCamera setProjectionTransformSelector projectionTransform

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNCamera scnCamera => scnCamera -> IO (Id NSString)
name scnCamera =
  sendMessage scnCamera nameSelector

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNCamera scnCamera, IsNSString value) => scnCamera -> value -> IO ()
setName scnCamera value =
  sendMessage scnCamera setNameSelector (toNSString value)

-- | fieldOfView
--
-- Determines the receiver's field of view (in degree). Defaults to 60°. Animatable.
--
-- The fieldOfView is automatically updated when the sensorHeight or focalLength are set. Setting the fieldOfView will update the focalLength according to the new fieldOfView and the current sensorHeight.
--
-- ObjC selector: @- fieldOfView@
fieldOfView :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
fieldOfView scnCamera =
  sendMessage scnCamera fieldOfViewSelector

-- | fieldOfView
--
-- Determines the receiver's field of view (in degree). Defaults to 60°. Animatable.
--
-- The fieldOfView is automatically updated when the sensorHeight or focalLength are set. Setting the fieldOfView will update the focalLength according to the new fieldOfView and the current sensorHeight.
--
-- ObjC selector: @- setFieldOfView:@
setFieldOfView :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFieldOfView scnCamera value =
  sendMessage scnCamera setFieldOfViewSelector value

-- | projectionDirection
--
-- Determines whether the fieldOfView (or orthographicScale) is vertical or horizontal. Defaults to vertical.
--
-- ObjC selector: @- projectionDirection@
projectionDirection :: IsSCNCamera scnCamera => scnCamera -> IO SCNCameraProjectionDirection
projectionDirection scnCamera =
  sendMessage scnCamera projectionDirectionSelector

-- | projectionDirection
--
-- Determines whether the fieldOfView (or orthographicScale) is vertical or horizontal. Defaults to vertical.
--
-- ObjC selector: @- setProjectionDirection:@
setProjectionDirection :: IsSCNCamera scnCamera => scnCamera -> SCNCameraProjectionDirection -> IO ()
setProjectionDirection scnCamera value =
  sendMessage scnCamera setProjectionDirectionSelector value

-- | focalLength
--
-- Determines the receiver's focal length in millimeter. Defaults to 50mm. Animatable.
--
-- The focalLength is automatically updated when the sensorHeight or fieldOfView are set. Setting the focalLength will update the fieldOfView according to the new focalLength and the current sensorHeight.
--
-- ObjC selector: @- focalLength@
focalLength :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalLength scnCamera =
  sendMessage scnCamera focalLengthSelector

-- | focalLength
--
-- Determines the receiver's focal length in millimeter. Defaults to 50mm. Animatable.
--
-- The focalLength is automatically updated when the sensorHeight or fieldOfView are set. Setting the focalLength will update the fieldOfView according to the new focalLength and the current sensorHeight.
--
-- ObjC selector: @- setFocalLength:@
setFocalLength :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalLength scnCamera value =
  sendMessage scnCamera setFocalLengthSelector value

-- | sensorHeight
--
-- Determines the vertical size of the sensor in millimeter. Defaults to 24mm. Animatable.
--
-- Setting the sensorHeight will automatically update the fieldOfView according to the new sensorHeight and the current focalLength.
--
-- ObjC selector: @- sensorHeight@
sensorHeight :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
sensorHeight scnCamera =
  sendMessage scnCamera sensorHeightSelector

-- | sensorHeight
--
-- Determines the vertical size of the sensor in millimeter. Defaults to 24mm. Animatable.
--
-- Setting the sensorHeight will automatically update the fieldOfView according to the new sensorHeight and the current focalLength.
--
-- ObjC selector: @- setSensorHeight:@
setSensorHeight :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setSensorHeight scnCamera value =
  sendMessage scnCamera setSensorHeightSelector value

-- | zNear
--
-- Determines the receiver's near value. Animatable.
--
-- The near value determines the minimal distance between the camera and a visible surface. If a surface is closer to the camera than this minimal distance, then the surface is clipped. The near value must be different than zero. Defaults to 1.
--
-- ObjC selector: @- zNear@
zNear :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
zNear scnCamera =
  sendMessage scnCamera zNearSelector

-- | zNear
--
-- Determines the receiver's near value. Animatable.
--
-- The near value determines the minimal distance between the camera and a visible surface. If a surface is closer to the camera than this minimal distance, then the surface is clipped. The near value must be different than zero. Defaults to 1.
--
-- ObjC selector: @- setZNear:@
setZNear :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setZNear scnCamera value =
  sendMessage scnCamera setZNearSelector value

-- | zFar
--
-- Determines the receiver's far value. Animatable.
--
-- The far value determines the maximal distance between the camera and a visible surface. If a surface is further from the camera than this maximal distance, then the surface is clipped. Defaults to 100.
--
-- ObjC selector: @- zFar@
zFar :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
zFar scnCamera =
  sendMessage scnCamera zFarSelector

-- | zFar
--
-- Determines the receiver's far value. Animatable.
--
-- The far value determines the maximal distance between the camera and a visible surface. If a surface is further from the camera than this maximal distance, then the surface is clipped. Defaults to 100.
--
-- ObjC selector: @- setZFar:@
setZFar :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setZFar scnCamera value =
  sendMessage scnCamera setZFarSelector value

-- | automaticallyAdjustsZRange
--
-- Determines whether the receiver automatically adjusts the zFar value. Defaults to NO.
--
-- When set to YES, the near and far planes are automatically set to fit the bounding box of the entire scene at render time.
--
-- ObjC selector: @- automaticallyAdjustsZRange@
automaticallyAdjustsZRange :: IsSCNCamera scnCamera => scnCamera -> IO Bool
automaticallyAdjustsZRange scnCamera =
  sendMessage scnCamera automaticallyAdjustsZRangeSelector

-- | automaticallyAdjustsZRange
--
-- Determines whether the receiver automatically adjusts the zFar value. Defaults to NO.
--
-- When set to YES, the near and far planes are automatically set to fit the bounding box of the entire scene at render time.
--
-- ObjC selector: @- setAutomaticallyAdjustsZRange:@
setAutomaticallyAdjustsZRange :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setAutomaticallyAdjustsZRange scnCamera value =
  sendMessage scnCamera setAutomaticallyAdjustsZRangeSelector value

-- | usesOrthographicProjection
--
-- Determines whether the receiver uses an orthographic projection or not. Defaults to NO.
--
-- ObjC selector: @- usesOrthographicProjection@
usesOrthographicProjection :: IsSCNCamera scnCamera => scnCamera -> IO Bool
usesOrthographicProjection scnCamera =
  sendMessage scnCamera usesOrthographicProjectionSelector

-- | usesOrthographicProjection
--
-- Determines whether the receiver uses an orthographic projection or not. Defaults to NO.
--
-- ObjC selector: @- setUsesOrthographicProjection:@
setUsesOrthographicProjection :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setUsesOrthographicProjection scnCamera value =
  sendMessage scnCamera setUsesOrthographicProjectionSelector value

-- | orthographicScale
--
-- Determines the receiver's orthographic scale value. Animatable. Defaults to 1.
--
-- This setting determines the size of the camera's visible area. This is only enabled when usesOrthographicProjection is set to YES.
--
-- ObjC selector: @- orthographicScale@
orthographicScale :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
orthographicScale scnCamera =
  sendMessage scnCamera orthographicScaleSelector

-- | orthographicScale
--
-- Determines the receiver's orthographic scale value. Animatable. Defaults to 1.
--
-- This setting determines the size of the camera's visible area. This is only enabled when usesOrthographicProjection is set to YES.
--
-- ObjC selector: @- setOrthographicScale:@
setOrthographicScale :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setOrthographicScale scnCamera value =
  sendMessage scnCamera setOrthographicScaleSelector value

-- | wantsDepthOfField
--
-- Determines if the receiver has depth of field. Defaults to NO.
--
-- ObjC selector: @- wantsDepthOfField@
wantsDepthOfField :: IsSCNCamera scnCamera => scnCamera -> IO Bool
wantsDepthOfField scnCamera =
  sendMessage scnCamera wantsDepthOfFieldSelector

-- | wantsDepthOfField
--
-- Determines if the receiver has depth of field. Defaults to NO.
--
-- ObjC selector: @- setWantsDepthOfField:@
setWantsDepthOfField :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setWantsDepthOfField scnCamera value =
  sendMessage scnCamera setWantsDepthOfFieldSelector value

-- | focusDistance
--
-- Determines the receiver's focus distance. Animatable.
--
-- Defaults to 2.5
--
-- ObjC selector: @- focusDistance@
focusDistance :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focusDistance scnCamera =
  sendMessage scnCamera focusDistanceSelector

-- | focusDistance
--
-- Determines the receiver's focus distance. Animatable.
--
-- Defaults to 2.5
--
-- ObjC selector: @- setFocusDistance:@
setFocusDistance :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocusDistance scnCamera value =
  sendMessage scnCamera setFocusDistanceSelector value

-- | focalBlurSampleCount
--
-- Determines the receiver's sample count for depth of field effect.
--
-- Defaults to 25.
--
-- ObjC selector: @- focalBlurSampleCount@
focalBlurSampleCount :: IsSCNCamera scnCamera => scnCamera -> IO CLong
focalBlurSampleCount scnCamera =
  sendMessage scnCamera focalBlurSampleCountSelector

-- | focalBlurSampleCount
--
-- Determines the receiver's sample count for depth of field effect.
--
-- Defaults to 25.
--
-- ObjC selector: @- setFocalBlurSampleCount:@
setFocalBlurSampleCount :: IsSCNCamera scnCamera => scnCamera -> CLong -> IO ()
setFocalBlurSampleCount scnCamera value =
  sendMessage scnCamera setFocalBlurSampleCountSelector value

-- | fStop
--
-- Determines the receiver's fstop. Animatable.
--
-- Defaults to 5.6.
--
-- ObjC selector: @- fStop@
fStop :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
fStop scnCamera =
  sendMessage scnCamera fStopSelector

-- | fStop
--
-- Determines the receiver's fstop. Animatable.
--
-- Defaults to 5.6.
--
-- ObjC selector: @- setFStop:@
setFStop :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFStop scnCamera value =
  sendMessage scnCamera setFStopSelector value

-- | apertureBladeCount
--
-- Determines the receiver's blade count of the aperture.
--
-- Defaults to 6.
--
-- ObjC selector: @- apertureBladeCount@
apertureBladeCount :: IsSCNCamera scnCamera => scnCamera -> IO CLong
apertureBladeCount scnCamera =
  sendMessage scnCamera apertureBladeCountSelector

-- | apertureBladeCount
--
-- Determines the receiver's blade count of the aperture.
--
-- Defaults to 6.
--
-- ObjC selector: @- setApertureBladeCount:@
setApertureBladeCount :: IsSCNCamera scnCamera => scnCamera -> CLong -> IO ()
setApertureBladeCount scnCamera value =
  sendMessage scnCamera setApertureBladeCountSelector value

-- | motionBlurIntensity
--
-- Determines the intensity of the motion blur. Animatable. Defaults to 0.
--
-- An intensity of zero means no motion blur. The intensity should not exceeed 1.
--
-- ObjC selector: @- motionBlurIntensity@
motionBlurIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
motionBlurIntensity scnCamera =
  sendMessage scnCamera motionBlurIntensitySelector

-- | motionBlurIntensity
--
-- Determines the intensity of the motion blur. Animatable. Defaults to 0.
--
-- An intensity of zero means no motion blur. The intensity should not exceeed 1.
--
-- ObjC selector: @- setMotionBlurIntensity:@
setMotionBlurIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setMotionBlurIntensity scnCamera value =
  sendMessage scnCamera setMotionBlurIntensitySelector value

-- | screenSpaceAmbientOcclusionIntensity
--
-- Determines the intensity of the screen space ambient occlusion. Animatable.
--
-- defaults to 0.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionIntensity@
screenSpaceAmbientOcclusionIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionIntensity scnCamera =
  sendMessage scnCamera screenSpaceAmbientOcclusionIntensitySelector

-- | screenSpaceAmbientOcclusionIntensity
--
-- Determines the intensity of the screen space ambient occlusion. Animatable.
--
-- defaults to 0.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionIntensity:@
setScreenSpaceAmbientOcclusionIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionIntensity scnCamera value =
  sendMessage scnCamera setScreenSpaceAmbientOcclusionIntensitySelector value

-- | screenSpaceAmbientOcclusionRadius
--
-- Determines the screen space ambient occlusion radius in scene unit. Animatable.
--
-- defaults to 5.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionRadius@
screenSpaceAmbientOcclusionRadius :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionRadius scnCamera =
  sendMessage scnCamera screenSpaceAmbientOcclusionRadiusSelector

-- | screenSpaceAmbientOcclusionRadius
--
-- Determines the screen space ambient occlusion radius in scene unit. Animatable.
--
-- defaults to 5.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionRadius:@
setScreenSpaceAmbientOcclusionRadius :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionRadius scnCamera value =
  sendMessage scnCamera setScreenSpaceAmbientOcclusionRadiusSelector value

-- | screenSpaceAmbientOcclusionBias
--
-- Determines self occlusion bias in scene unit.
--
-- defaults to 0.03.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionBias@
screenSpaceAmbientOcclusionBias :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionBias scnCamera =
  sendMessage scnCamera screenSpaceAmbientOcclusionBiasSelector

-- | screenSpaceAmbientOcclusionBias
--
-- Determines self occlusion bias in scene unit.
--
-- defaults to 0.03.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionBias:@
setScreenSpaceAmbientOcclusionBias :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionBias scnCamera value =
  sendMessage scnCamera setScreenSpaceAmbientOcclusionBiasSelector value

-- | screenSpaceAmbientOcclusionDepthThreshold
--
-- Determines the depth blur threshold in scene unit.
--
-- defaults to 0.2.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionDepthThreshold@
screenSpaceAmbientOcclusionDepthThreshold :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionDepthThreshold scnCamera =
  sendMessage scnCamera screenSpaceAmbientOcclusionDepthThresholdSelector

-- | screenSpaceAmbientOcclusionDepthThreshold
--
-- Determines the depth blur threshold in scene unit.
--
-- defaults to 0.2.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionDepthThreshold:@
setScreenSpaceAmbientOcclusionDepthThreshold :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionDepthThreshold scnCamera value =
  sendMessage scnCamera setScreenSpaceAmbientOcclusionDepthThresholdSelector value

-- | screenSpaceAmbientOcclusionNormalThreshold
--
-- Determines the normal blur threshold.
--
-- defaults to 0.3.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionNormalThreshold@
screenSpaceAmbientOcclusionNormalThreshold :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionNormalThreshold scnCamera =
  sendMessage scnCamera screenSpaceAmbientOcclusionNormalThresholdSelector

-- | screenSpaceAmbientOcclusionNormalThreshold
--
-- Determines the normal blur threshold.
--
-- defaults to 0.3.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionNormalThreshold:@
setScreenSpaceAmbientOcclusionNormalThreshold :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionNormalThreshold scnCamera value =
  sendMessage scnCamera setScreenSpaceAmbientOcclusionNormalThresholdSelector value

-- | wantsHDR
--
-- Determines if the receiver has a high dynamic range. Defaults to NO.
--
-- ObjC selector: @- wantsHDR@
wantsHDR :: IsSCNCamera scnCamera => scnCamera -> IO Bool
wantsHDR scnCamera =
  sendMessage scnCamera wantsHDRSelector

-- | wantsHDR
--
-- Determines if the receiver has a high dynamic range. Defaults to NO.
--
-- ObjC selector: @- setWantsHDR:@
setWantsHDR :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setWantsHDR scnCamera value =
  sendMessage scnCamera setWantsHDRSelector value

-- | exposureOffset
--
-- Determines the logarithmic exposure biasing, in EV. Defaults to 0.
--
-- ObjC selector: @- exposureOffset@
exposureOffset :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
exposureOffset scnCamera =
  sendMessage scnCamera exposureOffsetSelector

-- | exposureOffset
--
-- Determines the logarithmic exposure biasing, in EV. Defaults to 0.
--
-- ObjC selector: @- setExposureOffset:@
setExposureOffset :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setExposureOffset scnCamera value =
  sendMessage scnCamera setExposureOffsetSelector value

-- | averageGray
--
-- Determines the average gray level desired in the final image. Defaults to 0.18.
--
-- ObjC selector: @- averageGray@
averageGray :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
averageGray scnCamera =
  sendMessage scnCamera averageGraySelector

-- | averageGray
--
-- Determines the average gray level desired in the final image. Defaults to 0.18.
--
-- ObjC selector: @- setAverageGray:@
setAverageGray :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setAverageGray scnCamera value =
  sendMessage scnCamera setAverageGraySelector value

-- | whitePoint
--
-- Determines the smallest luminance level that will be mapped to white in the final image. Defaults to 1.
--
-- ObjC selector: @- whitePoint@
whitePoint :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
whitePoint scnCamera =
  sendMessage scnCamera whitePointSelector

-- | whitePoint
--
-- Determines the smallest luminance level that will be mapped to white in the final image. Defaults to 1.
--
-- ObjC selector: @- setWhitePoint:@
setWhitePoint :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setWhitePoint scnCamera value =
  sendMessage scnCamera setWhitePointSelector value

-- | wantsExposureAdaptation
--
-- Determines if the receiver should simulate an eye and continuously adjust to luminance. Defaults to YES.
--
-- ObjC selector: @- wantsExposureAdaptation@
wantsExposureAdaptation :: IsSCNCamera scnCamera => scnCamera -> IO Bool
wantsExposureAdaptation scnCamera =
  sendMessage scnCamera wantsExposureAdaptationSelector

-- | wantsExposureAdaptation
--
-- Determines if the receiver should simulate an eye and continuously adjust to luminance. Defaults to YES.
--
-- ObjC selector: @- setWantsExposureAdaptation:@
setWantsExposureAdaptation :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setWantsExposureAdaptation scnCamera value =
  sendMessage scnCamera setWantsExposureAdaptationSelector value

-- | exposureAdaptationBrighteningSpeedFactor
--
-- Determines the exposure adaptation speed when going from bright areas to dark areas. Defaults to 0.4.
--
-- ObjC selector: @- exposureAdaptationBrighteningSpeedFactor@
exposureAdaptationBrighteningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
exposureAdaptationBrighteningSpeedFactor scnCamera =
  sendMessage scnCamera exposureAdaptationBrighteningSpeedFactorSelector

-- | exposureAdaptationBrighteningSpeedFactor
--
-- Determines the exposure adaptation speed when going from bright areas to dark areas. Defaults to 0.4.
--
-- ObjC selector: @- setExposureAdaptationBrighteningSpeedFactor:@
setExposureAdaptationBrighteningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setExposureAdaptationBrighteningSpeedFactor scnCamera value =
  sendMessage scnCamera setExposureAdaptationBrighteningSpeedFactorSelector value

-- | exposureAdaptationDarkeningSpeedFactor
--
-- Determines the exposure adaptation speed when going from dark areas to bright areas. Defaults to 0.6.
--
-- ObjC selector: @- exposureAdaptationDarkeningSpeedFactor@
exposureAdaptationDarkeningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
exposureAdaptationDarkeningSpeedFactor scnCamera =
  sendMessage scnCamera exposureAdaptationDarkeningSpeedFactorSelector

-- | exposureAdaptationDarkeningSpeedFactor
--
-- Determines the exposure adaptation speed when going from dark areas to bright areas. Defaults to 0.6.
--
-- ObjC selector: @- setExposureAdaptationDarkeningSpeedFactor:@
setExposureAdaptationDarkeningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setExposureAdaptationDarkeningSpeedFactor scnCamera value =
  sendMessage scnCamera setExposureAdaptationDarkeningSpeedFactorSelector value

-- | minimumExposure
--
-- Determines the minimum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- minimumExposure@
minimumExposure :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
minimumExposure scnCamera =
  sendMessage scnCamera minimumExposureSelector

-- | minimumExposure
--
-- Determines the minimum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- setMinimumExposure:@
setMinimumExposure :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setMinimumExposure scnCamera value =
  sendMessage scnCamera setMinimumExposureSelector value

-- | maximumExposure
--
-- Determines the maximum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- maximumExposure@
maximumExposure :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
maximumExposure scnCamera =
  sendMessage scnCamera maximumExposureSelector

-- | maximumExposure
--
-- Determines the maximum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- setMaximumExposure:@
setMaximumExposure :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setMaximumExposure scnCamera value =
  sendMessage scnCamera setMaximumExposureSelector value

-- | bloomThreshold
--
-- Determines the luminance threshold for the bloom effect. Animatable. Defaults to 1.
--
-- ObjC selector: @- bloomThreshold@
bloomThreshold :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomThreshold scnCamera =
  sendMessage scnCamera bloomThresholdSelector

-- | bloomThreshold
--
-- Determines the luminance threshold for the bloom effect. Animatable. Defaults to 1.
--
-- ObjC selector: @- setBloomThreshold:@
setBloomThreshold :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomThreshold scnCamera value =
  sendMessage scnCamera setBloomThresholdSelector value

-- | bloomIteration
--
-- Determines the number of blur iterations. Defaults to 1.
--
-- ObjC selector: @- bloomIterationCount@
bloomIterationCount :: IsSCNCamera scnCamera => scnCamera -> IO CLong
bloomIterationCount scnCamera =
  sendMessage scnCamera bloomIterationCountSelector

-- | bloomIteration
--
-- Determines the number of blur iterations. Defaults to 1.
--
-- ObjC selector: @- setBloomIterationCount:@
setBloomIterationCount :: IsSCNCamera scnCamera => scnCamera -> CLong -> IO ()
setBloomIterationCount scnCamera value =
  sendMessage scnCamera setBloomIterationCountSelector value

-- | bloomIterationSpread
--
-- Determines how the bloom iterations are spread. Defaults to 0.
--
-- ObjC selector: @- bloomIterationSpread@
bloomIterationSpread :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomIterationSpread scnCamera =
  sendMessage scnCamera bloomIterationSpreadSelector

-- | bloomIterationSpread
--
-- Determines how the bloom iterations are spread. Defaults to 0.
--
-- ObjC selector: @- setBloomIterationSpread:@
setBloomIterationSpread :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomIterationSpread scnCamera value =
  sendMessage scnCamera setBloomIterationSpreadSelector value

-- | bloomIntensity
--
-- Determines the intensity of the bloom effect. Animatable. Defaults to 0 (no effect).
--
-- ObjC selector: @- bloomIntensity@
bloomIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomIntensity scnCamera =
  sendMessage scnCamera bloomIntensitySelector

-- | bloomIntensity
--
-- Determines the intensity of the bloom effect. Animatable. Defaults to 0 (no effect).
--
-- ObjC selector: @- setBloomIntensity:@
setBloomIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomIntensity scnCamera value =
  sendMessage scnCamera setBloomIntensitySelector value

-- | bloomBlurRadius
--
-- Determines the radius of the bloom effect in points. Animatable. Defaults to 4.
--
-- ObjC selector: @- bloomBlurRadius@
bloomBlurRadius :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomBlurRadius scnCamera =
  sendMessage scnCamera bloomBlurRadiusSelector

-- | bloomBlurRadius
--
-- Determines the radius of the bloom effect in points. Animatable. Defaults to 4.
--
-- ObjC selector: @- setBloomBlurRadius:@
setBloomBlurRadius :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomBlurRadius scnCamera value =
  sendMessage scnCamera setBloomBlurRadiusSelector value

-- | vignettingPower
--
-- Controls the shape of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- vignettingPower@
vignettingPower :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
vignettingPower scnCamera =
  sendMessage scnCamera vignettingPowerSelector

-- | vignettingPower
--
-- Controls the shape of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- setVignettingPower:@
setVignettingPower :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setVignettingPower scnCamera value =
  sendMessage scnCamera setVignettingPowerSelector value

-- | vignettingIntensity
--
-- Controls the intensity of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- vignettingIntensity@
vignettingIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
vignettingIntensity scnCamera =
  sendMessage scnCamera vignettingIntensitySelector

-- | vignettingIntensity
--
-- Controls the intensity of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- setVignettingIntensity:@
setVignettingIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setVignettingIntensity scnCamera value =
  sendMessage scnCamera setVignettingIntensitySelector value

-- | colorFringeStrength
--
-- Controls the strength of the color shift effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- colorFringeStrength@
colorFringeStrength :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
colorFringeStrength scnCamera =
  sendMessage scnCamera colorFringeStrengthSelector

-- | colorFringeStrength
--
-- Controls the strength of the color shift effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- setColorFringeStrength:@
setColorFringeStrength :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setColorFringeStrength scnCamera value =
  sendMessage scnCamera setColorFringeStrengthSelector value

-- | colorFringeIntensity
--
-- Controls the intensity of the color shift effect. Defaults to 1.
--
-- ObjC selector: @- colorFringeIntensity@
colorFringeIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
colorFringeIntensity scnCamera =
  sendMessage scnCamera colorFringeIntensitySelector

-- | colorFringeIntensity
--
-- Controls the intensity of the color shift effect. Defaults to 1.
--
-- ObjC selector: @- setColorFringeIntensity:@
setColorFringeIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setColorFringeIntensity scnCamera value =
  sendMessage scnCamera setColorFringeIntensitySelector value

-- | saturation
--
-- Controls the overall saturation of the scene. Defaults to 1 (no effect).
--
-- ObjC selector: @- saturation@
saturation :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
saturation scnCamera =
  sendMessage scnCamera saturationSelector

-- | saturation
--
-- Controls the overall saturation of the scene. Defaults to 1 (no effect).
--
-- ObjC selector: @- setSaturation:@
setSaturation :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setSaturation scnCamera value =
  sendMessage scnCamera setSaturationSelector value

-- | contrast
--
-- Controls the overall contrast of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- contrast@
contrast :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
contrast scnCamera =
  sendMessage scnCamera contrastSelector

-- | contrast
--
-- Controls the overall contrast of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- setContrast:@
setContrast :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setContrast scnCamera value =
  sendMessage scnCamera setContrastSelector value

-- | grainIntensity
--
-- Controls the intensity of the grain. Defaults to 0 (no effect).
--
-- ObjC selector: @- grainIntensity@
grainIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
grainIntensity scnCamera =
  sendMessage scnCamera grainIntensitySelector

-- | grainIntensity
--
-- Controls the intensity of the grain. Defaults to 0 (no effect).
--
-- ObjC selector: @- setGrainIntensity:@
setGrainIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setGrainIntensity scnCamera value =
  sendMessage scnCamera setGrainIntensitySelector value

-- | grainScale
--
-- Controls the scale of the grain. Defaults to 1.
--
-- ObjC selector: @- grainScale@
grainScale :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
grainScale scnCamera =
  sendMessage scnCamera grainScaleSelector

-- | grainScale
--
-- Controls the scale of the grain. Defaults to 1.
--
-- ObjC selector: @- setGrainScale:@
setGrainScale :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setGrainScale scnCamera value =
  sendMessage scnCamera setGrainScaleSelector value

-- | grainIsColored
--
-- Determines if the grain is colored or not. Defaults to NO.
--
-- ObjC selector: @- grainIsColored@
grainIsColored :: IsSCNCamera scnCamera => scnCamera -> IO Bool
grainIsColored scnCamera =
  sendMessage scnCamera grainIsColoredSelector

-- | grainIsColored
--
-- Determines if the grain is colored or not. Defaults to NO.
--
-- ObjC selector: @- setGrainIsColored:@
setGrainIsColored :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setGrainIsColored scnCamera value =
  sendMessage scnCamera setGrainIsColoredSelector value

-- | whiteBalanceTemperature
--
-- Controls the overall white balance temperature of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- whiteBalanceTemperature@
whiteBalanceTemperature :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
whiteBalanceTemperature scnCamera =
  sendMessage scnCamera whiteBalanceTemperatureSelector

-- | whiteBalanceTemperature
--
-- Controls the overall white balance temperature of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- setWhiteBalanceTemperature:@
setWhiteBalanceTemperature :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setWhiteBalanceTemperature scnCamera value =
  sendMessage scnCamera setWhiteBalanceTemperatureSelector value

-- | whiteBalanceTint
--
-- Controls the overall white balance tint of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- whiteBalanceTint@
whiteBalanceTint :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
whiteBalanceTint scnCamera =
  sendMessage scnCamera whiteBalanceTintSelector

-- | whiteBalanceTint
--
-- Controls the overall white balance tint of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- setWhiteBalanceTint:@
setWhiteBalanceTint :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setWhiteBalanceTint scnCamera value =
  sendMessage scnCamera setWhiteBalanceTintSelector value

-- | colorGrading
--
-- Specifies a lookup texture to apply color grading. The contents must a 2D image representing @n@ slices of a unit color cube texture, arranged in an horizontal row of @n@ images. For instance, a color cube of dimension 16x16x16 should be provided as an image of size 256x16.
--
-- ObjC selector: @- colorGrading@
colorGrading :: IsSCNCamera scnCamera => scnCamera -> IO (Id SCNMaterialProperty)
colorGrading scnCamera =
  sendMessage scnCamera colorGradingSelector

-- | categoryBitMask
--
-- Determines the node categories that are visible from the receiver. Defaults to all bits set.
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSCNCamera scnCamera => scnCamera -> IO CULong
categoryBitMask scnCamera =
  sendMessage scnCamera categoryBitMaskSelector

-- | categoryBitMask
--
-- Determines the node categories that are visible from the receiver. Defaults to all bits set.
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNCamera scnCamera => scnCamera -> CULong -> IO ()
setCategoryBitMask scnCamera value =
  sendMessage scnCamera setCategoryBitMaskSelector value

-- | focalBlurRadius
--
-- Determines the receiver's focal radius. Animatable.
--
-- Determines the maximum amount of blur for objects out of focus. Defaults to 0.
--
-- ObjC selector: @- focalBlurRadius@
focalBlurRadius :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalBlurRadius scnCamera =
  sendMessage scnCamera focalBlurRadiusSelector

-- | focalBlurRadius
--
-- Determines the receiver's focal radius. Animatable.
--
-- Determines the maximum amount of blur for objects out of focus. Defaults to 0.
--
-- ObjC selector: @- setFocalBlurRadius:@
setFocalBlurRadius :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalBlurRadius scnCamera value =
  sendMessage scnCamera setFocalBlurRadiusSelector value

-- | xFov
--
-- Determines the receiver's field of view on the X axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- xFov@
xFov :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
xFov scnCamera =
  sendMessage scnCamera xFovSelector

-- | xFov
--
-- Determines the receiver's field of view on the X axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- setXFov:@
setXFov :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setXFov scnCamera value =
  sendMessage scnCamera setXFovSelector value

-- | yFov
--
-- Determines the receiver's field of view on the Y axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- yFov@
yFov :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
yFov scnCamera =
  sendMessage scnCamera yFovSelector

-- | yFov
--
-- Determines the receiver's field of view on the Y axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- setYFov:@
setYFov :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setYFov scnCamera value =
  sendMessage scnCamera setYFovSelector value

-- | aperture
--
-- Determines the receiver's aperture. Animatable.
--
-- Defaults to 1/8.0.
--
-- ObjC selector: @- aperture@
aperture :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
aperture scnCamera =
  sendMessage scnCamera apertureSelector

-- | aperture
--
-- Determines the receiver's aperture. Animatable.
--
-- Defaults to 1/8.0.
--
-- ObjC selector: @- setAperture:@
setAperture :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setAperture scnCamera value =
  sendMessage scnCamera setApertureSelector value

-- | focalSize
--
-- Determines the receiver's focal size. Animatable.
--
-- Determines the size of the area around focalDistance where the objects are in focus. Defaults to 0.
--
-- ObjC selector: @- focalSize@
focalSize :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalSize scnCamera =
  sendMessage scnCamera focalSizeSelector

-- | focalSize
--
-- Determines the receiver's focal size. Animatable.
--
-- Determines the size of the area around focalDistance where the objects are in focus. Defaults to 0.
--
-- ObjC selector: @- setFocalSize:@
setFocalSize :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalSize scnCamera value =
  sendMessage scnCamera setFocalSizeSelector value

-- | focalDistance
--
-- Determines the receiver's focal distance. Animatable.
--
-- When non zero, the focal distance determines how the camera focuses the objects in the 3d scene. Defaults to 10.0 prior to macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to 2.5 otherwise.
--
-- ObjC selector: @- focalDistance@
focalDistance :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalDistance scnCamera =
  sendMessage scnCamera focalDistanceSelector

-- | focalDistance
--
-- Determines the receiver's focal distance. Animatable.
--
-- When non zero, the focal distance determines how the camera focuses the objects in the 3d scene. Defaults to 10.0 prior to macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to 2.5 otherwise.
--
-- ObjC selector: @- setFocalDistance:@
setFocalDistance :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalDistance scnCamera value =
  sendMessage scnCamera setFocalDistanceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @camera@
cameraSelector :: Selector '[] (Id SCNCamera)
cameraSelector = mkSelector "camera"

-- | @Selector@ for @projectionTransform@
projectionTransformSelector :: Selector '[] SCNMatrix4
projectionTransformSelector = mkSelector "projectionTransform"

-- | @Selector@ for @setProjectionTransform:@
setProjectionTransformSelector :: Selector '[SCNMatrix4] ()
setProjectionTransformSelector = mkSelector "setProjectionTransform:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @fieldOfView@
fieldOfViewSelector :: Selector '[] CDouble
fieldOfViewSelector = mkSelector "fieldOfView"

-- | @Selector@ for @setFieldOfView:@
setFieldOfViewSelector :: Selector '[CDouble] ()
setFieldOfViewSelector = mkSelector "setFieldOfView:"

-- | @Selector@ for @projectionDirection@
projectionDirectionSelector :: Selector '[] SCNCameraProjectionDirection
projectionDirectionSelector = mkSelector "projectionDirection"

-- | @Selector@ for @setProjectionDirection:@
setProjectionDirectionSelector :: Selector '[SCNCameraProjectionDirection] ()
setProjectionDirectionSelector = mkSelector "setProjectionDirection:"

-- | @Selector@ for @focalLength@
focalLengthSelector :: Selector '[] CDouble
focalLengthSelector = mkSelector "focalLength"

-- | @Selector@ for @setFocalLength:@
setFocalLengthSelector :: Selector '[CDouble] ()
setFocalLengthSelector = mkSelector "setFocalLength:"

-- | @Selector@ for @sensorHeight@
sensorHeightSelector :: Selector '[] CDouble
sensorHeightSelector = mkSelector "sensorHeight"

-- | @Selector@ for @setSensorHeight:@
setSensorHeightSelector :: Selector '[CDouble] ()
setSensorHeightSelector = mkSelector "setSensorHeight:"

-- | @Selector@ for @zNear@
zNearSelector :: Selector '[] CDouble
zNearSelector = mkSelector "zNear"

-- | @Selector@ for @setZNear:@
setZNearSelector :: Selector '[CDouble] ()
setZNearSelector = mkSelector "setZNear:"

-- | @Selector@ for @zFar@
zFarSelector :: Selector '[] CDouble
zFarSelector = mkSelector "zFar"

-- | @Selector@ for @setZFar:@
setZFarSelector :: Selector '[CDouble] ()
setZFarSelector = mkSelector "setZFar:"

-- | @Selector@ for @automaticallyAdjustsZRange@
automaticallyAdjustsZRangeSelector :: Selector '[] Bool
automaticallyAdjustsZRangeSelector = mkSelector "automaticallyAdjustsZRange"

-- | @Selector@ for @setAutomaticallyAdjustsZRange:@
setAutomaticallyAdjustsZRangeSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsZRangeSelector = mkSelector "setAutomaticallyAdjustsZRange:"

-- | @Selector@ for @usesOrthographicProjection@
usesOrthographicProjectionSelector :: Selector '[] Bool
usesOrthographicProjectionSelector = mkSelector "usesOrthographicProjection"

-- | @Selector@ for @setUsesOrthographicProjection:@
setUsesOrthographicProjectionSelector :: Selector '[Bool] ()
setUsesOrthographicProjectionSelector = mkSelector "setUsesOrthographicProjection:"

-- | @Selector@ for @orthographicScale@
orthographicScaleSelector :: Selector '[] CDouble
orthographicScaleSelector = mkSelector "orthographicScale"

-- | @Selector@ for @setOrthographicScale:@
setOrthographicScaleSelector :: Selector '[CDouble] ()
setOrthographicScaleSelector = mkSelector "setOrthographicScale:"

-- | @Selector@ for @wantsDepthOfField@
wantsDepthOfFieldSelector :: Selector '[] Bool
wantsDepthOfFieldSelector = mkSelector "wantsDepthOfField"

-- | @Selector@ for @setWantsDepthOfField:@
setWantsDepthOfFieldSelector :: Selector '[Bool] ()
setWantsDepthOfFieldSelector = mkSelector "setWantsDepthOfField:"

-- | @Selector@ for @focusDistance@
focusDistanceSelector :: Selector '[] CDouble
focusDistanceSelector = mkSelector "focusDistance"

-- | @Selector@ for @setFocusDistance:@
setFocusDistanceSelector :: Selector '[CDouble] ()
setFocusDistanceSelector = mkSelector "setFocusDistance:"

-- | @Selector@ for @focalBlurSampleCount@
focalBlurSampleCountSelector :: Selector '[] CLong
focalBlurSampleCountSelector = mkSelector "focalBlurSampleCount"

-- | @Selector@ for @setFocalBlurSampleCount:@
setFocalBlurSampleCountSelector :: Selector '[CLong] ()
setFocalBlurSampleCountSelector = mkSelector "setFocalBlurSampleCount:"

-- | @Selector@ for @fStop@
fStopSelector :: Selector '[] CDouble
fStopSelector = mkSelector "fStop"

-- | @Selector@ for @setFStop:@
setFStopSelector :: Selector '[CDouble] ()
setFStopSelector = mkSelector "setFStop:"

-- | @Selector@ for @apertureBladeCount@
apertureBladeCountSelector :: Selector '[] CLong
apertureBladeCountSelector = mkSelector "apertureBladeCount"

-- | @Selector@ for @setApertureBladeCount:@
setApertureBladeCountSelector :: Selector '[CLong] ()
setApertureBladeCountSelector = mkSelector "setApertureBladeCount:"

-- | @Selector@ for @motionBlurIntensity@
motionBlurIntensitySelector :: Selector '[] CDouble
motionBlurIntensitySelector = mkSelector "motionBlurIntensity"

-- | @Selector@ for @setMotionBlurIntensity:@
setMotionBlurIntensitySelector :: Selector '[CDouble] ()
setMotionBlurIntensitySelector = mkSelector "setMotionBlurIntensity:"

-- | @Selector@ for @screenSpaceAmbientOcclusionIntensity@
screenSpaceAmbientOcclusionIntensitySelector :: Selector '[] CDouble
screenSpaceAmbientOcclusionIntensitySelector = mkSelector "screenSpaceAmbientOcclusionIntensity"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionIntensity:@
setScreenSpaceAmbientOcclusionIntensitySelector :: Selector '[CDouble] ()
setScreenSpaceAmbientOcclusionIntensitySelector = mkSelector "setScreenSpaceAmbientOcclusionIntensity:"

-- | @Selector@ for @screenSpaceAmbientOcclusionRadius@
screenSpaceAmbientOcclusionRadiusSelector :: Selector '[] CDouble
screenSpaceAmbientOcclusionRadiusSelector = mkSelector "screenSpaceAmbientOcclusionRadius"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionRadius:@
setScreenSpaceAmbientOcclusionRadiusSelector :: Selector '[CDouble] ()
setScreenSpaceAmbientOcclusionRadiusSelector = mkSelector "setScreenSpaceAmbientOcclusionRadius:"

-- | @Selector@ for @screenSpaceAmbientOcclusionBias@
screenSpaceAmbientOcclusionBiasSelector :: Selector '[] CDouble
screenSpaceAmbientOcclusionBiasSelector = mkSelector "screenSpaceAmbientOcclusionBias"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionBias:@
setScreenSpaceAmbientOcclusionBiasSelector :: Selector '[CDouble] ()
setScreenSpaceAmbientOcclusionBiasSelector = mkSelector "setScreenSpaceAmbientOcclusionBias:"

-- | @Selector@ for @screenSpaceAmbientOcclusionDepthThreshold@
screenSpaceAmbientOcclusionDepthThresholdSelector :: Selector '[] CDouble
screenSpaceAmbientOcclusionDepthThresholdSelector = mkSelector "screenSpaceAmbientOcclusionDepthThreshold"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionDepthThreshold:@
setScreenSpaceAmbientOcclusionDepthThresholdSelector :: Selector '[CDouble] ()
setScreenSpaceAmbientOcclusionDepthThresholdSelector = mkSelector "setScreenSpaceAmbientOcclusionDepthThreshold:"

-- | @Selector@ for @screenSpaceAmbientOcclusionNormalThreshold@
screenSpaceAmbientOcclusionNormalThresholdSelector :: Selector '[] CDouble
screenSpaceAmbientOcclusionNormalThresholdSelector = mkSelector "screenSpaceAmbientOcclusionNormalThreshold"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionNormalThreshold:@
setScreenSpaceAmbientOcclusionNormalThresholdSelector :: Selector '[CDouble] ()
setScreenSpaceAmbientOcclusionNormalThresholdSelector = mkSelector "setScreenSpaceAmbientOcclusionNormalThreshold:"

-- | @Selector@ for @wantsHDR@
wantsHDRSelector :: Selector '[] Bool
wantsHDRSelector = mkSelector "wantsHDR"

-- | @Selector@ for @setWantsHDR:@
setWantsHDRSelector :: Selector '[Bool] ()
setWantsHDRSelector = mkSelector "setWantsHDR:"

-- | @Selector@ for @exposureOffset@
exposureOffsetSelector :: Selector '[] CDouble
exposureOffsetSelector = mkSelector "exposureOffset"

-- | @Selector@ for @setExposureOffset:@
setExposureOffsetSelector :: Selector '[CDouble] ()
setExposureOffsetSelector = mkSelector "setExposureOffset:"

-- | @Selector@ for @averageGray@
averageGraySelector :: Selector '[] CDouble
averageGraySelector = mkSelector "averageGray"

-- | @Selector@ for @setAverageGray:@
setAverageGraySelector :: Selector '[CDouble] ()
setAverageGraySelector = mkSelector "setAverageGray:"

-- | @Selector@ for @whitePoint@
whitePointSelector :: Selector '[] CDouble
whitePointSelector = mkSelector "whitePoint"

-- | @Selector@ for @setWhitePoint:@
setWhitePointSelector :: Selector '[CDouble] ()
setWhitePointSelector = mkSelector "setWhitePoint:"

-- | @Selector@ for @wantsExposureAdaptation@
wantsExposureAdaptationSelector :: Selector '[] Bool
wantsExposureAdaptationSelector = mkSelector "wantsExposureAdaptation"

-- | @Selector@ for @setWantsExposureAdaptation:@
setWantsExposureAdaptationSelector :: Selector '[Bool] ()
setWantsExposureAdaptationSelector = mkSelector "setWantsExposureAdaptation:"

-- | @Selector@ for @exposureAdaptationBrighteningSpeedFactor@
exposureAdaptationBrighteningSpeedFactorSelector :: Selector '[] CDouble
exposureAdaptationBrighteningSpeedFactorSelector = mkSelector "exposureAdaptationBrighteningSpeedFactor"

-- | @Selector@ for @setExposureAdaptationBrighteningSpeedFactor:@
setExposureAdaptationBrighteningSpeedFactorSelector :: Selector '[CDouble] ()
setExposureAdaptationBrighteningSpeedFactorSelector = mkSelector "setExposureAdaptationBrighteningSpeedFactor:"

-- | @Selector@ for @exposureAdaptationDarkeningSpeedFactor@
exposureAdaptationDarkeningSpeedFactorSelector :: Selector '[] CDouble
exposureAdaptationDarkeningSpeedFactorSelector = mkSelector "exposureAdaptationDarkeningSpeedFactor"

-- | @Selector@ for @setExposureAdaptationDarkeningSpeedFactor:@
setExposureAdaptationDarkeningSpeedFactorSelector :: Selector '[CDouble] ()
setExposureAdaptationDarkeningSpeedFactorSelector = mkSelector "setExposureAdaptationDarkeningSpeedFactor:"

-- | @Selector@ for @minimumExposure@
minimumExposureSelector :: Selector '[] CDouble
minimumExposureSelector = mkSelector "minimumExposure"

-- | @Selector@ for @setMinimumExposure:@
setMinimumExposureSelector :: Selector '[CDouble] ()
setMinimumExposureSelector = mkSelector "setMinimumExposure:"

-- | @Selector@ for @maximumExposure@
maximumExposureSelector :: Selector '[] CDouble
maximumExposureSelector = mkSelector "maximumExposure"

-- | @Selector@ for @setMaximumExposure:@
setMaximumExposureSelector :: Selector '[CDouble] ()
setMaximumExposureSelector = mkSelector "setMaximumExposure:"

-- | @Selector@ for @bloomThreshold@
bloomThresholdSelector :: Selector '[] CDouble
bloomThresholdSelector = mkSelector "bloomThreshold"

-- | @Selector@ for @setBloomThreshold:@
setBloomThresholdSelector :: Selector '[CDouble] ()
setBloomThresholdSelector = mkSelector "setBloomThreshold:"

-- | @Selector@ for @bloomIterationCount@
bloomIterationCountSelector :: Selector '[] CLong
bloomIterationCountSelector = mkSelector "bloomIterationCount"

-- | @Selector@ for @setBloomIterationCount:@
setBloomIterationCountSelector :: Selector '[CLong] ()
setBloomIterationCountSelector = mkSelector "setBloomIterationCount:"

-- | @Selector@ for @bloomIterationSpread@
bloomIterationSpreadSelector :: Selector '[] CDouble
bloomIterationSpreadSelector = mkSelector "bloomIterationSpread"

-- | @Selector@ for @setBloomIterationSpread:@
setBloomIterationSpreadSelector :: Selector '[CDouble] ()
setBloomIterationSpreadSelector = mkSelector "setBloomIterationSpread:"

-- | @Selector@ for @bloomIntensity@
bloomIntensitySelector :: Selector '[] CDouble
bloomIntensitySelector = mkSelector "bloomIntensity"

-- | @Selector@ for @setBloomIntensity:@
setBloomIntensitySelector :: Selector '[CDouble] ()
setBloomIntensitySelector = mkSelector "setBloomIntensity:"

-- | @Selector@ for @bloomBlurRadius@
bloomBlurRadiusSelector :: Selector '[] CDouble
bloomBlurRadiusSelector = mkSelector "bloomBlurRadius"

-- | @Selector@ for @setBloomBlurRadius:@
setBloomBlurRadiusSelector :: Selector '[CDouble] ()
setBloomBlurRadiusSelector = mkSelector "setBloomBlurRadius:"

-- | @Selector@ for @vignettingPower@
vignettingPowerSelector :: Selector '[] CDouble
vignettingPowerSelector = mkSelector "vignettingPower"

-- | @Selector@ for @setVignettingPower:@
setVignettingPowerSelector :: Selector '[CDouble] ()
setVignettingPowerSelector = mkSelector "setVignettingPower:"

-- | @Selector@ for @vignettingIntensity@
vignettingIntensitySelector :: Selector '[] CDouble
vignettingIntensitySelector = mkSelector "vignettingIntensity"

-- | @Selector@ for @setVignettingIntensity:@
setVignettingIntensitySelector :: Selector '[CDouble] ()
setVignettingIntensitySelector = mkSelector "setVignettingIntensity:"

-- | @Selector@ for @colorFringeStrength@
colorFringeStrengthSelector :: Selector '[] CDouble
colorFringeStrengthSelector = mkSelector "colorFringeStrength"

-- | @Selector@ for @setColorFringeStrength:@
setColorFringeStrengthSelector :: Selector '[CDouble] ()
setColorFringeStrengthSelector = mkSelector "setColorFringeStrength:"

-- | @Selector@ for @colorFringeIntensity@
colorFringeIntensitySelector :: Selector '[] CDouble
colorFringeIntensitySelector = mkSelector "colorFringeIntensity"

-- | @Selector@ for @setColorFringeIntensity:@
setColorFringeIntensitySelector :: Selector '[CDouble] ()
setColorFringeIntensitySelector = mkSelector "setColorFringeIntensity:"

-- | @Selector@ for @saturation@
saturationSelector :: Selector '[] CDouble
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @setSaturation:@
setSaturationSelector :: Selector '[CDouble] ()
setSaturationSelector = mkSelector "setSaturation:"

-- | @Selector@ for @contrast@
contrastSelector :: Selector '[] CDouble
contrastSelector = mkSelector "contrast"

-- | @Selector@ for @setContrast:@
setContrastSelector :: Selector '[CDouble] ()
setContrastSelector = mkSelector "setContrast:"

-- | @Selector@ for @grainIntensity@
grainIntensitySelector :: Selector '[] CDouble
grainIntensitySelector = mkSelector "grainIntensity"

-- | @Selector@ for @setGrainIntensity:@
setGrainIntensitySelector :: Selector '[CDouble] ()
setGrainIntensitySelector = mkSelector "setGrainIntensity:"

-- | @Selector@ for @grainScale@
grainScaleSelector :: Selector '[] CDouble
grainScaleSelector = mkSelector "grainScale"

-- | @Selector@ for @setGrainScale:@
setGrainScaleSelector :: Selector '[CDouble] ()
setGrainScaleSelector = mkSelector "setGrainScale:"

-- | @Selector@ for @grainIsColored@
grainIsColoredSelector :: Selector '[] Bool
grainIsColoredSelector = mkSelector "grainIsColored"

-- | @Selector@ for @setGrainIsColored:@
setGrainIsColoredSelector :: Selector '[Bool] ()
setGrainIsColoredSelector = mkSelector "setGrainIsColored:"

-- | @Selector@ for @whiteBalanceTemperature@
whiteBalanceTemperatureSelector :: Selector '[] CDouble
whiteBalanceTemperatureSelector = mkSelector "whiteBalanceTemperature"

-- | @Selector@ for @setWhiteBalanceTemperature:@
setWhiteBalanceTemperatureSelector :: Selector '[CDouble] ()
setWhiteBalanceTemperatureSelector = mkSelector "setWhiteBalanceTemperature:"

-- | @Selector@ for @whiteBalanceTint@
whiteBalanceTintSelector :: Selector '[] CDouble
whiteBalanceTintSelector = mkSelector "whiteBalanceTint"

-- | @Selector@ for @setWhiteBalanceTint:@
setWhiteBalanceTintSelector :: Selector '[CDouble] ()
setWhiteBalanceTintSelector = mkSelector "setWhiteBalanceTint:"

-- | @Selector@ for @colorGrading@
colorGradingSelector :: Selector '[] (Id SCNMaterialProperty)
colorGradingSelector = mkSelector "colorGrading"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector '[] CULong
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector '[CULong] ()
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @focalBlurRadius@
focalBlurRadiusSelector :: Selector '[] CDouble
focalBlurRadiusSelector = mkSelector "focalBlurRadius"

-- | @Selector@ for @setFocalBlurRadius:@
setFocalBlurRadiusSelector :: Selector '[CDouble] ()
setFocalBlurRadiusSelector = mkSelector "setFocalBlurRadius:"

-- | @Selector@ for @xFov@
xFovSelector :: Selector '[] CDouble
xFovSelector = mkSelector "xFov"

-- | @Selector@ for @setXFov:@
setXFovSelector :: Selector '[CDouble] ()
setXFovSelector = mkSelector "setXFov:"

-- | @Selector@ for @yFov@
yFovSelector :: Selector '[] CDouble
yFovSelector = mkSelector "yFov"

-- | @Selector@ for @setYFov:@
setYFovSelector :: Selector '[CDouble] ()
setYFovSelector = mkSelector "setYFov:"

-- | @Selector@ for @aperture@
apertureSelector :: Selector '[] CDouble
apertureSelector = mkSelector "aperture"

-- | @Selector@ for @setAperture:@
setApertureSelector :: Selector '[CDouble] ()
setApertureSelector = mkSelector "setAperture:"

-- | @Selector@ for @focalSize@
focalSizeSelector :: Selector '[] CDouble
focalSizeSelector = mkSelector "focalSize"

-- | @Selector@ for @setFocalSize:@
setFocalSizeSelector :: Selector '[CDouble] ()
setFocalSizeSelector = mkSelector "setFocalSize:"

-- | @Selector@ for @focalDistance@
focalDistanceSelector :: Selector '[] CDouble
focalDistanceSelector = mkSelector "focalDistance"

-- | @Selector@ for @setFocalDistance:@
setFocalDistanceSelector :: Selector '[CDouble] ()
setFocalDistanceSelector = mkSelector "setFocalDistance:"

