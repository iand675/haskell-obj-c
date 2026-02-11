{-# LANGUAGE PatternSynonyms #-}
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
  , cameraSelector
  , projectionTransformSelector
  , setProjectionTransformSelector
  , nameSelector
  , setNameSelector
  , fieldOfViewSelector
  , setFieldOfViewSelector
  , projectionDirectionSelector
  , setProjectionDirectionSelector
  , focalLengthSelector
  , setFocalLengthSelector
  , sensorHeightSelector
  , setSensorHeightSelector
  , zNearSelector
  , setZNearSelector
  , zFarSelector
  , setZFarSelector
  , automaticallyAdjustsZRangeSelector
  , setAutomaticallyAdjustsZRangeSelector
  , usesOrthographicProjectionSelector
  , setUsesOrthographicProjectionSelector
  , orthographicScaleSelector
  , setOrthographicScaleSelector
  , wantsDepthOfFieldSelector
  , setWantsDepthOfFieldSelector
  , focusDistanceSelector
  , setFocusDistanceSelector
  , focalBlurSampleCountSelector
  , setFocalBlurSampleCountSelector
  , fStopSelector
  , setFStopSelector
  , apertureBladeCountSelector
  , setApertureBladeCountSelector
  , motionBlurIntensitySelector
  , setMotionBlurIntensitySelector
  , screenSpaceAmbientOcclusionIntensitySelector
  , setScreenSpaceAmbientOcclusionIntensitySelector
  , screenSpaceAmbientOcclusionRadiusSelector
  , setScreenSpaceAmbientOcclusionRadiusSelector
  , screenSpaceAmbientOcclusionBiasSelector
  , setScreenSpaceAmbientOcclusionBiasSelector
  , screenSpaceAmbientOcclusionDepthThresholdSelector
  , setScreenSpaceAmbientOcclusionDepthThresholdSelector
  , screenSpaceAmbientOcclusionNormalThresholdSelector
  , setScreenSpaceAmbientOcclusionNormalThresholdSelector
  , wantsHDRSelector
  , setWantsHDRSelector
  , exposureOffsetSelector
  , setExposureOffsetSelector
  , averageGraySelector
  , setAverageGraySelector
  , whitePointSelector
  , setWhitePointSelector
  , wantsExposureAdaptationSelector
  , setWantsExposureAdaptationSelector
  , exposureAdaptationBrighteningSpeedFactorSelector
  , setExposureAdaptationBrighteningSpeedFactorSelector
  , exposureAdaptationDarkeningSpeedFactorSelector
  , setExposureAdaptationDarkeningSpeedFactorSelector
  , minimumExposureSelector
  , setMinimumExposureSelector
  , maximumExposureSelector
  , setMaximumExposureSelector
  , bloomThresholdSelector
  , setBloomThresholdSelector
  , bloomIterationCountSelector
  , setBloomIterationCountSelector
  , bloomIterationSpreadSelector
  , setBloomIterationSpreadSelector
  , bloomIntensitySelector
  , setBloomIntensitySelector
  , bloomBlurRadiusSelector
  , setBloomBlurRadiusSelector
  , vignettingPowerSelector
  , setVignettingPowerSelector
  , vignettingIntensitySelector
  , setVignettingIntensitySelector
  , colorFringeStrengthSelector
  , setColorFringeStrengthSelector
  , colorFringeIntensitySelector
  , setColorFringeIntensitySelector
  , saturationSelector
  , setSaturationSelector
  , contrastSelector
  , setContrastSelector
  , grainIntensitySelector
  , setGrainIntensitySelector
  , grainScaleSelector
  , setGrainScaleSelector
  , grainIsColoredSelector
  , setGrainIsColoredSelector
  , whiteBalanceTemperatureSelector
  , setWhiteBalanceTemperatureSelector
  , whiteBalanceTintSelector
  , setWhiteBalanceTintSelector
  , colorGradingSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector
  , focalBlurRadiusSelector
  , setFocalBlurRadiusSelector
  , xFovSelector
  , setXFovSelector
  , yFovSelector
  , setYFovSelector
  , apertureSelector
  , setApertureSelector
  , focalSizeSelector
  , setFocalSizeSelector
  , focalDistanceSelector
  , setFocalDistanceSelector

  -- * Enum types
  , SCNCameraProjectionDirection(SCNCameraProjectionDirection)
  , pattern SCNCameraProjectionDirectionVertical
  , pattern SCNCameraProjectionDirectionHorizontal

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    sendClassMsg cls' (mkSelector "camera") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- projectionTransform@
projectionTransform :: IsSCNCamera scnCamera => scnCamera -> IO SCNMatrix4
projectionTransform scnCamera  =
  sendMsgStret scnCamera (mkSelector "projectionTransform") retSCNMatrix4 []

-- | @- setProjectionTransform:@
setProjectionTransform :: IsSCNCamera scnCamera => scnCamera -> SCNMatrix4 -> IO ()
setProjectionTransform scnCamera  projectionTransform =
  sendMsg scnCamera (mkSelector "setProjectionTransform:") retVoid [argSCNMatrix4 projectionTransform]

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNCamera scnCamera => scnCamera -> IO (Id NSString)
name scnCamera  =
  sendMsg scnCamera (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNCamera scnCamera, IsNSString value) => scnCamera -> value -> IO ()
setName scnCamera  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnCamera (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | fieldOfView
--
-- Determines the receiver's field of view (in degree). Defaults to 60°. Animatable.
--
-- The fieldOfView is automatically updated when the sensorHeight or focalLength are set. Setting the fieldOfView will update the focalLength according to the new fieldOfView and the current sensorHeight.
--
-- ObjC selector: @- fieldOfView@
fieldOfView :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
fieldOfView scnCamera  =
  sendMsg scnCamera (mkSelector "fieldOfView") retCDouble []

-- | fieldOfView
--
-- Determines the receiver's field of view (in degree). Defaults to 60°. Animatable.
--
-- The fieldOfView is automatically updated when the sensorHeight or focalLength are set. Setting the fieldOfView will update the focalLength according to the new fieldOfView and the current sensorHeight.
--
-- ObjC selector: @- setFieldOfView:@
setFieldOfView :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFieldOfView scnCamera  value =
  sendMsg scnCamera (mkSelector "setFieldOfView:") retVoid [argCDouble (fromIntegral value)]

-- | projectionDirection
--
-- Determines whether the fieldOfView (or orthographicScale) is vertical or horizontal. Defaults to vertical.
--
-- ObjC selector: @- projectionDirection@
projectionDirection :: IsSCNCamera scnCamera => scnCamera -> IO SCNCameraProjectionDirection
projectionDirection scnCamera  =
  fmap (coerce :: CLong -> SCNCameraProjectionDirection) $ sendMsg scnCamera (mkSelector "projectionDirection") retCLong []

-- | projectionDirection
--
-- Determines whether the fieldOfView (or orthographicScale) is vertical or horizontal. Defaults to vertical.
--
-- ObjC selector: @- setProjectionDirection:@
setProjectionDirection :: IsSCNCamera scnCamera => scnCamera -> SCNCameraProjectionDirection -> IO ()
setProjectionDirection scnCamera  value =
  sendMsg scnCamera (mkSelector "setProjectionDirection:") retVoid [argCLong (coerce value)]

-- | focalLength
--
-- Determines the receiver's focal length in millimeter. Defaults to 50mm. Animatable.
--
-- The focalLength is automatically updated when the sensorHeight or fieldOfView are set. Setting the focalLength will update the fieldOfView according to the new focalLength and the current sensorHeight.
--
-- ObjC selector: @- focalLength@
focalLength :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalLength scnCamera  =
  sendMsg scnCamera (mkSelector "focalLength") retCDouble []

-- | focalLength
--
-- Determines the receiver's focal length in millimeter. Defaults to 50mm. Animatable.
--
-- The focalLength is automatically updated when the sensorHeight or fieldOfView are set. Setting the focalLength will update the fieldOfView according to the new focalLength and the current sensorHeight.
--
-- ObjC selector: @- setFocalLength:@
setFocalLength :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalLength scnCamera  value =
  sendMsg scnCamera (mkSelector "setFocalLength:") retVoid [argCDouble (fromIntegral value)]

-- | sensorHeight
--
-- Determines the vertical size of the sensor in millimeter. Defaults to 24mm. Animatable.
--
-- Setting the sensorHeight will automatically update the fieldOfView according to the new sensorHeight and the current focalLength.
--
-- ObjC selector: @- sensorHeight@
sensorHeight :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
sensorHeight scnCamera  =
  sendMsg scnCamera (mkSelector "sensorHeight") retCDouble []

-- | sensorHeight
--
-- Determines the vertical size of the sensor in millimeter. Defaults to 24mm. Animatable.
--
-- Setting the sensorHeight will automatically update the fieldOfView according to the new sensorHeight and the current focalLength.
--
-- ObjC selector: @- setSensorHeight:@
setSensorHeight :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setSensorHeight scnCamera  value =
  sendMsg scnCamera (mkSelector "setSensorHeight:") retVoid [argCDouble (fromIntegral value)]

-- | zNear
--
-- Determines the receiver's near value. Animatable.
--
-- The near value determines the minimal distance between the camera and a visible surface. If a surface is closer to the camera than this minimal distance, then the surface is clipped. The near value must be different than zero. Defaults to 1.
--
-- ObjC selector: @- zNear@
zNear :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
zNear scnCamera  =
  sendMsg scnCamera (mkSelector "zNear") retCDouble []

-- | zNear
--
-- Determines the receiver's near value. Animatable.
--
-- The near value determines the minimal distance between the camera and a visible surface. If a surface is closer to the camera than this minimal distance, then the surface is clipped. The near value must be different than zero. Defaults to 1.
--
-- ObjC selector: @- setZNear:@
setZNear :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setZNear scnCamera  value =
  sendMsg scnCamera (mkSelector "setZNear:") retVoid [argCDouble (fromIntegral value)]

-- | zFar
--
-- Determines the receiver's far value. Animatable.
--
-- The far value determines the maximal distance between the camera and a visible surface. If a surface is further from the camera than this maximal distance, then the surface is clipped. Defaults to 100.
--
-- ObjC selector: @- zFar@
zFar :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
zFar scnCamera  =
  sendMsg scnCamera (mkSelector "zFar") retCDouble []

-- | zFar
--
-- Determines the receiver's far value. Animatable.
--
-- The far value determines the maximal distance between the camera and a visible surface. If a surface is further from the camera than this maximal distance, then the surface is clipped. Defaults to 100.
--
-- ObjC selector: @- setZFar:@
setZFar :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setZFar scnCamera  value =
  sendMsg scnCamera (mkSelector "setZFar:") retVoid [argCDouble (fromIntegral value)]

-- | automaticallyAdjustsZRange
--
-- Determines whether the receiver automatically adjusts the zFar value. Defaults to NO.
--
-- When set to YES, the near and far planes are automatically set to fit the bounding box of the entire scene at render time.
--
-- ObjC selector: @- automaticallyAdjustsZRange@
automaticallyAdjustsZRange :: IsSCNCamera scnCamera => scnCamera -> IO Bool
automaticallyAdjustsZRange scnCamera  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCamera (mkSelector "automaticallyAdjustsZRange") retCULong []

-- | automaticallyAdjustsZRange
--
-- Determines whether the receiver automatically adjusts the zFar value. Defaults to NO.
--
-- When set to YES, the near and far planes are automatically set to fit the bounding box of the entire scene at render time.
--
-- ObjC selector: @- setAutomaticallyAdjustsZRange:@
setAutomaticallyAdjustsZRange :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setAutomaticallyAdjustsZRange scnCamera  value =
  sendMsg scnCamera (mkSelector "setAutomaticallyAdjustsZRange:") retVoid [argCULong (if value then 1 else 0)]

-- | usesOrthographicProjection
--
-- Determines whether the receiver uses an orthographic projection or not. Defaults to NO.
--
-- ObjC selector: @- usesOrthographicProjection@
usesOrthographicProjection :: IsSCNCamera scnCamera => scnCamera -> IO Bool
usesOrthographicProjection scnCamera  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCamera (mkSelector "usesOrthographicProjection") retCULong []

-- | usesOrthographicProjection
--
-- Determines whether the receiver uses an orthographic projection or not. Defaults to NO.
--
-- ObjC selector: @- setUsesOrthographicProjection:@
setUsesOrthographicProjection :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setUsesOrthographicProjection scnCamera  value =
  sendMsg scnCamera (mkSelector "setUsesOrthographicProjection:") retVoid [argCULong (if value then 1 else 0)]

-- | orthographicScale
--
-- Determines the receiver's orthographic scale value. Animatable. Defaults to 1.
--
-- This setting determines the size of the camera's visible area. This is only enabled when usesOrthographicProjection is set to YES.
--
-- ObjC selector: @- orthographicScale@
orthographicScale :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
orthographicScale scnCamera  =
  sendMsg scnCamera (mkSelector "orthographicScale") retCDouble []

-- | orthographicScale
--
-- Determines the receiver's orthographic scale value. Animatable. Defaults to 1.
--
-- This setting determines the size of the camera's visible area. This is only enabled when usesOrthographicProjection is set to YES.
--
-- ObjC selector: @- setOrthographicScale:@
setOrthographicScale :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setOrthographicScale scnCamera  value =
  sendMsg scnCamera (mkSelector "setOrthographicScale:") retVoid [argCDouble (fromIntegral value)]

-- | wantsDepthOfField
--
-- Determines if the receiver has depth of field. Defaults to NO.
--
-- ObjC selector: @- wantsDepthOfField@
wantsDepthOfField :: IsSCNCamera scnCamera => scnCamera -> IO Bool
wantsDepthOfField scnCamera  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCamera (mkSelector "wantsDepthOfField") retCULong []

-- | wantsDepthOfField
--
-- Determines if the receiver has depth of field. Defaults to NO.
--
-- ObjC selector: @- setWantsDepthOfField:@
setWantsDepthOfField :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setWantsDepthOfField scnCamera  value =
  sendMsg scnCamera (mkSelector "setWantsDepthOfField:") retVoid [argCULong (if value then 1 else 0)]

-- | focusDistance
--
-- Determines the receiver's focus distance. Animatable.
--
-- Defaults to 2.5
--
-- ObjC selector: @- focusDistance@
focusDistance :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focusDistance scnCamera  =
  sendMsg scnCamera (mkSelector "focusDistance") retCDouble []

-- | focusDistance
--
-- Determines the receiver's focus distance. Animatable.
--
-- Defaults to 2.5
--
-- ObjC selector: @- setFocusDistance:@
setFocusDistance :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocusDistance scnCamera  value =
  sendMsg scnCamera (mkSelector "setFocusDistance:") retVoid [argCDouble (fromIntegral value)]

-- | focalBlurSampleCount
--
-- Determines the receiver's sample count for depth of field effect.
--
-- Defaults to 25.
--
-- ObjC selector: @- focalBlurSampleCount@
focalBlurSampleCount :: IsSCNCamera scnCamera => scnCamera -> IO CLong
focalBlurSampleCount scnCamera  =
  sendMsg scnCamera (mkSelector "focalBlurSampleCount") retCLong []

-- | focalBlurSampleCount
--
-- Determines the receiver's sample count for depth of field effect.
--
-- Defaults to 25.
--
-- ObjC selector: @- setFocalBlurSampleCount:@
setFocalBlurSampleCount :: IsSCNCamera scnCamera => scnCamera -> CLong -> IO ()
setFocalBlurSampleCount scnCamera  value =
  sendMsg scnCamera (mkSelector "setFocalBlurSampleCount:") retVoid [argCLong (fromIntegral value)]

-- | fStop
--
-- Determines the receiver's fstop. Animatable.
--
-- Defaults to 5.6.
--
-- ObjC selector: @- fStop@
fStop :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
fStop scnCamera  =
  sendMsg scnCamera (mkSelector "fStop") retCDouble []

-- | fStop
--
-- Determines the receiver's fstop. Animatable.
--
-- Defaults to 5.6.
--
-- ObjC selector: @- setFStop:@
setFStop :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFStop scnCamera  value =
  sendMsg scnCamera (mkSelector "setFStop:") retVoid [argCDouble (fromIntegral value)]

-- | apertureBladeCount
--
-- Determines the receiver's blade count of the aperture.
--
-- Defaults to 6.
--
-- ObjC selector: @- apertureBladeCount@
apertureBladeCount :: IsSCNCamera scnCamera => scnCamera -> IO CLong
apertureBladeCount scnCamera  =
  sendMsg scnCamera (mkSelector "apertureBladeCount") retCLong []

-- | apertureBladeCount
--
-- Determines the receiver's blade count of the aperture.
--
-- Defaults to 6.
--
-- ObjC selector: @- setApertureBladeCount:@
setApertureBladeCount :: IsSCNCamera scnCamera => scnCamera -> CLong -> IO ()
setApertureBladeCount scnCamera  value =
  sendMsg scnCamera (mkSelector "setApertureBladeCount:") retVoid [argCLong (fromIntegral value)]

-- | motionBlurIntensity
--
-- Determines the intensity of the motion blur. Animatable. Defaults to 0.
--
-- An intensity of zero means no motion blur. The intensity should not exceeed 1.
--
-- ObjC selector: @- motionBlurIntensity@
motionBlurIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
motionBlurIntensity scnCamera  =
  sendMsg scnCamera (mkSelector "motionBlurIntensity") retCDouble []

-- | motionBlurIntensity
--
-- Determines the intensity of the motion blur. Animatable. Defaults to 0.
--
-- An intensity of zero means no motion blur. The intensity should not exceeed 1.
--
-- ObjC selector: @- setMotionBlurIntensity:@
setMotionBlurIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setMotionBlurIntensity scnCamera  value =
  sendMsg scnCamera (mkSelector "setMotionBlurIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | screenSpaceAmbientOcclusionIntensity
--
-- Determines the intensity of the screen space ambient occlusion. Animatable.
--
-- defaults to 0.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionIntensity@
screenSpaceAmbientOcclusionIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionIntensity scnCamera  =
  sendMsg scnCamera (mkSelector "screenSpaceAmbientOcclusionIntensity") retCDouble []

-- | screenSpaceAmbientOcclusionIntensity
--
-- Determines the intensity of the screen space ambient occlusion. Animatable.
--
-- defaults to 0.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionIntensity:@
setScreenSpaceAmbientOcclusionIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionIntensity scnCamera  value =
  sendMsg scnCamera (mkSelector "setScreenSpaceAmbientOcclusionIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | screenSpaceAmbientOcclusionRadius
--
-- Determines the screen space ambient occlusion radius in scene unit. Animatable.
--
-- defaults to 5.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionRadius@
screenSpaceAmbientOcclusionRadius :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionRadius scnCamera  =
  sendMsg scnCamera (mkSelector "screenSpaceAmbientOcclusionRadius") retCDouble []

-- | screenSpaceAmbientOcclusionRadius
--
-- Determines the screen space ambient occlusion radius in scene unit. Animatable.
--
-- defaults to 5.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionRadius:@
setScreenSpaceAmbientOcclusionRadius :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionRadius scnCamera  value =
  sendMsg scnCamera (mkSelector "setScreenSpaceAmbientOcclusionRadius:") retVoid [argCDouble (fromIntegral value)]

-- | screenSpaceAmbientOcclusionBias
--
-- Determines self occlusion bias in scene unit.
--
-- defaults to 0.03.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionBias@
screenSpaceAmbientOcclusionBias :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionBias scnCamera  =
  sendMsg scnCamera (mkSelector "screenSpaceAmbientOcclusionBias") retCDouble []

-- | screenSpaceAmbientOcclusionBias
--
-- Determines self occlusion bias in scene unit.
--
-- defaults to 0.03.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionBias:@
setScreenSpaceAmbientOcclusionBias :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionBias scnCamera  value =
  sendMsg scnCamera (mkSelector "setScreenSpaceAmbientOcclusionBias:") retVoid [argCDouble (fromIntegral value)]

-- | screenSpaceAmbientOcclusionDepthThreshold
--
-- Determines the depth blur threshold in scene unit.
--
-- defaults to 0.2.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionDepthThreshold@
screenSpaceAmbientOcclusionDepthThreshold :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionDepthThreshold scnCamera  =
  sendMsg scnCamera (mkSelector "screenSpaceAmbientOcclusionDepthThreshold") retCDouble []

-- | screenSpaceAmbientOcclusionDepthThreshold
--
-- Determines the depth blur threshold in scene unit.
--
-- defaults to 0.2.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionDepthThreshold:@
setScreenSpaceAmbientOcclusionDepthThreshold :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionDepthThreshold scnCamera  value =
  sendMsg scnCamera (mkSelector "setScreenSpaceAmbientOcclusionDepthThreshold:") retVoid [argCDouble (fromIntegral value)]

-- | screenSpaceAmbientOcclusionNormalThreshold
--
-- Determines the normal blur threshold.
--
-- defaults to 0.3.
--
-- ObjC selector: @- screenSpaceAmbientOcclusionNormalThreshold@
screenSpaceAmbientOcclusionNormalThreshold :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
screenSpaceAmbientOcclusionNormalThreshold scnCamera  =
  sendMsg scnCamera (mkSelector "screenSpaceAmbientOcclusionNormalThreshold") retCDouble []

-- | screenSpaceAmbientOcclusionNormalThreshold
--
-- Determines the normal blur threshold.
--
-- defaults to 0.3.
--
-- ObjC selector: @- setScreenSpaceAmbientOcclusionNormalThreshold:@
setScreenSpaceAmbientOcclusionNormalThreshold :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setScreenSpaceAmbientOcclusionNormalThreshold scnCamera  value =
  sendMsg scnCamera (mkSelector "setScreenSpaceAmbientOcclusionNormalThreshold:") retVoid [argCDouble (fromIntegral value)]

-- | wantsHDR
--
-- Determines if the receiver has a high dynamic range. Defaults to NO.
--
-- ObjC selector: @- wantsHDR@
wantsHDR :: IsSCNCamera scnCamera => scnCamera -> IO Bool
wantsHDR scnCamera  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCamera (mkSelector "wantsHDR") retCULong []

-- | wantsHDR
--
-- Determines if the receiver has a high dynamic range. Defaults to NO.
--
-- ObjC selector: @- setWantsHDR:@
setWantsHDR :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setWantsHDR scnCamera  value =
  sendMsg scnCamera (mkSelector "setWantsHDR:") retVoid [argCULong (if value then 1 else 0)]

-- | exposureOffset
--
-- Determines the logarithmic exposure biasing, in EV. Defaults to 0.
--
-- ObjC selector: @- exposureOffset@
exposureOffset :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
exposureOffset scnCamera  =
  sendMsg scnCamera (mkSelector "exposureOffset") retCDouble []

-- | exposureOffset
--
-- Determines the logarithmic exposure biasing, in EV. Defaults to 0.
--
-- ObjC selector: @- setExposureOffset:@
setExposureOffset :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setExposureOffset scnCamera  value =
  sendMsg scnCamera (mkSelector "setExposureOffset:") retVoid [argCDouble (fromIntegral value)]

-- | averageGray
--
-- Determines the average gray level desired in the final image. Defaults to 0.18.
--
-- ObjC selector: @- averageGray@
averageGray :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
averageGray scnCamera  =
  sendMsg scnCamera (mkSelector "averageGray") retCDouble []

-- | averageGray
--
-- Determines the average gray level desired in the final image. Defaults to 0.18.
--
-- ObjC selector: @- setAverageGray:@
setAverageGray :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setAverageGray scnCamera  value =
  sendMsg scnCamera (mkSelector "setAverageGray:") retVoid [argCDouble (fromIntegral value)]

-- | whitePoint
--
-- Determines the smallest luminance level that will be mapped to white in the final image. Defaults to 1.
--
-- ObjC selector: @- whitePoint@
whitePoint :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
whitePoint scnCamera  =
  sendMsg scnCamera (mkSelector "whitePoint") retCDouble []

-- | whitePoint
--
-- Determines the smallest luminance level that will be mapped to white in the final image. Defaults to 1.
--
-- ObjC selector: @- setWhitePoint:@
setWhitePoint :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setWhitePoint scnCamera  value =
  sendMsg scnCamera (mkSelector "setWhitePoint:") retVoid [argCDouble (fromIntegral value)]

-- | wantsExposureAdaptation
--
-- Determines if the receiver should simulate an eye and continuously adjust to luminance. Defaults to YES.
--
-- ObjC selector: @- wantsExposureAdaptation@
wantsExposureAdaptation :: IsSCNCamera scnCamera => scnCamera -> IO Bool
wantsExposureAdaptation scnCamera  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCamera (mkSelector "wantsExposureAdaptation") retCULong []

-- | wantsExposureAdaptation
--
-- Determines if the receiver should simulate an eye and continuously adjust to luminance. Defaults to YES.
--
-- ObjC selector: @- setWantsExposureAdaptation:@
setWantsExposureAdaptation :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setWantsExposureAdaptation scnCamera  value =
  sendMsg scnCamera (mkSelector "setWantsExposureAdaptation:") retVoid [argCULong (if value then 1 else 0)]

-- | exposureAdaptationBrighteningSpeedFactor
--
-- Determines the exposure adaptation speed when going from bright areas to dark areas. Defaults to 0.4.
--
-- ObjC selector: @- exposureAdaptationBrighteningSpeedFactor@
exposureAdaptationBrighteningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
exposureAdaptationBrighteningSpeedFactor scnCamera  =
  sendMsg scnCamera (mkSelector "exposureAdaptationBrighteningSpeedFactor") retCDouble []

-- | exposureAdaptationBrighteningSpeedFactor
--
-- Determines the exposure adaptation speed when going from bright areas to dark areas. Defaults to 0.4.
--
-- ObjC selector: @- setExposureAdaptationBrighteningSpeedFactor:@
setExposureAdaptationBrighteningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setExposureAdaptationBrighteningSpeedFactor scnCamera  value =
  sendMsg scnCamera (mkSelector "setExposureAdaptationBrighteningSpeedFactor:") retVoid [argCDouble (fromIntegral value)]

-- | exposureAdaptationDarkeningSpeedFactor
--
-- Determines the exposure adaptation speed when going from dark areas to bright areas. Defaults to 0.6.
--
-- ObjC selector: @- exposureAdaptationDarkeningSpeedFactor@
exposureAdaptationDarkeningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
exposureAdaptationDarkeningSpeedFactor scnCamera  =
  sendMsg scnCamera (mkSelector "exposureAdaptationDarkeningSpeedFactor") retCDouble []

-- | exposureAdaptationDarkeningSpeedFactor
--
-- Determines the exposure adaptation speed when going from dark areas to bright areas. Defaults to 0.6.
--
-- ObjC selector: @- setExposureAdaptationDarkeningSpeedFactor:@
setExposureAdaptationDarkeningSpeedFactor :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setExposureAdaptationDarkeningSpeedFactor scnCamera  value =
  sendMsg scnCamera (mkSelector "setExposureAdaptationDarkeningSpeedFactor:") retVoid [argCDouble (fromIntegral value)]

-- | minimumExposure
--
-- Determines the minimum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- minimumExposure@
minimumExposure :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
minimumExposure scnCamera  =
  sendMsg scnCamera (mkSelector "minimumExposure") retCDouble []

-- | minimumExposure
--
-- Determines the minimum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- setMinimumExposure:@
setMinimumExposure :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setMinimumExposure scnCamera  value =
  sendMsg scnCamera (mkSelector "setMinimumExposure:") retVoid [argCDouble (fromIntegral value)]

-- | maximumExposure
--
-- Determines the maximum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- maximumExposure@
maximumExposure :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
maximumExposure scnCamera  =
  sendMsg scnCamera (mkSelector "maximumExposure") retCDouble []

-- | maximumExposure
--
-- Determines the maximum exposure offset of the adaptation, in EV. Defaults to -15.
--
-- ObjC selector: @- setMaximumExposure:@
setMaximumExposure :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setMaximumExposure scnCamera  value =
  sendMsg scnCamera (mkSelector "setMaximumExposure:") retVoid [argCDouble (fromIntegral value)]

-- | bloomThreshold
--
-- Determines the luminance threshold for the bloom effect. Animatable. Defaults to 1.
--
-- ObjC selector: @- bloomThreshold@
bloomThreshold :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomThreshold scnCamera  =
  sendMsg scnCamera (mkSelector "bloomThreshold") retCDouble []

-- | bloomThreshold
--
-- Determines the luminance threshold for the bloom effect. Animatable. Defaults to 1.
--
-- ObjC selector: @- setBloomThreshold:@
setBloomThreshold :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomThreshold scnCamera  value =
  sendMsg scnCamera (mkSelector "setBloomThreshold:") retVoid [argCDouble (fromIntegral value)]

-- | bloomIteration
--
-- Determines the number of blur iterations. Defaults to 1.
--
-- ObjC selector: @- bloomIterationCount@
bloomIterationCount :: IsSCNCamera scnCamera => scnCamera -> IO CLong
bloomIterationCount scnCamera  =
  sendMsg scnCamera (mkSelector "bloomIterationCount") retCLong []

-- | bloomIteration
--
-- Determines the number of blur iterations. Defaults to 1.
--
-- ObjC selector: @- setBloomIterationCount:@
setBloomIterationCount :: IsSCNCamera scnCamera => scnCamera -> CLong -> IO ()
setBloomIterationCount scnCamera  value =
  sendMsg scnCamera (mkSelector "setBloomIterationCount:") retVoid [argCLong (fromIntegral value)]

-- | bloomIterationSpread
--
-- Determines how the bloom iterations are spread. Defaults to 0.
--
-- ObjC selector: @- bloomIterationSpread@
bloomIterationSpread :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomIterationSpread scnCamera  =
  sendMsg scnCamera (mkSelector "bloomIterationSpread") retCDouble []

-- | bloomIterationSpread
--
-- Determines how the bloom iterations are spread. Defaults to 0.
--
-- ObjC selector: @- setBloomIterationSpread:@
setBloomIterationSpread :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomIterationSpread scnCamera  value =
  sendMsg scnCamera (mkSelector "setBloomIterationSpread:") retVoid [argCDouble (fromIntegral value)]

-- | bloomIntensity
--
-- Determines the intensity of the bloom effect. Animatable. Defaults to 0 (no effect).
--
-- ObjC selector: @- bloomIntensity@
bloomIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomIntensity scnCamera  =
  sendMsg scnCamera (mkSelector "bloomIntensity") retCDouble []

-- | bloomIntensity
--
-- Determines the intensity of the bloom effect. Animatable. Defaults to 0 (no effect).
--
-- ObjC selector: @- setBloomIntensity:@
setBloomIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomIntensity scnCamera  value =
  sendMsg scnCamera (mkSelector "setBloomIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | bloomBlurRadius
--
-- Determines the radius of the bloom effect in points. Animatable. Defaults to 4.
--
-- ObjC selector: @- bloomBlurRadius@
bloomBlurRadius :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
bloomBlurRadius scnCamera  =
  sendMsg scnCamera (mkSelector "bloomBlurRadius") retCDouble []

-- | bloomBlurRadius
--
-- Determines the radius of the bloom effect in points. Animatable. Defaults to 4.
--
-- ObjC selector: @- setBloomBlurRadius:@
setBloomBlurRadius :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setBloomBlurRadius scnCamera  value =
  sendMsg scnCamera (mkSelector "setBloomBlurRadius:") retVoid [argCDouble (fromIntegral value)]

-- | vignettingPower
--
-- Controls the shape of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- vignettingPower@
vignettingPower :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
vignettingPower scnCamera  =
  sendMsg scnCamera (mkSelector "vignettingPower") retCDouble []

-- | vignettingPower
--
-- Controls the shape of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- setVignettingPower:@
setVignettingPower :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setVignettingPower scnCamera  value =
  sendMsg scnCamera (mkSelector "setVignettingPower:") retVoid [argCDouble (fromIntegral value)]

-- | vignettingIntensity
--
-- Controls the intensity of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- vignettingIntensity@
vignettingIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
vignettingIntensity scnCamera  =
  sendMsg scnCamera (mkSelector "vignettingIntensity") retCDouble []

-- | vignettingIntensity
--
-- Controls the intensity of the vignetting effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- setVignettingIntensity:@
setVignettingIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setVignettingIntensity scnCamera  value =
  sendMsg scnCamera (mkSelector "setVignettingIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | colorFringeStrength
--
-- Controls the strength of the color shift effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- colorFringeStrength@
colorFringeStrength :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
colorFringeStrength scnCamera  =
  sendMsg scnCamera (mkSelector "colorFringeStrength") retCDouble []

-- | colorFringeStrength
--
-- Controls the strength of the color shift effect. Defaults to 0 (no effect).
--
-- ObjC selector: @- setColorFringeStrength:@
setColorFringeStrength :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setColorFringeStrength scnCamera  value =
  sendMsg scnCamera (mkSelector "setColorFringeStrength:") retVoid [argCDouble (fromIntegral value)]

-- | colorFringeIntensity
--
-- Controls the intensity of the color shift effect. Defaults to 1.
--
-- ObjC selector: @- colorFringeIntensity@
colorFringeIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
colorFringeIntensity scnCamera  =
  sendMsg scnCamera (mkSelector "colorFringeIntensity") retCDouble []

-- | colorFringeIntensity
--
-- Controls the intensity of the color shift effect. Defaults to 1.
--
-- ObjC selector: @- setColorFringeIntensity:@
setColorFringeIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setColorFringeIntensity scnCamera  value =
  sendMsg scnCamera (mkSelector "setColorFringeIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | saturation
--
-- Controls the overall saturation of the scene. Defaults to 1 (no effect).
--
-- ObjC selector: @- saturation@
saturation :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
saturation scnCamera  =
  sendMsg scnCamera (mkSelector "saturation") retCDouble []

-- | saturation
--
-- Controls the overall saturation of the scene. Defaults to 1 (no effect).
--
-- ObjC selector: @- setSaturation:@
setSaturation :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setSaturation scnCamera  value =
  sendMsg scnCamera (mkSelector "setSaturation:") retVoid [argCDouble (fromIntegral value)]

-- | contrast
--
-- Controls the overall contrast of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- contrast@
contrast :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
contrast scnCamera  =
  sendMsg scnCamera (mkSelector "contrast") retCDouble []

-- | contrast
--
-- Controls the overall contrast of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- setContrast:@
setContrast :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setContrast scnCamera  value =
  sendMsg scnCamera (mkSelector "setContrast:") retVoid [argCDouble (fromIntegral value)]

-- | grainIntensity
--
-- Controls the intensity of the grain. Defaults to 0 (no effect).
--
-- ObjC selector: @- grainIntensity@
grainIntensity :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
grainIntensity scnCamera  =
  sendMsg scnCamera (mkSelector "grainIntensity") retCDouble []

-- | grainIntensity
--
-- Controls the intensity of the grain. Defaults to 0 (no effect).
--
-- ObjC selector: @- setGrainIntensity:@
setGrainIntensity :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setGrainIntensity scnCamera  value =
  sendMsg scnCamera (mkSelector "setGrainIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | grainScale
--
-- Controls the scale of the grain. Defaults to 1.
--
-- ObjC selector: @- grainScale@
grainScale :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
grainScale scnCamera  =
  sendMsg scnCamera (mkSelector "grainScale") retCDouble []

-- | grainScale
--
-- Controls the scale of the grain. Defaults to 1.
--
-- ObjC selector: @- setGrainScale:@
setGrainScale :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setGrainScale scnCamera  value =
  sendMsg scnCamera (mkSelector "setGrainScale:") retVoid [argCDouble (fromIntegral value)]

-- | grainIsColored
--
-- Determines if the grain is colored or not. Defaults to NO.
--
-- ObjC selector: @- grainIsColored@
grainIsColored :: IsSCNCamera scnCamera => scnCamera -> IO Bool
grainIsColored scnCamera  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCamera (mkSelector "grainIsColored") retCULong []

-- | grainIsColored
--
-- Determines if the grain is colored or not. Defaults to NO.
--
-- ObjC selector: @- setGrainIsColored:@
setGrainIsColored :: IsSCNCamera scnCamera => scnCamera -> Bool -> IO ()
setGrainIsColored scnCamera  value =
  sendMsg scnCamera (mkSelector "setGrainIsColored:") retVoid [argCULong (if value then 1 else 0)]

-- | whiteBalanceTemperature
--
-- Controls the overall white balance temperature of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- whiteBalanceTemperature@
whiteBalanceTemperature :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
whiteBalanceTemperature scnCamera  =
  sendMsg scnCamera (mkSelector "whiteBalanceTemperature") retCDouble []

-- | whiteBalanceTemperature
--
-- Controls the overall white balance temperature of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- setWhiteBalanceTemperature:@
setWhiteBalanceTemperature :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setWhiteBalanceTemperature scnCamera  value =
  sendMsg scnCamera (mkSelector "setWhiteBalanceTemperature:") retVoid [argCDouble (fromIntegral value)]

-- | whiteBalanceTint
--
-- Controls the overall white balance tint of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- whiteBalanceTint@
whiteBalanceTint :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
whiteBalanceTint scnCamera  =
  sendMsg scnCamera (mkSelector "whiteBalanceTint") retCDouble []

-- | whiteBalanceTint
--
-- Controls the overall white balance tint of the scene. Defaults to 0 (no effect).
--
-- ObjC selector: @- setWhiteBalanceTint:@
setWhiteBalanceTint :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setWhiteBalanceTint scnCamera  value =
  sendMsg scnCamera (mkSelector "setWhiteBalanceTint:") retVoid [argCDouble (fromIntegral value)]

-- | colorGrading
--
-- Specifies a lookup texture to apply color grading. The contents must a 2D image representing @n@ slices of a unit color cube texture, arranged in an horizontal row of @n@ images. For instance, a color cube of dimension 16x16x16 should be provided as an image of size 256x16.
--
-- ObjC selector: @- colorGrading@
colorGrading :: IsSCNCamera scnCamera => scnCamera -> IO (Id SCNMaterialProperty)
colorGrading scnCamera  =
  sendMsg scnCamera (mkSelector "colorGrading") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | categoryBitMask
--
-- Determines the node categories that are visible from the receiver. Defaults to all bits set.
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSCNCamera scnCamera => scnCamera -> IO CULong
categoryBitMask scnCamera  =
  sendMsg scnCamera (mkSelector "categoryBitMask") retCULong []

-- | categoryBitMask
--
-- Determines the node categories that are visible from the receiver. Defaults to all bits set.
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNCamera scnCamera => scnCamera -> CULong -> IO ()
setCategoryBitMask scnCamera  value =
  sendMsg scnCamera (mkSelector "setCategoryBitMask:") retVoid [argCULong (fromIntegral value)]

-- | focalBlurRadius
--
-- Determines the receiver's focal radius. Animatable.
--
-- Determines the maximum amount of blur for objects out of focus. Defaults to 0.
--
-- ObjC selector: @- focalBlurRadius@
focalBlurRadius :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalBlurRadius scnCamera  =
  sendMsg scnCamera (mkSelector "focalBlurRadius") retCDouble []

-- | focalBlurRadius
--
-- Determines the receiver's focal radius. Animatable.
--
-- Determines the maximum amount of blur for objects out of focus. Defaults to 0.
--
-- ObjC selector: @- setFocalBlurRadius:@
setFocalBlurRadius :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalBlurRadius scnCamera  value =
  sendMsg scnCamera (mkSelector "setFocalBlurRadius:") retVoid [argCDouble (fromIntegral value)]

-- | xFov
--
-- Determines the receiver's field of view on the X axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- xFov@
xFov :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
xFov scnCamera  =
  sendMsg scnCamera (mkSelector "xFov") retCDouble []

-- | xFov
--
-- Determines the receiver's field of view on the X axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- setXFov:@
setXFov :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setXFov scnCamera  value =
  sendMsg scnCamera (mkSelector "setXFov:") retVoid [argCDouble (fromIntegral value)]

-- | yFov
--
-- Determines the receiver's field of view on the Y axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- yFov@
yFov :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
yFov scnCamera  =
  sendMsg scnCamera (mkSelector "yFov") retCDouble []

-- | yFov
--
-- Determines the receiver's field of view on the Y axis (in degree). Animatable.
--
-- When both xFov and yFov are null an yFov of 60° is used. When both are set, the one that best fits the renderer's aspect ratio is used. When only one is set, it is used. Defaults to 0.
--
-- ObjC selector: @- setYFov:@
setYFov :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setYFov scnCamera  value =
  sendMsg scnCamera (mkSelector "setYFov:") retVoid [argCDouble (fromIntegral value)]

-- | aperture
--
-- Determines the receiver's aperture. Animatable.
--
-- Defaults to 1/8.0.
--
-- ObjC selector: @- aperture@
aperture :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
aperture scnCamera  =
  sendMsg scnCamera (mkSelector "aperture") retCDouble []

-- | aperture
--
-- Determines the receiver's aperture. Animatable.
--
-- Defaults to 1/8.0.
--
-- ObjC selector: @- setAperture:@
setAperture :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setAperture scnCamera  value =
  sendMsg scnCamera (mkSelector "setAperture:") retVoid [argCDouble (fromIntegral value)]

-- | focalSize
--
-- Determines the receiver's focal size. Animatable.
--
-- Determines the size of the area around focalDistance where the objects are in focus. Defaults to 0.
--
-- ObjC selector: @- focalSize@
focalSize :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalSize scnCamera  =
  sendMsg scnCamera (mkSelector "focalSize") retCDouble []

-- | focalSize
--
-- Determines the receiver's focal size. Animatable.
--
-- Determines the size of the area around focalDistance where the objects are in focus. Defaults to 0.
--
-- ObjC selector: @- setFocalSize:@
setFocalSize :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalSize scnCamera  value =
  sendMsg scnCamera (mkSelector "setFocalSize:") retVoid [argCDouble (fromIntegral value)]

-- | focalDistance
--
-- Determines the receiver's focal distance. Animatable.
--
-- When non zero, the focal distance determines how the camera focuses the objects in the 3d scene. Defaults to 10.0 prior to macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to 2.5 otherwise.
--
-- ObjC selector: @- focalDistance@
focalDistance :: IsSCNCamera scnCamera => scnCamera -> IO CDouble
focalDistance scnCamera  =
  sendMsg scnCamera (mkSelector "focalDistance") retCDouble []

-- | focalDistance
--
-- Determines the receiver's focal distance. Animatable.
--
-- When non zero, the focal distance determines how the camera focuses the objects in the 3d scene. Defaults to 10.0 prior to macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to 2.5 otherwise.
--
-- ObjC selector: @- setFocalDistance:@
setFocalDistance :: IsSCNCamera scnCamera => scnCamera -> CDouble -> IO ()
setFocalDistance scnCamera  value =
  sendMsg scnCamera (mkSelector "setFocalDistance:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @camera@
cameraSelector :: Selector
cameraSelector = mkSelector "camera"

-- | @Selector@ for @projectionTransform@
projectionTransformSelector :: Selector
projectionTransformSelector = mkSelector "projectionTransform"

-- | @Selector@ for @setProjectionTransform:@
setProjectionTransformSelector :: Selector
setProjectionTransformSelector = mkSelector "setProjectionTransform:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @fieldOfView@
fieldOfViewSelector :: Selector
fieldOfViewSelector = mkSelector "fieldOfView"

-- | @Selector@ for @setFieldOfView:@
setFieldOfViewSelector :: Selector
setFieldOfViewSelector = mkSelector "setFieldOfView:"

-- | @Selector@ for @projectionDirection@
projectionDirectionSelector :: Selector
projectionDirectionSelector = mkSelector "projectionDirection"

-- | @Selector@ for @setProjectionDirection:@
setProjectionDirectionSelector :: Selector
setProjectionDirectionSelector = mkSelector "setProjectionDirection:"

-- | @Selector@ for @focalLength@
focalLengthSelector :: Selector
focalLengthSelector = mkSelector "focalLength"

-- | @Selector@ for @setFocalLength:@
setFocalLengthSelector :: Selector
setFocalLengthSelector = mkSelector "setFocalLength:"

-- | @Selector@ for @sensorHeight@
sensorHeightSelector :: Selector
sensorHeightSelector = mkSelector "sensorHeight"

-- | @Selector@ for @setSensorHeight:@
setSensorHeightSelector :: Selector
setSensorHeightSelector = mkSelector "setSensorHeight:"

-- | @Selector@ for @zNear@
zNearSelector :: Selector
zNearSelector = mkSelector "zNear"

-- | @Selector@ for @setZNear:@
setZNearSelector :: Selector
setZNearSelector = mkSelector "setZNear:"

-- | @Selector@ for @zFar@
zFarSelector :: Selector
zFarSelector = mkSelector "zFar"

-- | @Selector@ for @setZFar:@
setZFarSelector :: Selector
setZFarSelector = mkSelector "setZFar:"

-- | @Selector@ for @automaticallyAdjustsZRange@
automaticallyAdjustsZRangeSelector :: Selector
automaticallyAdjustsZRangeSelector = mkSelector "automaticallyAdjustsZRange"

-- | @Selector@ for @setAutomaticallyAdjustsZRange:@
setAutomaticallyAdjustsZRangeSelector :: Selector
setAutomaticallyAdjustsZRangeSelector = mkSelector "setAutomaticallyAdjustsZRange:"

-- | @Selector@ for @usesOrthographicProjection@
usesOrthographicProjectionSelector :: Selector
usesOrthographicProjectionSelector = mkSelector "usesOrthographicProjection"

-- | @Selector@ for @setUsesOrthographicProjection:@
setUsesOrthographicProjectionSelector :: Selector
setUsesOrthographicProjectionSelector = mkSelector "setUsesOrthographicProjection:"

-- | @Selector@ for @orthographicScale@
orthographicScaleSelector :: Selector
orthographicScaleSelector = mkSelector "orthographicScale"

-- | @Selector@ for @setOrthographicScale:@
setOrthographicScaleSelector :: Selector
setOrthographicScaleSelector = mkSelector "setOrthographicScale:"

-- | @Selector@ for @wantsDepthOfField@
wantsDepthOfFieldSelector :: Selector
wantsDepthOfFieldSelector = mkSelector "wantsDepthOfField"

-- | @Selector@ for @setWantsDepthOfField:@
setWantsDepthOfFieldSelector :: Selector
setWantsDepthOfFieldSelector = mkSelector "setWantsDepthOfField:"

-- | @Selector@ for @focusDistance@
focusDistanceSelector :: Selector
focusDistanceSelector = mkSelector "focusDistance"

-- | @Selector@ for @setFocusDistance:@
setFocusDistanceSelector :: Selector
setFocusDistanceSelector = mkSelector "setFocusDistance:"

-- | @Selector@ for @focalBlurSampleCount@
focalBlurSampleCountSelector :: Selector
focalBlurSampleCountSelector = mkSelector "focalBlurSampleCount"

-- | @Selector@ for @setFocalBlurSampleCount:@
setFocalBlurSampleCountSelector :: Selector
setFocalBlurSampleCountSelector = mkSelector "setFocalBlurSampleCount:"

-- | @Selector@ for @fStop@
fStopSelector :: Selector
fStopSelector = mkSelector "fStop"

-- | @Selector@ for @setFStop:@
setFStopSelector :: Selector
setFStopSelector = mkSelector "setFStop:"

-- | @Selector@ for @apertureBladeCount@
apertureBladeCountSelector :: Selector
apertureBladeCountSelector = mkSelector "apertureBladeCount"

-- | @Selector@ for @setApertureBladeCount:@
setApertureBladeCountSelector :: Selector
setApertureBladeCountSelector = mkSelector "setApertureBladeCount:"

-- | @Selector@ for @motionBlurIntensity@
motionBlurIntensitySelector :: Selector
motionBlurIntensitySelector = mkSelector "motionBlurIntensity"

-- | @Selector@ for @setMotionBlurIntensity:@
setMotionBlurIntensitySelector :: Selector
setMotionBlurIntensitySelector = mkSelector "setMotionBlurIntensity:"

-- | @Selector@ for @screenSpaceAmbientOcclusionIntensity@
screenSpaceAmbientOcclusionIntensitySelector :: Selector
screenSpaceAmbientOcclusionIntensitySelector = mkSelector "screenSpaceAmbientOcclusionIntensity"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionIntensity:@
setScreenSpaceAmbientOcclusionIntensitySelector :: Selector
setScreenSpaceAmbientOcclusionIntensitySelector = mkSelector "setScreenSpaceAmbientOcclusionIntensity:"

-- | @Selector@ for @screenSpaceAmbientOcclusionRadius@
screenSpaceAmbientOcclusionRadiusSelector :: Selector
screenSpaceAmbientOcclusionRadiusSelector = mkSelector "screenSpaceAmbientOcclusionRadius"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionRadius:@
setScreenSpaceAmbientOcclusionRadiusSelector :: Selector
setScreenSpaceAmbientOcclusionRadiusSelector = mkSelector "setScreenSpaceAmbientOcclusionRadius:"

-- | @Selector@ for @screenSpaceAmbientOcclusionBias@
screenSpaceAmbientOcclusionBiasSelector :: Selector
screenSpaceAmbientOcclusionBiasSelector = mkSelector "screenSpaceAmbientOcclusionBias"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionBias:@
setScreenSpaceAmbientOcclusionBiasSelector :: Selector
setScreenSpaceAmbientOcclusionBiasSelector = mkSelector "setScreenSpaceAmbientOcclusionBias:"

-- | @Selector@ for @screenSpaceAmbientOcclusionDepthThreshold@
screenSpaceAmbientOcclusionDepthThresholdSelector :: Selector
screenSpaceAmbientOcclusionDepthThresholdSelector = mkSelector "screenSpaceAmbientOcclusionDepthThreshold"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionDepthThreshold:@
setScreenSpaceAmbientOcclusionDepthThresholdSelector :: Selector
setScreenSpaceAmbientOcclusionDepthThresholdSelector = mkSelector "setScreenSpaceAmbientOcclusionDepthThreshold:"

-- | @Selector@ for @screenSpaceAmbientOcclusionNormalThreshold@
screenSpaceAmbientOcclusionNormalThresholdSelector :: Selector
screenSpaceAmbientOcclusionNormalThresholdSelector = mkSelector "screenSpaceAmbientOcclusionNormalThreshold"

-- | @Selector@ for @setScreenSpaceAmbientOcclusionNormalThreshold:@
setScreenSpaceAmbientOcclusionNormalThresholdSelector :: Selector
setScreenSpaceAmbientOcclusionNormalThresholdSelector = mkSelector "setScreenSpaceAmbientOcclusionNormalThreshold:"

-- | @Selector@ for @wantsHDR@
wantsHDRSelector :: Selector
wantsHDRSelector = mkSelector "wantsHDR"

-- | @Selector@ for @setWantsHDR:@
setWantsHDRSelector :: Selector
setWantsHDRSelector = mkSelector "setWantsHDR:"

-- | @Selector@ for @exposureOffset@
exposureOffsetSelector :: Selector
exposureOffsetSelector = mkSelector "exposureOffset"

-- | @Selector@ for @setExposureOffset:@
setExposureOffsetSelector :: Selector
setExposureOffsetSelector = mkSelector "setExposureOffset:"

-- | @Selector@ for @averageGray@
averageGraySelector :: Selector
averageGraySelector = mkSelector "averageGray"

-- | @Selector@ for @setAverageGray:@
setAverageGraySelector :: Selector
setAverageGraySelector = mkSelector "setAverageGray:"

-- | @Selector@ for @whitePoint@
whitePointSelector :: Selector
whitePointSelector = mkSelector "whitePoint"

-- | @Selector@ for @setWhitePoint:@
setWhitePointSelector :: Selector
setWhitePointSelector = mkSelector "setWhitePoint:"

-- | @Selector@ for @wantsExposureAdaptation@
wantsExposureAdaptationSelector :: Selector
wantsExposureAdaptationSelector = mkSelector "wantsExposureAdaptation"

-- | @Selector@ for @setWantsExposureAdaptation:@
setWantsExposureAdaptationSelector :: Selector
setWantsExposureAdaptationSelector = mkSelector "setWantsExposureAdaptation:"

-- | @Selector@ for @exposureAdaptationBrighteningSpeedFactor@
exposureAdaptationBrighteningSpeedFactorSelector :: Selector
exposureAdaptationBrighteningSpeedFactorSelector = mkSelector "exposureAdaptationBrighteningSpeedFactor"

-- | @Selector@ for @setExposureAdaptationBrighteningSpeedFactor:@
setExposureAdaptationBrighteningSpeedFactorSelector :: Selector
setExposureAdaptationBrighteningSpeedFactorSelector = mkSelector "setExposureAdaptationBrighteningSpeedFactor:"

-- | @Selector@ for @exposureAdaptationDarkeningSpeedFactor@
exposureAdaptationDarkeningSpeedFactorSelector :: Selector
exposureAdaptationDarkeningSpeedFactorSelector = mkSelector "exposureAdaptationDarkeningSpeedFactor"

-- | @Selector@ for @setExposureAdaptationDarkeningSpeedFactor:@
setExposureAdaptationDarkeningSpeedFactorSelector :: Selector
setExposureAdaptationDarkeningSpeedFactorSelector = mkSelector "setExposureAdaptationDarkeningSpeedFactor:"

-- | @Selector@ for @minimumExposure@
minimumExposureSelector :: Selector
minimumExposureSelector = mkSelector "minimumExposure"

-- | @Selector@ for @setMinimumExposure:@
setMinimumExposureSelector :: Selector
setMinimumExposureSelector = mkSelector "setMinimumExposure:"

-- | @Selector@ for @maximumExposure@
maximumExposureSelector :: Selector
maximumExposureSelector = mkSelector "maximumExposure"

-- | @Selector@ for @setMaximumExposure:@
setMaximumExposureSelector :: Selector
setMaximumExposureSelector = mkSelector "setMaximumExposure:"

-- | @Selector@ for @bloomThreshold@
bloomThresholdSelector :: Selector
bloomThresholdSelector = mkSelector "bloomThreshold"

-- | @Selector@ for @setBloomThreshold:@
setBloomThresholdSelector :: Selector
setBloomThresholdSelector = mkSelector "setBloomThreshold:"

-- | @Selector@ for @bloomIterationCount@
bloomIterationCountSelector :: Selector
bloomIterationCountSelector = mkSelector "bloomIterationCount"

-- | @Selector@ for @setBloomIterationCount:@
setBloomIterationCountSelector :: Selector
setBloomIterationCountSelector = mkSelector "setBloomIterationCount:"

-- | @Selector@ for @bloomIterationSpread@
bloomIterationSpreadSelector :: Selector
bloomIterationSpreadSelector = mkSelector "bloomIterationSpread"

-- | @Selector@ for @setBloomIterationSpread:@
setBloomIterationSpreadSelector :: Selector
setBloomIterationSpreadSelector = mkSelector "setBloomIterationSpread:"

-- | @Selector@ for @bloomIntensity@
bloomIntensitySelector :: Selector
bloomIntensitySelector = mkSelector "bloomIntensity"

-- | @Selector@ for @setBloomIntensity:@
setBloomIntensitySelector :: Selector
setBloomIntensitySelector = mkSelector "setBloomIntensity:"

-- | @Selector@ for @bloomBlurRadius@
bloomBlurRadiusSelector :: Selector
bloomBlurRadiusSelector = mkSelector "bloomBlurRadius"

-- | @Selector@ for @setBloomBlurRadius:@
setBloomBlurRadiusSelector :: Selector
setBloomBlurRadiusSelector = mkSelector "setBloomBlurRadius:"

-- | @Selector@ for @vignettingPower@
vignettingPowerSelector :: Selector
vignettingPowerSelector = mkSelector "vignettingPower"

-- | @Selector@ for @setVignettingPower:@
setVignettingPowerSelector :: Selector
setVignettingPowerSelector = mkSelector "setVignettingPower:"

-- | @Selector@ for @vignettingIntensity@
vignettingIntensitySelector :: Selector
vignettingIntensitySelector = mkSelector "vignettingIntensity"

-- | @Selector@ for @setVignettingIntensity:@
setVignettingIntensitySelector :: Selector
setVignettingIntensitySelector = mkSelector "setVignettingIntensity:"

-- | @Selector@ for @colorFringeStrength@
colorFringeStrengthSelector :: Selector
colorFringeStrengthSelector = mkSelector "colorFringeStrength"

-- | @Selector@ for @setColorFringeStrength:@
setColorFringeStrengthSelector :: Selector
setColorFringeStrengthSelector = mkSelector "setColorFringeStrength:"

-- | @Selector@ for @colorFringeIntensity@
colorFringeIntensitySelector :: Selector
colorFringeIntensitySelector = mkSelector "colorFringeIntensity"

-- | @Selector@ for @setColorFringeIntensity:@
setColorFringeIntensitySelector :: Selector
setColorFringeIntensitySelector = mkSelector "setColorFringeIntensity:"

-- | @Selector@ for @saturation@
saturationSelector :: Selector
saturationSelector = mkSelector "saturation"

-- | @Selector@ for @setSaturation:@
setSaturationSelector :: Selector
setSaturationSelector = mkSelector "setSaturation:"

-- | @Selector@ for @contrast@
contrastSelector :: Selector
contrastSelector = mkSelector "contrast"

-- | @Selector@ for @setContrast:@
setContrastSelector :: Selector
setContrastSelector = mkSelector "setContrast:"

-- | @Selector@ for @grainIntensity@
grainIntensitySelector :: Selector
grainIntensitySelector = mkSelector "grainIntensity"

-- | @Selector@ for @setGrainIntensity:@
setGrainIntensitySelector :: Selector
setGrainIntensitySelector = mkSelector "setGrainIntensity:"

-- | @Selector@ for @grainScale@
grainScaleSelector :: Selector
grainScaleSelector = mkSelector "grainScale"

-- | @Selector@ for @setGrainScale:@
setGrainScaleSelector :: Selector
setGrainScaleSelector = mkSelector "setGrainScale:"

-- | @Selector@ for @grainIsColored@
grainIsColoredSelector :: Selector
grainIsColoredSelector = mkSelector "grainIsColored"

-- | @Selector@ for @setGrainIsColored:@
setGrainIsColoredSelector :: Selector
setGrainIsColoredSelector = mkSelector "setGrainIsColored:"

-- | @Selector@ for @whiteBalanceTemperature@
whiteBalanceTemperatureSelector :: Selector
whiteBalanceTemperatureSelector = mkSelector "whiteBalanceTemperature"

-- | @Selector@ for @setWhiteBalanceTemperature:@
setWhiteBalanceTemperatureSelector :: Selector
setWhiteBalanceTemperatureSelector = mkSelector "setWhiteBalanceTemperature:"

-- | @Selector@ for @whiteBalanceTint@
whiteBalanceTintSelector :: Selector
whiteBalanceTintSelector = mkSelector "whiteBalanceTint"

-- | @Selector@ for @setWhiteBalanceTint:@
setWhiteBalanceTintSelector :: Selector
setWhiteBalanceTintSelector = mkSelector "setWhiteBalanceTint:"

-- | @Selector@ for @colorGrading@
colorGradingSelector :: Selector
colorGradingSelector = mkSelector "colorGrading"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @focalBlurRadius@
focalBlurRadiusSelector :: Selector
focalBlurRadiusSelector = mkSelector "focalBlurRadius"

-- | @Selector@ for @setFocalBlurRadius:@
setFocalBlurRadiusSelector :: Selector
setFocalBlurRadiusSelector = mkSelector "setFocalBlurRadius:"

-- | @Selector@ for @xFov@
xFovSelector :: Selector
xFovSelector = mkSelector "xFov"

-- | @Selector@ for @setXFov:@
setXFovSelector :: Selector
setXFovSelector = mkSelector "setXFov:"

-- | @Selector@ for @yFov@
yFovSelector :: Selector
yFovSelector = mkSelector "yFov"

-- | @Selector@ for @setYFov:@
setYFovSelector :: Selector
setYFovSelector = mkSelector "setYFov:"

-- | @Selector@ for @aperture@
apertureSelector :: Selector
apertureSelector = mkSelector "aperture"

-- | @Selector@ for @setAperture:@
setApertureSelector :: Selector
setApertureSelector = mkSelector "setAperture:"

-- | @Selector@ for @focalSize@
focalSizeSelector :: Selector
focalSizeSelector = mkSelector "focalSize"

-- | @Selector@ for @setFocalSize:@
setFocalSizeSelector :: Selector
setFocalSizeSelector = mkSelector "setFocalSize:"

-- | @Selector@ for @focalDistance@
focalDistanceSelector :: Selector
focalDistanceSelector = mkSelector "focalDistance"

-- | @Selector@ for @setFocalDistance:@
setFocalDistanceSelector :: Selector
setFocalDistanceSelector = mkSelector "setFocalDistance:"

