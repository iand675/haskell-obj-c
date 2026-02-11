{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNLight
--
-- SCNLight represents a light that can be attached to a SCNNode.
--
-- Generated bindings for @SCNLight@.
module ObjC.SceneKit.SCNLight
  ( SCNLight
  , IsSCNLight(..)
  , light
  , attributeForKey
  , setAttribute_forKey
  , type_
  , setType
  , color
  , setColor
  , temperature
  , setTemperature
  , intensity
  , setIntensity
  , name
  , setName
  , castsShadow
  , setCastsShadow
  , shadowColor
  , setShadowColor
  , shadowRadius
  , setShadowRadius
  , shadowSampleCount
  , setShadowSampleCount
  , shadowMode
  , setShadowMode
  , shadowBias
  , setShadowBias
  , automaticallyAdjustsShadowProjection
  , setAutomaticallyAdjustsShadowProjection
  , maximumShadowDistance
  , setMaximumShadowDistance
  , forcesBackFaceCasters
  , setForcesBackFaceCasters
  , sampleDistributedShadowMaps
  , setSampleDistributedShadowMaps
  , shadowCascadeCount
  , setShadowCascadeCount
  , shadowCascadeSplittingFactor
  , setShadowCascadeSplittingFactor
  , orthographicScale
  , setOrthographicScale
  , zNear
  , setZNear
  , zFar
  , setZFar
  , attenuationStartDistance
  , setAttenuationStartDistance
  , attenuationEndDistance
  , setAttenuationEndDistance
  , attenuationFalloffExponent
  , setAttenuationFalloffExponent
  , spotInnerAngle
  , setSpotInnerAngle
  , spotOuterAngle
  , setSpotOuterAngle
  , iesProfileURL
  , setIESProfileURL
  , sphericalHarmonicsCoefficients
  , probeType
  , setProbeType
  , probeUpdateType
  , setProbeUpdateType
  , parallaxCorrectionEnabled
  , setParallaxCorrectionEnabled
  , probeEnvironment
  , areaType
  , setAreaType
  , areaPolygonVertices
  , setAreaPolygonVertices
  , drawsArea
  , setDrawsArea
  , doubleSided
  , setDoubleSided
  , categoryBitMask
  , setCategoryBitMask
  , lightSelector
  , attributeForKeySelector
  , setAttribute_forKeySelector
  , typeSelector
  , setTypeSelector
  , colorSelector
  , setColorSelector
  , temperatureSelector
  , setTemperatureSelector
  , intensitySelector
  , setIntensitySelector
  , nameSelector
  , setNameSelector
  , castsShadowSelector
  , setCastsShadowSelector
  , shadowColorSelector
  , setShadowColorSelector
  , shadowRadiusSelector
  , setShadowRadiusSelector
  , shadowSampleCountSelector
  , setShadowSampleCountSelector
  , shadowModeSelector
  , setShadowModeSelector
  , shadowBiasSelector
  , setShadowBiasSelector
  , automaticallyAdjustsShadowProjectionSelector
  , setAutomaticallyAdjustsShadowProjectionSelector
  , maximumShadowDistanceSelector
  , setMaximumShadowDistanceSelector
  , forcesBackFaceCastersSelector
  , setForcesBackFaceCastersSelector
  , sampleDistributedShadowMapsSelector
  , setSampleDistributedShadowMapsSelector
  , shadowCascadeCountSelector
  , setShadowCascadeCountSelector
  , shadowCascadeSplittingFactorSelector
  , setShadowCascadeSplittingFactorSelector
  , orthographicScaleSelector
  , setOrthographicScaleSelector
  , zNearSelector
  , setZNearSelector
  , zFarSelector
  , setZFarSelector
  , attenuationStartDistanceSelector
  , setAttenuationStartDistanceSelector
  , attenuationEndDistanceSelector
  , setAttenuationEndDistanceSelector
  , attenuationFalloffExponentSelector
  , setAttenuationFalloffExponentSelector
  , spotInnerAngleSelector
  , setSpotInnerAngleSelector
  , spotOuterAngleSelector
  , setSpotOuterAngleSelector
  , iesProfileURLSelector
  , setIESProfileURLSelector
  , sphericalHarmonicsCoefficientsSelector
  , probeTypeSelector
  , setProbeTypeSelector
  , probeUpdateTypeSelector
  , setProbeUpdateTypeSelector
  , parallaxCorrectionEnabledSelector
  , setParallaxCorrectionEnabledSelector
  , probeEnvironmentSelector
  , areaTypeSelector
  , setAreaTypeSelector
  , areaPolygonVerticesSelector
  , setAreaPolygonVerticesSelector
  , drawsAreaSelector
  , setDrawsAreaSelector
  , doubleSidedSelector
  , setDoubleSidedSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector

  -- * Enum types
  , SCNLightAreaType(SCNLightAreaType)
  , pattern SCNLightAreaTypeRectangle
  , pattern SCNLightAreaTypePolygon
  , SCNLightProbeType(SCNLightProbeType)
  , pattern SCNLightProbeTypeIrradiance
  , pattern SCNLightProbeTypeRadiance
  , SCNLightProbeUpdateType(SCNLightProbeUpdateType)
  , pattern SCNLightProbeUpdateTypeNever
  , pattern SCNLightProbeUpdateTypeRealtime
  , SCNShadowMode(SCNShadowMode)
  , pattern SCNShadowModeForward
  , pattern SCNShadowModeDeferred
  , pattern SCNShadowModeModulated

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
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | light
--
-- Creates and returns a light instance.
--
-- ObjC selector: @+ light@
light :: IO (Id SCNLight)
light  =
  do
    cls' <- getRequiredClass "SCNLight"
    sendClassMsg cls' (mkSelector "light") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | attributeForKey:
--
-- @key@ — The key for which to return the corresponding attribute.
--
-- Returns the attribute for the specified key. The valid keys are described in the "Light Attributes" constants.
--
-- ObjC selector: @- attributeForKey:@
attributeForKey :: (IsSCNLight scnLight, IsNSString key) => scnLight -> key -> IO RawId
attributeForKey scnLight  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg scnLight (mkSelector "attributeForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | setAttribute:forKey:
--
-- @attribute@ — The attribute for the property identified by key.
--
-- @key@ — The name of a property.
--
-- Set the specified attribute for the specified key. The valid keys are described in the "Light Attributes" constants.
--
-- ObjC selector: @- setAttribute:forKey:@
setAttribute_forKey :: (IsSCNLight scnLight, IsNSString key) => scnLight -> RawId -> key -> IO ()
setAttribute_forKey scnLight  attribute key =
withObjCPtr key $ \raw_key ->
    sendMsg scnLight (mkSelector "setAttribute:forKey:") retVoid [argPtr (castPtr (unRawId attribute) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | type
--
-- Specifies the receiver's type.
--
-- Defaults to SCNLightTypeOmni on iOS 8 and later, and on macOS 10.10 and later (otherwise defaults to SCNLightTypeAmbient).
--
-- ObjC selector: @- type@
type_ :: IsSCNLight scnLight => scnLight -> IO (Id NSString)
type_ scnLight  =
  sendMsg scnLight (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | type
--
-- Specifies the receiver's type.
--
-- Defaults to SCNLightTypeOmni on iOS 8 and later, and on macOS 10.10 and later (otherwise defaults to SCNLightTypeAmbient).
--
-- ObjC selector: @- setType:@
setType :: (IsSCNLight scnLight, IsNSString value) => scnLight -> value -> IO ()
setType scnLight  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnLight (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | color
--
-- Specifies the receiver's color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor. The renderer multiplies the light's color is by the color derived from the light's temperature.
--
-- ObjC selector: @- color@
color :: IsSCNLight scnLight => scnLight -> IO RawId
color scnLight  =
  fmap (RawId . castPtr) $ sendMsg scnLight (mkSelector "color") (retPtr retVoid) []

-- | color
--
-- Specifies the receiver's color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor. The renderer multiplies the light's color is by the color derived from the light's temperature.
--
-- ObjC selector: @- setColor:@
setColor :: IsSCNLight scnLight => scnLight -> RawId -> IO ()
setColor scnLight  value =
  sendMsg scnLight (mkSelector "setColor:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | temperature
--
-- Specifies the receiver's temperature.
--
-- This specifies the temperature of the light in Kelvin. The renderer multiplies the light's color by the color derived from the light's temperature. Defaults to 6500 (pure white). Animatable.
--
-- ObjC selector: @- temperature@
temperature :: IsSCNLight scnLight => scnLight -> IO CDouble
temperature scnLight  =
  sendMsg scnLight (mkSelector "temperature") retCDouble []

-- | temperature
--
-- Specifies the receiver's temperature.
--
-- This specifies the temperature of the light in Kelvin. The renderer multiplies the light's color by the color derived from the light's temperature. Defaults to 6500 (pure white). Animatable.
--
-- ObjC selector: @- setTemperature:@
setTemperature :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setTemperature scnLight  value =
  sendMsg scnLight (mkSelector "setTemperature:") retVoid [argCDouble (fromIntegral value)]

-- | intensity
--
-- Specifies the receiver's intensity.
--
-- This intensity is used to modulate the light color. When used with a physically-based material, this corresponds to the luminous flux of the light, expressed in lumens (lm). Defaults to 1000. Animatable.
--
-- ObjC selector: @- intensity@
intensity :: IsSCNLight scnLight => scnLight -> IO CDouble
intensity scnLight  =
  sendMsg scnLight (mkSelector "intensity") retCDouble []

-- | intensity
--
-- Specifies the receiver's intensity.
--
-- This intensity is used to modulate the light color. When used with a physically-based material, this corresponds to the luminous flux of the light, expressed in lumens (lm). Defaults to 1000. Animatable.
--
-- ObjC selector: @- setIntensity:@
setIntensity :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setIntensity scnLight  value =
  sendMsg scnLight (mkSelector "setIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNLight scnLight => scnLight -> IO (Id NSString)
name scnLight  =
  sendMsg scnLight (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNLight scnLight, IsNSString value) => scnLight -> value -> IO ()
setName scnLight  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnLight (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | castsShadow
--
-- Determines whether the receiver casts a shadow. Defaults to NO.
--
-- Shadows are only supported by spot and directional lights.
--
-- ObjC selector: @- castsShadow@
castsShadow :: IsSCNLight scnLight => scnLight -> IO Bool
castsShadow scnLight  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLight (mkSelector "castsShadow") retCULong []

-- | castsShadow
--
-- Determines whether the receiver casts a shadow. Defaults to NO.
--
-- Shadows are only supported by spot and directional lights.
--
-- ObjC selector: @- setCastsShadow:@
setCastsShadow :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setCastsShadow scnLight  value =
  sendMsg scnLight (mkSelector "setCastsShadow:") retVoid [argCULong (if value then 1 else 0)]

-- | shadowColor
--
-- Specifies the color (CGColorRef or NSColor) of the shadow casted by the receiver. Defaults to black. Animatable.
--
-- On iOS 9 or earlier and macOS 10.11 or earlier, this defaults to black 50% transparent.
--
-- ObjC selector: @- shadowColor@
shadowColor :: IsSCNLight scnLight => scnLight -> IO RawId
shadowColor scnLight  =
  fmap (RawId . castPtr) $ sendMsg scnLight (mkSelector "shadowColor") (retPtr retVoid) []

-- | shadowColor
--
-- Specifies the color (CGColorRef or NSColor) of the shadow casted by the receiver. Defaults to black. Animatable.
--
-- On iOS 9 or earlier and macOS 10.11 or earlier, this defaults to black 50% transparent.
--
-- ObjC selector: @- setShadowColor:@
setShadowColor :: IsSCNLight scnLight => scnLight -> RawId -> IO ()
setShadowColor scnLight  value =
  sendMsg scnLight (mkSelector "setShadowColor:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | shadowRadius
--
-- Specifies the sample radius used to render the receiver’s shadow. Default value is 3.0. Animatable.
--
-- ObjC selector: @- shadowRadius@
shadowRadius :: IsSCNLight scnLight => scnLight -> IO CDouble
shadowRadius scnLight  =
  sendMsg scnLight (mkSelector "shadowRadius") retCDouble []

-- | shadowRadius
--
-- Specifies the sample radius used to render the receiver’s shadow. Default value is 3.0. Animatable.
--
-- ObjC selector: @- setShadowRadius:@
setShadowRadius :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setShadowRadius scnLight  value =
  sendMsg scnLight (mkSelector "setShadowRadius:") retVoid [argCDouble (fromIntegral value)]

-- | shadowSampleCount
--
-- Specifies the number of sample per fragment to compute the shadow map. Defaults to 0.
--
-- On macOS 10.11 or earlier, the shadowSampleCount defaults to 16. On iOS 9 or earlier it defaults to 1.0. On macOS 10.12, iOS 10 and greater, when the shadowSampleCount is set to 0, a default sample count is chosen depending on the platform.
--
-- ObjC selector: @- shadowSampleCount@
shadowSampleCount :: IsSCNLight scnLight => scnLight -> IO CULong
shadowSampleCount scnLight  =
  sendMsg scnLight (mkSelector "shadowSampleCount") retCULong []

-- | shadowSampleCount
--
-- Specifies the number of sample per fragment to compute the shadow map. Defaults to 0.
--
-- On macOS 10.11 or earlier, the shadowSampleCount defaults to 16. On iOS 9 or earlier it defaults to 1.0. On macOS 10.12, iOS 10 and greater, when the shadowSampleCount is set to 0, a default sample count is chosen depending on the platform.
--
-- ObjC selector: @- setShadowSampleCount:@
setShadowSampleCount :: IsSCNLight scnLight => scnLight -> CULong -> IO ()
setShadowSampleCount scnLight  value =
  sendMsg scnLight (mkSelector "setShadowSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | shadowMode
--
-- Specified the mode to use to cast shadows. See above for the available modes and their description. Defaults to SCNShadowModeDefered on 10.9 and before, defaults to SCNShadowModeForward otherwise.
--
-- ObjC selector: @- shadowMode@
shadowMode :: IsSCNLight scnLight => scnLight -> IO SCNShadowMode
shadowMode scnLight  =
  fmap (coerce :: CLong -> SCNShadowMode) $ sendMsg scnLight (mkSelector "shadowMode") retCLong []

-- | shadowMode
--
-- Specified the mode to use to cast shadows. See above for the available modes and their description. Defaults to SCNShadowModeDefered on 10.9 and before, defaults to SCNShadowModeForward otherwise.
--
-- ObjC selector: @- setShadowMode:@
setShadowMode :: IsSCNLight scnLight => scnLight -> SCNShadowMode -> IO ()
setShadowMode scnLight  value =
  sendMsg scnLight (mkSelector "setShadowMode:") retVoid [argCLong (coerce value)]

-- | shadowBias
--
-- Specifies the correction to apply to the shadow map to correct acne artefacts. It is multiplied by an implementation-specific value to create a constant depth offset. Defaults to 1.0
--
-- ObjC selector: @- shadowBias@
shadowBias :: IsSCNLight scnLight => scnLight -> IO CDouble
shadowBias scnLight  =
  sendMsg scnLight (mkSelector "shadowBias") retCDouble []

-- | shadowBias
--
-- Specifies the correction to apply to the shadow map to correct acne artefacts. It is multiplied by an implementation-specific value to create a constant depth offset. Defaults to 1.0
--
-- ObjC selector: @- setShadowBias:@
setShadowBias :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setShadowBias scnLight  value =
  sendMsg scnLight (mkSelector "setShadowBias:") retVoid [argCDouble (fromIntegral value)]

-- | automaticallyAdjustsShadowProjection
--
-- Specifies if the shadow map projection should be done automatically or manually by the user. Defaults to YES.
--
-- ObjC selector: @- automaticallyAdjustsShadowProjection@
automaticallyAdjustsShadowProjection :: IsSCNLight scnLight => scnLight -> IO Bool
automaticallyAdjustsShadowProjection scnLight  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLight (mkSelector "automaticallyAdjustsShadowProjection") retCULong []

-- | automaticallyAdjustsShadowProjection
--
-- Specifies if the shadow map projection should be done automatically or manually by the user. Defaults to YES.
--
-- ObjC selector: @- setAutomaticallyAdjustsShadowProjection:@
setAutomaticallyAdjustsShadowProjection :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setAutomaticallyAdjustsShadowProjection scnLight  value =
  sendMsg scnLight (mkSelector "setAutomaticallyAdjustsShadowProjection:") retVoid [argCULong (if value then 1 else 0)]

-- | maximumShadowDistance
--
-- Specifies the maximum distance from the viewpoint from which the shadows for the receiver light won't be computed. Defaults to 100.0.
--
-- ObjC selector: @- maximumShadowDistance@
maximumShadowDistance :: IsSCNLight scnLight => scnLight -> IO CDouble
maximumShadowDistance scnLight  =
  sendMsg scnLight (mkSelector "maximumShadowDistance") retCDouble []

-- | maximumShadowDistance
--
-- Specifies the maximum distance from the viewpoint from which the shadows for the receiver light won't be computed. Defaults to 100.0.
--
-- ObjC selector: @- setMaximumShadowDistance:@
setMaximumShadowDistance :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setMaximumShadowDistance scnLight  value =
  sendMsg scnLight (mkSelector "setMaximumShadowDistance:") retVoid [argCDouble (fromIntegral value)]

-- | forcesBackFaceCasters
--
-- Render only back faces of the shadow caster when enabled. Defaults to NO. This is a behavior change from previous releases.
--
-- ObjC selector: @- forcesBackFaceCasters@
forcesBackFaceCasters :: IsSCNLight scnLight => scnLight -> IO Bool
forcesBackFaceCasters scnLight  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLight (mkSelector "forcesBackFaceCasters") retCULong []

-- | forcesBackFaceCasters
--
-- Render only back faces of the shadow caster when enabled. Defaults to NO. This is a behavior change from previous releases.
--
-- ObjC selector: @- setForcesBackFaceCasters:@
setForcesBackFaceCasters :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setForcesBackFaceCasters scnLight  value =
  sendMsg scnLight (mkSelector "setForcesBackFaceCasters:") retVoid [argCULong (if value then 1 else 0)]

-- | sampleDistributedShadowMaps
--
-- Use the sample distribution of the main rendering to better fit the shadow frusta. Defaults to NO.
--
-- ObjC selector: @- sampleDistributedShadowMaps@
sampleDistributedShadowMaps :: IsSCNLight scnLight => scnLight -> IO Bool
sampleDistributedShadowMaps scnLight  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLight (mkSelector "sampleDistributedShadowMaps") retCULong []

-- | sampleDistributedShadowMaps
--
-- Use the sample distribution of the main rendering to better fit the shadow frusta. Defaults to NO.
--
-- ObjC selector: @- setSampleDistributedShadowMaps:@
setSampleDistributedShadowMaps :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setSampleDistributedShadowMaps scnLight  value =
  sendMsg scnLight (mkSelector "setSampleDistributedShadowMaps:") retVoid [argCULong (if value then 1 else 0)]

-- | shadowCascadeCount
--
-- Specifies the number of distinct shadow maps that will be computed for the receiver light. Defaults to 1. Maximum is 4.
--
-- ObjC selector: @- shadowCascadeCount@
shadowCascadeCount :: IsSCNLight scnLight => scnLight -> IO CULong
shadowCascadeCount scnLight  =
  sendMsg scnLight (mkSelector "shadowCascadeCount") retCULong []

-- | shadowCascadeCount
--
-- Specifies the number of distinct shadow maps that will be computed for the receiver light. Defaults to 1. Maximum is 4.
--
-- ObjC selector: @- setShadowCascadeCount:@
setShadowCascadeCount :: IsSCNLight scnLight => scnLight -> CULong -> IO ()
setShadowCascadeCount scnLight  value =
  sendMsg scnLight (mkSelector "setShadowCascadeCount:") retVoid [argCULong (fromIntegral value)]

-- | shadowCascadeSplittingFactor
--
-- Specifies a factor to interpolate between linear splitting (0) and logarithmic splitting (1). Defaults to 0.15.
--
-- ObjC selector: @- shadowCascadeSplittingFactor@
shadowCascadeSplittingFactor :: IsSCNLight scnLight => scnLight -> IO CDouble
shadowCascadeSplittingFactor scnLight  =
  sendMsg scnLight (mkSelector "shadowCascadeSplittingFactor") retCDouble []

-- | shadowCascadeSplittingFactor
--
-- Specifies a factor to interpolate between linear splitting (0) and logarithmic splitting (1). Defaults to 0.15.
--
-- ObjC selector: @- setShadowCascadeSplittingFactor:@
setShadowCascadeSplittingFactor :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setShadowCascadeSplittingFactor scnLight  value =
  sendMsg scnLight (mkSelector "setShadowCascadeSplittingFactor:") retVoid [argCDouble (fromIntegral value)]

-- | orthographicScale
--
-- Specifies the orthographic scale used to render from the directional light into the shadow map. Defaults to 1.
--
-- This is only applicable for directional lights.
--
-- ObjC selector: @- orthographicScale@
orthographicScale :: IsSCNLight scnLight => scnLight -> IO CDouble
orthographicScale scnLight  =
  sendMsg scnLight (mkSelector "orthographicScale") retCDouble []

-- | orthographicScale
--
-- Specifies the orthographic scale used to render from the directional light into the shadow map. Defaults to 1.
--
-- This is only applicable for directional lights.
--
-- ObjC selector: @- setOrthographicScale:@
setOrthographicScale :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setOrthographicScale scnLight  value =
  sendMsg scnLight (mkSelector "setOrthographicScale:") retVoid [argCDouble (fromIntegral value)]

-- | zNear
--
-- Specifies the minimal distance between the light and the surface to cast shadow on. If a surface is closer to the light than this minimal distance, then the surface won't be shadowed. The near value must be different than zero. Animatable. Defaults to 1.
--
-- ObjC selector: @- zNear@
zNear :: IsSCNLight scnLight => scnLight -> IO CDouble
zNear scnLight  =
  sendMsg scnLight (mkSelector "zNear") retCDouble []

-- | zNear
--
-- Specifies the minimal distance between the light and the surface to cast shadow on. If a surface is closer to the light than this minimal distance, then the surface won't be shadowed. The near value must be different than zero. Animatable. Defaults to 1.
--
-- ObjC selector: @- setZNear:@
setZNear :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setZNear scnLight  value =
  sendMsg scnLight (mkSelector "setZNear:") retVoid [argCDouble (fromIntegral value)]

-- | zFar
--
-- Specifies the maximal distance between the light and a visible surface to cast shadow on. If a surface is further from the light than this maximal distance, then the surface won't be shadowed. Animatable. Defaults to 100.
--
-- ObjC selector: @- zFar@
zFar :: IsSCNLight scnLight => scnLight -> IO CDouble
zFar scnLight  =
  sendMsg scnLight (mkSelector "zFar") retCDouble []

-- | zFar
--
-- Specifies the maximal distance between the light and a visible surface to cast shadow on. If a surface is further from the light than this maximal distance, then the surface won't be shadowed. Animatable. Defaults to 100.
--
-- ObjC selector: @- setZFar:@
setZFar :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setZFar scnLight  value =
  sendMsg scnLight (mkSelector "setZFar:") retVoid [argCDouble (fromIntegral value)]

-- | attenuationStartDistance
--
-- The distance at which the attenuation starts (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- attenuationStartDistance@
attenuationStartDistance :: IsSCNLight scnLight => scnLight -> IO CDouble
attenuationStartDistance scnLight  =
  sendMsg scnLight (mkSelector "attenuationStartDistance") retCDouble []

-- | attenuationStartDistance
--
-- The distance at which the attenuation starts (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- setAttenuationStartDistance:@
setAttenuationStartDistance :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setAttenuationStartDistance scnLight  value =
  sendMsg scnLight (mkSelector "setAttenuationStartDistance:") retVoid [argCDouble (fromIntegral value)]

-- | attenuationEndDistance
--
-- The distance at which the attenuation ends (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- attenuationEndDistance@
attenuationEndDistance :: IsSCNLight scnLight => scnLight -> IO CDouble
attenuationEndDistance scnLight  =
  sendMsg scnLight (mkSelector "attenuationEndDistance") retCDouble []

-- | attenuationEndDistance
--
-- The distance at which the attenuation ends (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- setAttenuationEndDistance:@
setAttenuationEndDistance :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setAttenuationEndDistance scnLight  value =
  sendMsg scnLight (mkSelector "setAttenuationEndDistance:") retVoid [argCDouble (fromIntegral value)]

-- | attenuationFalloffExponent
--
-- Specifies the attenuation between the start and end attenuation distances. 0 means a constant attenuation, 1 a linear attenuation and 2 a quadratic attenuation, but any positive value will work (Omni or Spot light types only). Animatable. Defaults to 2.
--
-- ObjC selector: @- attenuationFalloffExponent@
attenuationFalloffExponent :: IsSCNLight scnLight => scnLight -> IO CDouble
attenuationFalloffExponent scnLight  =
  sendMsg scnLight (mkSelector "attenuationFalloffExponent") retCDouble []

-- | attenuationFalloffExponent
--
-- Specifies the attenuation between the start and end attenuation distances. 0 means a constant attenuation, 1 a linear attenuation and 2 a quadratic attenuation, but any positive value will work (Omni or Spot light types only). Animatable. Defaults to 2.
--
-- ObjC selector: @- setAttenuationFalloffExponent:@
setAttenuationFalloffExponent :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setAttenuationFalloffExponent scnLight  value =
  sendMsg scnLight (mkSelector "setAttenuationFalloffExponent:") retVoid [argCDouble (fromIntegral value)]

-- | spotInnerAngle
--
-- The angle in degrees between the spot direction and the lit element below which the lighting is at full strength. Animatable. Defaults to 0.
--
-- ObjC selector: @- spotInnerAngle@
spotInnerAngle :: IsSCNLight scnLight => scnLight -> IO CDouble
spotInnerAngle scnLight  =
  sendMsg scnLight (mkSelector "spotInnerAngle") retCDouble []

-- | spotInnerAngle
--
-- The angle in degrees between the spot direction and the lit element below which the lighting is at full strength. Animatable. Defaults to 0.
--
-- ObjC selector: @- setSpotInnerAngle:@
setSpotInnerAngle :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setSpotInnerAngle scnLight  value =
  sendMsg scnLight (mkSelector "setSpotInnerAngle:") retVoid [argCDouble (fromIntegral value)]

-- | spotOuterAngle
--
-- The angle in degrees between the spot direction and the lit element after which the lighting is at zero strength. Animatable. Defaults to 45 degrees.
--
-- ObjC selector: @- spotOuterAngle@
spotOuterAngle :: IsSCNLight scnLight => scnLight -> IO CDouble
spotOuterAngle scnLight  =
  sendMsg scnLight (mkSelector "spotOuterAngle") retCDouble []

-- | spotOuterAngle
--
-- The angle in degrees between the spot direction and the lit element after which the lighting is at zero strength. Animatable. Defaults to 45 degrees.
--
-- ObjC selector: @- setSpotOuterAngle:@
setSpotOuterAngle :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setSpotOuterAngle scnLight  value =
  sendMsg scnLight (mkSelector "setSpotOuterAngle:") retVoid [argCDouble (fromIntegral value)]

-- | IESProfileURL
--
-- Specifies the IES file from which the shape, direction, and intensity of illumination is determined. Defaults to nil.
--
-- ObjC selector: @- IESProfileURL@
iesProfileURL :: IsSCNLight scnLight => scnLight -> IO (Id NSURL)
iesProfileURL scnLight  =
  sendMsg scnLight (mkSelector "IESProfileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | IESProfileURL
--
-- Specifies the IES file from which the shape, direction, and intensity of illumination is determined. Defaults to nil.
--
-- ObjC selector: @- setIESProfileURL:@
setIESProfileURL :: (IsSCNLight scnLight, IsNSURL value) => scnLight -> value -> IO ()
setIESProfileURL scnLight  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnLight (mkSelector "setIESProfileURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sphericalHarmonicsCoefficients
--
-- The receiver's spherical harmonics coefficients.
--
-- Currently spherical harmonics are only supported by light probes (SCNLightTypeProbe). The data is an array of 27 32-bit floating-point values, containing three non-interleaved data sets corresponding to the red, green, and blue sets of coefficients.
--
-- ObjC selector: @- sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficients :: IsSCNLight scnLight => scnLight -> IO (Id NSData)
sphericalHarmonicsCoefficients scnLight  =
  sendMsg scnLight (mkSelector "sphericalHarmonicsCoefficients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- probeType@
probeType :: IsSCNLight scnLight => scnLight -> IO SCNLightProbeType
probeType scnLight  =
  fmap (coerce :: CLong -> SCNLightProbeType) $ sendMsg scnLight (mkSelector "probeType") retCLong []

-- | @- setProbeType:@
setProbeType :: IsSCNLight scnLight => scnLight -> SCNLightProbeType -> IO ()
setProbeType scnLight  value =
  sendMsg scnLight (mkSelector "setProbeType:") retVoid [argCLong (coerce value)]

-- | @- probeUpdateType@
probeUpdateType :: IsSCNLight scnLight => scnLight -> IO SCNLightProbeUpdateType
probeUpdateType scnLight  =
  fmap (coerce :: CLong -> SCNLightProbeUpdateType) $ sendMsg scnLight (mkSelector "probeUpdateType") retCLong []

-- | @- setProbeUpdateType:@
setProbeUpdateType :: IsSCNLight scnLight => scnLight -> SCNLightProbeUpdateType -> IO ()
setProbeUpdateType scnLight  value =
  sendMsg scnLight (mkSelector "setProbeUpdateType:") retVoid [argCLong (coerce value)]

-- | @- parallaxCorrectionEnabled@
parallaxCorrectionEnabled :: IsSCNLight scnLight => scnLight -> IO Bool
parallaxCorrectionEnabled scnLight  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLight (mkSelector "parallaxCorrectionEnabled") retCULong []

-- | @- setParallaxCorrectionEnabled:@
setParallaxCorrectionEnabled :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setParallaxCorrectionEnabled scnLight  value =
  sendMsg scnLight (mkSelector "setParallaxCorrectionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- probeEnvironment@
probeEnvironment :: IsSCNLight scnLight => scnLight -> IO (Id SCNMaterialProperty)
probeEnvironment scnLight  =
  sendMsg scnLight (mkSelector "probeEnvironment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | areaType
--
-- Determines the shape of a light of type SCNLightTypeArea. Defaults to SCNLightAreaTypeRectangle.
--
-- ObjC selector: @- areaType@
areaType :: IsSCNLight scnLight => scnLight -> IO SCNLightAreaType
areaType scnLight  =
  fmap (coerce :: CLong -> SCNLightAreaType) $ sendMsg scnLight (mkSelector "areaType") retCLong []

-- | areaType
--
-- Determines the shape of a light of type SCNLightTypeArea. Defaults to SCNLightAreaTypeRectangle.
--
-- ObjC selector: @- setAreaType:@
setAreaType :: IsSCNLight scnLight => scnLight -> SCNLightAreaType -> IO ()
setAreaType scnLight  value =
  sendMsg scnLight (mkSelector "setAreaType:") retVoid [argCLong (coerce value)]

-- | areaPolygonVertices
--
-- Determines the shape of light of an area light of type SCNLightAreaTypePolygon. Defaults nil.
--
-- An array of CGPoint values corresponding to the coordinates of the polygon's vertices in the XY plane.
--
-- ObjC selector: @- areaPolygonVertices@
areaPolygonVertices :: IsSCNLight scnLight => scnLight -> IO (Id NSArray)
areaPolygonVertices scnLight  =
  sendMsg scnLight (mkSelector "areaPolygonVertices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | areaPolygonVertices
--
-- Determines the shape of light of an area light of type SCNLightAreaTypePolygon. Defaults nil.
--
-- An array of CGPoint values corresponding to the coordinates of the polygon's vertices in the XY plane.
--
-- ObjC selector: @- setAreaPolygonVertices:@
setAreaPolygonVertices :: (IsSCNLight scnLight, IsNSArray value) => scnLight -> value -> IO ()
setAreaPolygonVertices scnLight  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnLight (mkSelector "setAreaPolygonVertices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | drawsArea
--
-- Determines whether the shape of a light of type SCNLightTypeArea is drawn in the scene. Defaults to YES.
--
-- ObjC selector: @- drawsArea@
drawsArea :: IsSCNLight scnLight => scnLight -> IO Bool
drawsArea scnLight  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLight (mkSelector "drawsArea") retCULong []

-- | drawsArea
--
-- Determines whether the shape of a light of type SCNLightTypeArea is drawn in the scene. Defaults to YES.
--
-- ObjC selector: @- setDrawsArea:@
setDrawsArea :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setDrawsArea scnLight  value =
  sendMsg scnLight (mkSelector "setDrawsArea:") retVoid [argCULong (if value then 1 else 0)]

-- | doubleSided
--
-- Determines whether a light of type SCNLightTypeArea is double-sided. Defaults NO.
--
-- Area lights of type SCNLightAreaTypeRectangle or SCNLightAreaTypePolygon emit light along the -Z axis. When set to YES, they also emit light along the +Z axis.
--
-- ObjC selector: @- doubleSided@
doubleSided :: IsSCNLight scnLight => scnLight -> IO Bool
doubleSided scnLight  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLight (mkSelector "doubleSided") retCULong []

-- | doubleSided
--
-- Determines whether a light of type SCNLightTypeArea is double-sided. Defaults NO.
--
-- Area lights of type SCNLightAreaTypeRectangle or SCNLightAreaTypePolygon emit light along the -Z axis. When set to YES, they also emit light along the +Z axis.
--
-- ObjC selector: @- setDoubleSided:@
setDoubleSided :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setDoubleSided scnLight  value =
  sendMsg scnLight (mkSelector "setDoubleSided:") retVoid [argCULong (if value then 1 else 0)]

-- | categoryBitMask
--
-- Determines the node categories that will be lit by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSCNLight scnLight => scnLight -> IO CULong
categoryBitMask scnLight  =
  sendMsg scnLight (mkSelector "categoryBitMask") retCULong []

-- | categoryBitMask
--
-- Determines the node categories that will be lit by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNLight scnLight => scnLight -> CULong -> IO ()
setCategoryBitMask scnLight  value =
  sendMsg scnLight (mkSelector "setCategoryBitMask:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @light@
lightSelector :: Selector
lightSelector = mkSelector "light"

-- | @Selector@ for @attributeForKey:@
attributeForKeySelector :: Selector
attributeForKeySelector = mkSelector "attributeForKey:"

-- | @Selector@ for @setAttribute:forKey:@
setAttribute_forKeySelector :: Selector
setAttribute_forKeySelector = mkSelector "setAttribute:forKey:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector
temperatureSelector = mkSelector "temperature"

-- | @Selector@ for @setTemperature:@
setTemperatureSelector :: Selector
setTemperatureSelector = mkSelector "setTemperature:"

-- | @Selector@ for @intensity@
intensitySelector :: Selector
intensitySelector = mkSelector "intensity"

-- | @Selector@ for @setIntensity:@
setIntensitySelector :: Selector
setIntensitySelector = mkSelector "setIntensity:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @castsShadow@
castsShadowSelector :: Selector
castsShadowSelector = mkSelector "castsShadow"

-- | @Selector@ for @setCastsShadow:@
setCastsShadowSelector :: Selector
setCastsShadowSelector = mkSelector "setCastsShadow:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector
setShadowColorSelector = mkSelector "setShadowColor:"

-- | @Selector@ for @shadowRadius@
shadowRadiusSelector :: Selector
shadowRadiusSelector = mkSelector "shadowRadius"

-- | @Selector@ for @setShadowRadius:@
setShadowRadiusSelector :: Selector
setShadowRadiusSelector = mkSelector "setShadowRadius:"

-- | @Selector@ for @shadowSampleCount@
shadowSampleCountSelector :: Selector
shadowSampleCountSelector = mkSelector "shadowSampleCount"

-- | @Selector@ for @setShadowSampleCount:@
setShadowSampleCountSelector :: Selector
setShadowSampleCountSelector = mkSelector "setShadowSampleCount:"

-- | @Selector@ for @shadowMode@
shadowModeSelector :: Selector
shadowModeSelector = mkSelector "shadowMode"

-- | @Selector@ for @setShadowMode:@
setShadowModeSelector :: Selector
setShadowModeSelector = mkSelector "setShadowMode:"

-- | @Selector@ for @shadowBias@
shadowBiasSelector :: Selector
shadowBiasSelector = mkSelector "shadowBias"

-- | @Selector@ for @setShadowBias:@
setShadowBiasSelector :: Selector
setShadowBiasSelector = mkSelector "setShadowBias:"

-- | @Selector@ for @automaticallyAdjustsShadowProjection@
automaticallyAdjustsShadowProjectionSelector :: Selector
automaticallyAdjustsShadowProjectionSelector = mkSelector "automaticallyAdjustsShadowProjection"

-- | @Selector@ for @setAutomaticallyAdjustsShadowProjection:@
setAutomaticallyAdjustsShadowProjectionSelector :: Selector
setAutomaticallyAdjustsShadowProjectionSelector = mkSelector "setAutomaticallyAdjustsShadowProjection:"

-- | @Selector@ for @maximumShadowDistance@
maximumShadowDistanceSelector :: Selector
maximumShadowDistanceSelector = mkSelector "maximumShadowDistance"

-- | @Selector@ for @setMaximumShadowDistance:@
setMaximumShadowDistanceSelector :: Selector
setMaximumShadowDistanceSelector = mkSelector "setMaximumShadowDistance:"

-- | @Selector@ for @forcesBackFaceCasters@
forcesBackFaceCastersSelector :: Selector
forcesBackFaceCastersSelector = mkSelector "forcesBackFaceCasters"

-- | @Selector@ for @setForcesBackFaceCasters:@
setForcesBackFaceCastersSelector :: Selector
setForcesBackFaceCastersSelector = mkSelector "setForcesBackFaceCasters:"

-- | @Selector@ for @sampleDistributedShadowMaps@
sampleDistributedShadowMapsSelector :: Selector
sampleDistributedShadowMapsSelector = mkSelector "sampleDistributedShadowMaps"

-- | @Selector@ for @setSampleDistributedShadowMaps:@
setSampleDistributedShadowMapsSelector :: Selector
setSampleDistributedShadowMapsSelector = mkSelector "setSampleDistributedShadowMaps:"

-- | @Selector@ for @shadowCascadeCount@
shadowCascadeCountSelector :: Selector
shadowCascadeCountSelector = mkSelector "shadowCascadeCount"

-- | @Selector@ for @setShadowCascadeCount:@
setShadowCascadeCountSelector :: Selector
setShadowCascadeCountSelector = mkSelector "setShadowCascadeCount:"

-- | @Selector@ for @shadowCascadeSplittingFactor@
shadowCascadeSplittingFactorSelector :: Selector
shadowCascadeSplittingFactorSelector = mkSelector "shadowCascadeSplittingFactor"

-- | @Selector@ for @setShadowCascadeSplittingFactor:@
setShadowCascadeSplittingFactorSelector :: Selector
setShadowCascadeSplittingFactorSelector = mkSelector "setShadowCascadeSplittingFactor:"

-- | @Selector@ for @orthographicScale@
orthographicScaleSelector :: Selector
orthographicScaleSelector = mkSelector "orthographicScale"

-- | @Selector@ for @setOrthographicScale:@
setOrthographicScaleSelector :: Selector
setOrthographicScaleSelector = mkSelector "setOrthographicScale:"

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

-- | @Selector@ for @attenuationStartDistance@
attenuationStartDistanceSelector :: Selector
attenuationStartDistanceSelector = mkSelector "attenuationStartDistance"

-- | @Selector@ for @setAttenuationStartDistance:@
setAttenuationStartDistanceSelector :: Selector
setAttenuationStartDistanceSelector = mkSelector "setAttenuationStartDistance:"

-- | @Selector@ for @attenuationEndDistance@
attenuationEndDistanceSelector :: Selector
attenuationEndDistanceSelector = mkSelector "attenuationEndDistance"

-- | @Selector@ for @setAttenuationEndDistance:@
setAttenuationEndDistanceSelector :: Selector
setAttenuationEndDistanceSelector = mkSelector "setAttenuationEndDistance:"

-- | @Selector@ for @attenuationFalloffExponent@
attenuationFalloffExponentSelector :: Selector
attenuationFalloffExponentSelector = mkSelector "attenuationFalloffExponent"

-- | @Selector@ for @setAttenuationFalloffExponent:@
setAttenuationFalloffExponentSelector :: Selector
setAttenuationFalloffExponentSelector = mkSelector "setAttenuationFalloffExponent:"

-- | @Selector@ for @spotInnerAngle@
spotInnerAngleSelector :: Selector
spotInnerAngleSelector = mkSelector "spotInnerAngle"

-- | @Selector@ for @setSpotInnerAngle:@
setSpotInnerAngleSelector :: Selector
setSpotInnerAngleSelector = mkSelector "setSpotInnerAngle:"

-- | @Selector@ for @spotOuterAngle@
spotOuterAngleSelector :: Selector
spotOuterAngleSelector = mkSelector "spotOuterAngle"

-- | @Selector@ for @setSpotOuterAngle:@
setSpotOuterAngleSelector :: Selector
setSpotOuterAngleSelector = mkSelector "setSpotOuterAngle:"

-- | @Selector@ for @IESProfileURL@
iesProfileURLSelector :: Selector
iesProfileURLSelector = mkSelector "IESProfileURL"

-- | @Selector@ for @setIESProfileURL:@
setIESProfileURLSelector :: Selector
setIESProfileURLSelector = mkSelector "setIESProfileURL:"

-- | @Selector@ for @sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficientsSelector :: Selector
sphericalHarmonicsCoefficientsSelector = mkSelector "sphericalHarmonicsCoefficients"

-- | @Selector@ for @probeType@
probeTypeSelector :: Selector
probeTypeSelector = mkSelector "probeType"

-- | @Selector@ for @setProbeType:@
setProbeTypeSelector :: Selector
setProbeTypeSelector = mkSelector "setProbeType:"

-- | @Selector@ for @probeUpdateType@
probeUpdateTypeSelector :: Selector
probeUpdateTypeSelector = mkSelector "probeUpdateType"

-- | @Selector@ for @setProbeUpdateType:@
setProbeUpdateTypeSelector :: Selector
setProbeUpdateTypeSelector = mkSelector "setProbeUpdateType:"

-- | @Selector@ for @parallaxCorrectionEnabled@
parallaxCorrectionEnabledSelector :: Selector
parallaxCorrectionEnabledSelector = mkSelector "parallaxCorrectionEnabled"

-- | @Selector@ for @setParallaxCorrectionEnabled:@
setParallaxCorrectionEnabledSelector :: Selector
setParallaxCorrectionEnabledSelector = mkSelector "setParallaxCorrectionEnabled:"

-- | @Selector@ for @probeEnvironment@
probeEnvironmentSelector :: Selector
probeEnvironmentSelector = mkSelector "probeEnvironment"

-- | @Selector@ for @areaType@
areaTypeSelector :: Selector
areaTypeSelector = mkSelector "areaType"

-- | @Selector@ for @setAreaType:@
setAreaTypeSelector :: Selector
setAreaTypeSelector = mkSelector "setAreaType:"

-- | @Selector@ for @areaPolygonVertices@
areaPolygonVerticesSelector :: Selector
areaPolygonVerticesSelector = mkSelector "areaPolygonVertices"

-- | @Selector@ for @setAreaPolygonVertices:@
setAreaPolygonVerticesSelector :: Selector
setAreaPolygonVerticesSelector = mkSelector "setAreaPolygonVertices:"

-- | @Selector@ for @drawsArea@
drawsAreaSelector :: Selector
drawsAreaSelector = mkSelector "drawsArea"

-- | @Selector@ for @setDrawsArea:@
setDrawsAreaSelector :: Selector
setDrawsAreaSelector = mkSelector "setDrawsArea:"

-- | @Selector@ for @doubleSided@
doubleSidedSelector :: Selector
doubleSidedSelector = mkSelector "doubleSided"

-- | @Selector@ for @setDoubleSided:@
setDoubleSidedSelector :: Selector
setDoubleSidedSelector = mkSelector "setDoubleSided:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

