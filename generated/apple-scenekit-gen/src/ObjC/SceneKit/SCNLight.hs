{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , gobo
  , categoryBitMask
  , setCategoryBitMask
  , areaPolygonVerticesSelector
  , areaTypeSelector
  , attenuationEndDistanceSelector
  , attenuationFalloffExponentSelector
  , attenuationStartDistanceSelector
  , attributeForKeySelector
  , automaticallyAdjustsShadowProjectionSelector
  , castsShadowSelector
  , categoryBitMaskSelector
  , colorSelector
  , doubleSidedSelector
  , drawsAreaSelector
  , forcesBackFaceCastersSelector
  , goboSelector
  , iesProfileURLSelector
  , intensitySelector
  , lightSelector
  , maximumShadowDistanceSelector
  , nameSelector
  , orthographicScaleSelector
  , parallaxCorrectionEnabledSelector
  , probeEnvironmentSelector
  , probeTypeSelector
  , probeUpdateTypeSelector
  , sampleDistributedShadowMapsSelector
  , setAreaPolygonVerticesSelector
  , setAreaTypeSelector
  , setAttenuationEndDistanceSelector
  , setAttenuationFalloffExponentSelector
  , setAttenuationStartDistanceSelector
  , setAttribute_forKeySelector
  , setAutomaticallyAdjustsShadowProjectionSelector
  , setCastsShadowSelector
  , setCategoryBitMaskSelector
  , setColorSelector
  , setDoubleSidedSelector
  , setDrawsAreaSelector
  , setForcesBackFaceCastersSelector
  , setIESProfileURLSelector
  , setIntensitySelector
  , setMaximumShadowDistanceSelector
  , setNameSelector
  , setOrthographicScaleSelector
  , setParallaxCorrectionEnabledSelector
  , setProbeTypeSelector
  , setProbeUpdateTypeSelector
  , setSampleDistributedShadowMapsSelector
  , setShadowBiasSelector
  , setShadowCascadeCountSelector
  , setShadowCascadeSplittingFactorSelector
  , setShadowColorSelector
  , setShadowModeSelector
  , setShadowRadiusSelector
  , setShadowSampleCountSelector
  , setSpotInnerAngleSelector
  , setSpotOuterAngleSelector
  , setTemperatureSelector
  , setTypeSelector
  , setZFarSelector
  , setZNearSelector
  , shadowBiasSelector
  , shadowCascadeCountSelector
  , shadowCascadeSplittingFactorSelector
  , shadowColorSelector
  , shadowModeSelector
  , shadowRadiusSelector
  , shadowSampleCountSelector
  , sphericalHarmonicsCoefficientsSelector
  , spotInnerAngleSelector
  , spotOuterAngleSelector
  , temperatureSelector
  , typeSelector
  , zFarSelector
  , zNearSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' lightSelector

-- | attributeForKey:
--
-- @key@ — The key for which to return the corresponding attribute.
--
-- Returns the attribute for the specified key. The valid keys are described in the "Light Attributes" constants.
--
-- ObjC selector: @- attributeForKey:@
attributeForKey :: (IsSCNLight scnLight, IsNSString key) => scnLight -> key -> IO RawId
attributeForKey scnLight key =
  sendMessage scnLight attributeForKeySelector (toNSString key)

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
setAttribute_forKey scnLight attribute key =
  sendMessage scnLight setAttribute_forKeySelector attribute (toNSString key)

-- | type
--
-- Specifies the receiver's type.
--
-- Defaults to SCNLightTypeOmni on iOS 8 and later, and on macOS 10.10 and later (otherwise defaults to SCNLightTypeAmbient).
--
-- ObjC selector: @- type@
type_ :: IsSCNLight scnLight => scnLight -> IO (Id NSString)
type_ scnLight =
  sendMessage scnLight typeSelector

-- | type
--
-- Specifies the receiver's type.
--
-- Defaults to SCNLightTypeOmni on iOS 8 and later, and on macOS 10.10 and later (otherwise defaults to SCNLightTypeAmbient).
--
-- ObjC selector: @- setType:@
setType :: (IsSCNLight scnLight, IsNSString value) => scnLight -> value -> IO ()
setType scnLight value =
  sendMessage scnLight setTypeSelector (toNSString value)

-- | color
--
-- Specifies the receiver's color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor. The renderer multiplies the light's color is by the color derived from the light's temperature.
--
-- ObjC selector: @- color@
color :: IsSCNLight scnLight => scnLight -> IO RawId
color scnLight =
  sendMessage scnLight colorSelector

-- | color
--
-- Specifies the receiver's color (NSColor or CGColorRef). Animatable. Defaults to white.
--
-- The initial value is a NSColor. The renderer multiplies the light's color is by the color derived from the light's temperature.
--
-- ObjC selector: @- setColor:@
setColor :: IsSCNLight scnLight => scnLight -> RawId -> IO ()
setColor scnLight value =
  sendMessage scnLight setColorSelector value

-- | temperature
--
-- Specifies the receiver's temperature.
--
-- This specifies the temperature of the light in Kelvin. The renderer multiplies the light's color by the color derived from the light's temperature. Defaults to 6500 (pure white). Animatable.
--
-- ObjC selector: @- temperature@
temperature :: IsSCNLight scnLight => scnLight -> IO CDouble
temperature scnLight =
  sendMessage scnLight temperatureSelector

-- | temperature
--
-- Specifies the receiver's temperature.
--
-- This specifies the temperature of the light in Kelvin. The renderer multiplies the light's color by the color derived from the light's temperature. Defaults to 6500 (pure white). Animatable.
--
-- ObjC selector: @- setTemperature:@
setTemperature :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setTemperature scnLight value =
  sendMessage scnLight setTemperatureSelector value

-- | intensity
--
-- Specifies the receiver's intensity.
--
-- This intensity is used to modulate the light color. When used with a physically-based material, this corresponds to the luminous flux of the light, expressed in lumens (lm). Defaults to 1000. Animatable.
--
-- ObjC selector: @- intensity@
intensity :: IsSCNLight scnLight => scnLight -> IO CDouble
intensity scnLight =
  sendMessage scnLight intensitySelector

-- | intensity
--
-- Specifies the receiver's intensity.
--
-- This intensity is used to modulate the light color. When used with a physically-based material, this corresponds to the luminous flux of the light, expressed in lumens (lm). Defaults to 1000. Animatable.
--
-- ObjC selector: @- setIntensity:@
setIntensity :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setIntensity scnLight value =
  sendMessage scnLight setIntensitySelector value

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNLight scnLight => scnLight -> IO (Id NSString)
name scnLight =
  sendMessage scnLight nameSelector

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNLight scnLight, IsNSString value) => scnLight -> value -> IO ()
setName scnLight value =
  sendMessage scnLight setNameSelector (toNSString value)

-- | castsShadow
--
-- Determines whether the receiver casts a shadow. Defaults to NO.
--
-- Shadows are only supported by spot and directional lights.
--
-- ObjC selector: @- castsShadow@
castsShadow :: IsSCNLight scnLight => scnLight -> IO Bool
castsShadow scnLight =
  sendMessage scnLight castsShadowSelector

-- | castsShadow
--
-- Determines whether the receiver casts a shadow. Defaults to NO.
--
-- Shadows are only supported by spot and directional lights.
--
-- ObjC selector: @- setCastsShadow:@
setCastsShadow :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setCastsShadow scnLight value =
  sendMessage scnLight setCastsShadowSelector value

-- | shadowColor
--
-- Specifies the color (CGColorRef or NSColor) of the shadow casted by the receiver. Defaults to black. Animatable.
--
-- On iOS 9 or earlier and macOS 10.11 or earlier, this defaults to black 50% transparent.
--
-- ObjC selector: @- shadowColor@
shadowColor :: IsSCNLight scnLight => scnLight -> IO RawId
shadowColor scnLight =
  sendMessage scnLight shadowColorSelector

-- | shadowColor
--
-- Specifies the color (CGColorRef or NSColor) of the shadow casted by the receiver. Defaults to black. Animatable.
--
-- On iOS 9 or earlier and macOS 10.11 or earlier, this defaults to black 50% transparent.
--
-- ObjC selector: @- setShadowColor:@
setShadowColor :: IsSCNLight scnLight => scnLight -> RawId -> IO ()
setShadowColor scnLight value =
  sendMessage scnLight setShadowColorSelector value

-- | shadowRadius
--
-- Specifies the sample radius used to render the receiver’s shadow. Default value is 3.0. Animatable.
--
-- ObjC selector: @- shadowRadius@
shadowRadius :: IsSCNLight scnLight => scnLight -> IO CDouble
shadowRadius scnLight =
  sendMessage scnLight shadowRadiusSelector

-- | shadowRadius
--
-- Specifies the sample radius used to render the receiver’s shadow. Default value is 3.0. Animatable.
--
-- ObjC selector: @- setShadowRadius:@
setShadowRadius :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setShadowRadius scnLight value =
  sendMessage scnLight setShadowRadiusSelector value

-- | shadowSampleCount
--
-- Specifies the number of sample per fragment to compute the shadow map. Defaults to 0.
--
-- On macOS 10.11 or earlier, the shadowSampleCount defaults to 16. On iOS 9 or earlier it defaults to 1.0. On macOS 10.12, iOS 10 and greater, when the shadowSampleCount is set to 0, a default sample count is chosen depending on the platform.
--
-- ObjC selector: @- shadowSampleCount@
shadowSampleCount :: IsSCNLight scnLight => scnLight -> IO CULong
shadowSampleCount scnLight =
  sendMessage scnLight shadowSampleCountSelector

-- | shadowSampleCount
--
-- Specifies the number of sample per fragment to compute the shadow map. Defaults to 0.
--
-- On macOS 10.11 or earlier, the shadowSampleCount defaults to 16. On iOS 9 or earlier it defaults to 1.0. On macOS 10.12, iOS 10 and greater, when the shadowSampleCount is set to 0, a default sample count is chosen depending on the platform.
--
-- ObjC selector: @- setShadowSampleCount:@
setShadowSampleCount :: IsSCNLight scnLight => scnLight -> CULong -> IO ()
setShadowSampleCount scnLight value =
  sendMessage scnLight setShadowSampleCountSelector value

-- | shadowMode
--
-- Specified the mode to use to cast shadows. See above for the available modes and their description. Defaults to SCNShadowModeDefered on 10.9 and before, defaults to SCNShadowModeForward otherwise.
--
-- ObjC selector: @- shadowMode@
shadowMode :: IsSCNLight scnLight => scnLight -> IO SCNShadowMode
shadowMode scnLight =
  sendMessage scnLight shadowModeSelector

-- | shadowMode
--
-- Specified the mode to use to cast shadows. See above for the available modes and their description. Defaults to SCNShadowModeDefered on 10.9 and before, defaults to SCNShadowModeForward otherwise.
--
-- ObjC selector: @- setShadowMode:@
setShadowMode :: IsSCNLight scnLight => scnLight -> SCNShadowMode -> IO ()
setShadowMode scnLight value =
  sendMessage scnLight setShadowModeSelector value

-- | shadowBias
--
-- Specifies the correction to apply to the shadow map to correct acne artefacts. It is multiplied by an implementation-specific value to create a constant depth offset. Defaults to 1.0
--
-- ObjC selector: @- shadowBias@
shadowBias :: IsSCNLight scnLight => scnLight -> IO CDouble
shadowBias scnLight =
  sendMessage scnLight shadowBiasSelector

-- | shadowBias
--
-- Specifies the correction to apply to the shadow map to correct acne artefacts. It is multiplied by an implementation-specific value to create a constant depth offset. Defaults to 1.0
--
-- ObjC selector: @- setShadowBias:@
setShadowBias :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setShadowBias scnLight value =
  sendMessage scnLight setShadowBiasSelector value

-- | automaticallyAdjustsShadowProjection
--
-- Specifies if the shadow map projection should be done automatically or manually by the user. Defaults to YES.
--
-- ObjC selector: @- automaticallyAdjustsShadowProjection@
automaticallyAdjustsShadowProjection :: IsSCNLight scnLight => scnLight -> IO Bool
automaticallyAdjustsShadowProjection scnLight =
  sendMessage scnLight automaticallyAdjustsShadowProjectionSelector

-- | automaticallyAdjustsShadowProjection
--
-- Specifies if the shadow map projection should be done automatically or manually by the user. Defaults to YES.
--
-- ObjC selector: @- setAutomaticallyAdjustsShadowProjection:@
setAutomaticallyAdjustsShadowProjection :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setAutomaticallyAdjustsShadowProjection scnLight value =
  sendMessage scnLight setAutomaticallyAdjustsShadowProjectionSelector value

-- | maximumShadowDistance
--
-- Specifies the maximum distance from the viewpoint from which the shadows for the receiver light won't be computed. Defaults to 100.0.
--
-- ObjC selector: @- maximumShadowDistance@
maximumShadowDistance :: IsSCNLight scnLight => scnLight -> IO CDouble
maximumShadowDistance scnLight =
  sendMessage scnLight maximumShadowDistanceSelector

-- | maximumShadowDistance
--
-- Specifies the maximum distance from the viewpoint from which the shadows for the receiver light won't be computed. Defaults to 100.0.
--
-- ObjC selector: @- setMaximumShadowDistance:@
setMaximumShadowDistance :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setMaximumShadowDistance scnLight value =
  sendMessage scnLight setMaximumShadowDistanceSelector value

-- | forcesBackFaceCasters
--
-- Render only back faces of the shadow caster when enabled. Defaults to NO. This is a behavior change from previous releases.
--
-- ObjC selector: @- forcesBackFaceCasters@
forcesBackFaceCasters :: IsSCNLight scnLight => scnLight -> IO Bool
forcesBackFaceCasters scnLight =
  sendMessage scnLight forcesBackFaceCastersSelector

-- | forcesBackFaceCasters
--
-- Render only back faces of the shadow caster when enabled. Defaults to NO. This is a behavior change from previous releases.
--
-- ObjC selector: @- setForcesBackFaceCasters:@
setForcesBackFaceCasters :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setForcesBackFaceCasters scnLight value =
  sendMessage scnLight setForcesBackFaceCastersSelector value

-- | sampleDistributedShadowMaps
--
-- Use the sample distribution of the main rendering to better fit the shadow frusta. Defaults to NO.
--
-- ObjC selector: @- sampleDistributedShadowMaps@
sampleDistributedShadowMaps :: IsSCNLight scnLight => scnLight -> IO Bool
sampleDistributedShadowMaps scnLight =
  sendMessage scnLight sampleDistributedShadowMapsSelector

-- | sampleDistributedShadowMaps
--
-- Use the sample distribution of the main rendering to better fit the shadow frusta. Defaults to NO.
--
-- ObjC selector: @- setSampleDistributedShadowMaps:@
setSampleDistributedShadowMaps :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setSampleDistributedShadowMaps scnLight value =
  sendMessage scnLight setSampleDistributedShadowMapsSelector value

-- | shadowCascadeCount
--
-- Specifies the number of distinct shadow maps that will be computed for the receiver light. Defaults to 1. Maximum is 4.
--
-- ObjC selector: @- shadowCascadeCount@
shadowCascadeCount :: IsSCNLight scnLight => scnLight -> IO CULong
shadowCascadeCount scnLight =
  sendMessage scnLight shadowCascadeCountSelector

-- | shadowCascadeCount
--
-- Specifies the number of distinct shadow maps that will be computed for the receiver light. Defaults to 1. Maximum is 4.
--
-- ObjC selector: @- setShadowCascadeCount:@
setShadowCascadeCount :: IsSCNLight scnLight => scnLight -> CULong -> IO ()
setShadowCascadeCount scnLight value =
  sendMessage scnLight setShadowCascadeCountSelector value

-- | shadowCascadeSplittingFactor
--
-- Specifies a factor to interpolate between linear splitting (0) and logarithmic splitting (1). Defaults to 0.15.
--
-- ObjC selector: @- shadowCascadeSplittingFactor@
shadowCascadeSplittingFactor :: IsSCNLight scnLight => scnLight -> IO CDouble
shadowCascadeSplittingFactor scnLight =
  sendMessage scnLight shadowCascadeSplittingFactorSelector

-- | shadowCascadeSplittingFactor
--
-- Specifies a factor to interpolate between linear splitting (0) and logarithmic splitting (1). Defaults to 0.15.
--
-- ObjC selector: @- setShadowCascadeSplittingFactor:@
setShadowCascadeSplittingFactor :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setShadowCascadeSplittingFactor scnLight value =
  sendMessage scnLight setShadowCascadeSplittingFactorSelector value

-- | orthographicScale
--
-- Specifies the orthographic scale used to render from the directional light into the shadow map. Defaults to 1.
--
-- This is only applicable for directional lights.
--
-- ObjC selector: @- orthographicScale@
orthographicScale :: IsSCNLight scnLight => scnLight -> IO CDouble
orthographicScale scnLight =
  sendMessage scnLight orthographicScaleSelector

-- | orthographicScale
--
-- Specifies the orthographic scale used to render from the directional light into the shadow map. Defaults to 1.
--
-- This is only applicable for directional lights.
--
-- ObjC selector: @- setOrthographicScale:@
setOrthographicScale :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setOrthographicScale scnLight value =
  sendMessage scnLight setOrthographicScaleSelector value

-- | zNear
--
-- Specifies the minimal distance between the light and the surface to cast shadow on. If a surface is closer to the light than this minimal distance, then the surface won't be shadowed. The near value must be different than zero. Animatable. Defaults to 1.
--
-- ObjC selector: @- zNear@
zNear :: IsSCNLight scnLight => scnLight -> IO CDouble
zNear scnLight =
  sendMessage scnLight zNearSelector

-- | zNear
--
-- Specifies the minimal distance between the light and the surface to cast shadow on. If a surface is closer to the light than this minimal distance, then the surface won't be shadowed. The near value must be different than zero. Animatable. Defaults to 1.
--
-- ObjC selector: @- setZNear:@
setZNear :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setZNear scnLight value =
  sendMessage scnLight setZNearSelector value

-- | zFar
--
-- Specifies the maximal distance between the light and a visible surface to cast shadow on. If a surface is further from the light than this maximal distance, then the surface won't be shadowed. Animatable. Defaults to 100.
--
-- ObjC selector: @- zFar@
zFar :: IsSCNLight scnLight => scnLight -> IO CDouble
zFar scnLight =
  sendMessage scnLight zFarSelector

-- | zFar
--
-- Specifies the maximal distance between the light and a visible surface to cast shadow on. If a surface is further from the light than this maximal distance, then the surface won't be shadowed. Animatable. Defaults to 100.
--
-- ObjC selector: @- setZFar:@
setZFar :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setZFar scnLight value =
  sendMessage scnLight setZFarSelector value

-- | attenuationStartDistance
--
-- The distance at which the attenuation starts (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- attenuationStartDistance@
attenuationStartDistance :: IsSCNLight scnLight => scnLight -> IO CDouble
attenuationStartDistance scnLight =
  sendMessage scnLight attenuationStartDistanceSelector

-- | attenuationStartDistance
--
-- The distance at which the attenuation starts (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- setAttenuationStartDistance:@
setAttenuationStartDistance :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setAttenuationStartDistance scnLight value =
  sendMessage scnLight setAttenuationStartDistanceSelector value

-- | attenuationEndDistance
--
-- The distance at which the attenuation ends (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- attenuationEndDistance@
attenuationEndDistance :: IsSCNLight scnLight => scnLight -> IO CDouble
attenuationEndDistance scnLight =
  sendMessage scnLight attenuationEndDistanceSelector

-- | attenuationEndDistance
--
-- The distance at which the attenuation ends (Omni or Spot light types only). Animatable. Defaults to 0.
--
-- ObjC selector: @- setAttenuationEndDistance:@
setAttenuationEndDistance :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setAttenuationEndDistance scnLight value =
  sendMessage scnLight setAttenuationEndDistanceSelector value

-- | attenuationFalloffExponent
--
-- Specifies the attenuation between the start and end attenuation distances. 0 means a constant attenuation, 1 a linear attenuation and 2 a quadratic attenuation, but any positive value will work (Omni or Spot light types only). Animatable. Defaults to 2.
--
-- ObjC selector: @- attenuationFalloffExponent@
attenuationFalloffExponent :: IsSCNLight scnLight => scnLight -> IO CDouble
attenuationFalloffExponent scnLight =
  sendMessage scnLight attenuationFalloffExponentSelector

-- | attenuationFalloffExponent
--
-- Specifies the attenuation between the start and end attenuation distances. 0 means a constant attenuation, 1 a linear attenuation and 2 a quadratic attenuation, but any positive value will work (Omni or Spot light types only). Animatable. Defaults to 2.
--
-- ObjC selector: @- setAttenuationFalloffExponent:@
setAttenuationFalloffExponent :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setAttenuationFalloffExponent scnLight value =
  sendMessage scnLight setAttenuationFalloffExponentSelector value

-- | spotInnerAngle
--
-- The angle in degrees between the spot direction and the lit element below which the lighting is at full strength. Animatable. Defaults to 0.
--
-- ObjC selector: @- spotInnerAngle@
spotInnerAngle :: IsSCNLight scnLight => scnLight -> IO CDouble
spotInnerAngle scnLight =
  sendMessage scnLight spotInnerAngleSelector

-- | spotInnerAngle
--
-- The angle in degrees between the spot direction and the lit element below which the lighting is at full strength. Animatable. Defaults to 0.
--
-- ObjC selector: @- setSpotInnerAngle:@
setSpotInnerAngle :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setSpotInnerAngle scnLight value =
  sendMessage scnLight setSpotInnerAngleSelector value

-- | spotOuterAngle
--
-- The angle in degrees between the spot direction and the lit element after which the lighting is at zero strength. Animatable. Defaults to 45 degrees.
--
-- ObjC selector: @- spotOuterAngle@
spotOuterAngle :: IsSCNLight scnLight => scnLight -> IO CDouble
spotOuterAngle scnLight =
  sendMessage scnLight spotOuterAngleSelector

-- | spotOuterAngle
--
-- The angle in degrees between the spot direction and the lit element after which the lighting is at zero strength. Animatable. Defaults to 45 degrees.
--
-- ObjC selector: @- setSpotOuterAngle:@
setSpotOuterAngle :: IsSCNLight scnLight => scnLight -> CDouble -> IO ()
setSpotOuterAngle scnLight value =
  sendMessage scnLight setSpotOuterAngleSelector value

-- | IESProfileURL
--
-- Specifies the IES file from which the shape, direction, and intensity of illumination is determined. Defaults to nil.
--
-- ObjC selector: @- IESProfileURL@
iesProfileURL :: IsSCNLight scnLight => scnLight -> IO (Id NSURL)
iesProfileURL scnLight =
  sendMessage scnLight iesProfileURLSelector

-- | IESProfileURL
--
-- Specifies the IES file from which the shape, direction, and intensity of illumination is determined. Defaults to nil.
--
-- ObjC selector: @- setIESProfileURL:@
setIESProfileURL :: (IsSCNLight scnLight, IsNSURL value) => scnLight -> value -> IO ()
setIESProfileURL scnLight value =
  sendMessage scnLight setIESProfileURLSelector (toNSURL value)

-- | sphericalHarmonicsCoefficients
--
-- The receiver's spherical harmonics coefficients.
--
-- Currently spherical harmonics are only supported by light probes (SCNLightTypeProbe). The data is an array of 27 32-bit floating-point values, containing three non-interleaved data sets corresponding to the red, green, and blue sets of coefficients.
--
-- ObjC selector: @- sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficients :: IsSCNLight scnLight => scnLight -> IO (Id NSData)
sphericalHarmonicsCoefficients scnLight =
  sendMessage scnLight sphericalHarmonicsCoefficientsSelector

-- | @- probeType@
probeType :: IsSCNLight scnLight => scnLight -> IO SCNLightProbeType
probeType scnLight =
  sendMessage scnLight probeTypeSelector

-- | @- setProbeType:@
setProbeType :: IsSCNLight scnLight => scnLight -> SCNLightProbeType -> IO ()
setProbeType scnLight value =
  sendMessage scnLight setProbeTypeSelector value

-- | @- probeUpdateType@
probeUpdateType :: IsSCNLight scnLight => scnLight -> IO SCNLightProbeUpdateType
probeUpdateType scnLight =
  sendMessage scnLight probeUpdateTypeSelector

-- | @- setProbeUpdateType:@
setProbeUpdateType :: IsSCNLight scnLight => scnLight -> SCNLightProbeUpdateType -> IO ()
setProbeUpdateType scnLight value =
  sendMessage scnLight setProbeUpdateTypeSelector value

-- | @- parallaxCorrectionEnabled@
parallaxCorrectionEnabled :: IsSCNLight scnLight => scnLight -> IO Bool
parallaxCorrectionEnabled scnLight =
  sendMessage scnLight parallaxCorrectionEnabledSelector

-- | @- setParallaxCorrectionEnabled:@
setParallaxCorrectionEnabled :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setParallaxCorrectionEnabled scnLight value =
  sendMessage scnLight setParallaxCorrectionEnabledSelector value

-- | @- probeEnvironment@
probeEnvironment :: IsSCNLight scnLight => scnLight -> IO (Id SCNMaterialProperty)
probeEnvironment scnLight =
  sendMessage scnLight probeEnvironmentSelector

-- | areaType
--
-- Determines the shape of a light of type SCNLightTypeArea. Defaults to SCNLightAreaTypeRectangle.
--
-- ObjC selector: @- areaType@
areaType :: IsSCNLight scnLight => scnLight -> IO SCNLightAreaType
areaType scnLight =
  sendMessage scnLight areaTypeSelector

-- | areaType
--
-- Determines the shape of a light of type SCNLightTypeArea. Defaults to SCNLightAreaTypeRectangle.
--
-- ObjC selector: @- setAreaType:@
setAreaType :: IsSCNLight scnLight => scnLight -> SCNLightAreaType -> IO ()
setAreaType scnLight value =
  sendMessage scnLight setAreaTypeSelector value

-- | areaPolygonVertices
--
-- Determines the shape of light of an area light of type SCNLightAreaTypePolygon. Defaults nil.
--
-- An array of CGPoint values corresponding to the coordinates of the polygon's vertices in the XY plane.
--
-- ObjC selector: @- areaPolygonVertices@
areaPolygonVertices :: IsSCNLight scnLight => scnLight -> IO (Id NSArray)
areaPolygonVertices scnLight =
  sendMessage scnLight areaPolygonVerticesSelector

-- | areaPolygonVertices
--
-- Determines the shape of light of an area light of type SCNLightAreaTypePolygon. Defaults nil.
--
-- An array of CGPoint values corresponding to the coordinates of the polygon's vertices in the XY plane.
--
-- ObjC selector: @- setAreaPolygonVertices:@
setAreaPolygonVertices :: (IsSCNLight scnLight, IsNSArray value) => scnLight -> value -> IO ()
setAreaPolygonVertices scnLight value =
  sendMessage scnLight setAreaPolygonVerticesSelector (toNSArray value)

-- | drawsArea
--
-- Determines whether the shape of a light of type SCNLightTypeArea is drawn in the scene. Defaults to YES.
--
-- ObjC selector: @- drawsArea@
drawsArea :: IsSCNLight scnLight => scnLight -> IO Bool
drawsArea scnLight =
  sendMessage scnLight drawsAreaSelector

-- | drawsArea
--
-- Determines whether the shape of a light of type SCNLightTypeArea is drawn in the scene. Defaults to YES.
--
-- ObjC selector: @- setDrawsArea:@
setDrawsArea :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setDrawsArea scnLight value =
  sendMessage scnLight setDrawsAreaSelector value

-- | doubleSided
--
-- Determines whether a light of type SCNLightTypeArea is double-sided. Defaults NO.
--
-- Area lights of type SCNLightAreaTypeRectangle or SCNLightAreaTypePolygon emit light along the -Z axis. When set to YES, they also emit light along the +Z axis.
--
-- ObjC selector: @- doubleSided@
doubleSided :: IsSCNLight scnLight => scnLight -> IO Bool
doubleSided scnLight =
  sendMessage scnLight doubleSidedSelector

-- | doubleSided
--
-- Determines whether a light of type SCNLightTypeArea is double-sided. Defaults NO.
--
-- Area lights of type SCNLightAreaTypeRectangle or SCNLightAreaTypePolygon emit light along the -Z axis. When set to YES, they also emit light along the +Z axis.
--
-- ObjC selector: @- setDoubleSided:@
setDoubleSided :: IsSCNLight scnLight => scnLight -> Bool -> IO ()
setDoubleSided scnLight value =
  sendMessage scnLight setDoubleSidedSelector value

-- | gobo
--
-- Specifies the gobo (or "cookie") of the light, used to control the shape of emitted light.
--
-- Gobos are only supported by spot lights.
--
-- ObjC selector: @- gobo@
gobo :: IsSCNLight scnLight => scnLight -> IO (Id SCNMaterialProperty)
gobo scnLight =
  sendMessage scnLight goboSelector

-- | categoryBitMask
--
-- Determines the node categories that will be lit by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSCNLight scnLight => scnLight -> IO CULong
categoryBitMask scnLight =
  sendMessage scnLight categoryBitMaskSelector

-- | categoryBitMask
--
-- Determines the node categories that will be lit by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNLight scnLight => scnLight -> CULong -> IO ()
setCategoryBitMask scnLight value =
  sendMessage scnLight setCategoryBitMaskSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @light@
lightSelector :: Selector '[] (Id SCNLight)
lightSelector = mkSelector "light"

-- | @Selector@ for @attributeForKey:@
attributeForKeySelector :: Selector '[Id NSString] RawId
attributeForKeySelector = mkSelector "attributeForKey:"

-- | @Selector@ for @setAttribute:forKey:@
setAttribute_forKeySelector :: Selector '[RawId, Id NSString] ()
setAttribute_forKeySelector = mkSelector "setAttribute:forKey:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] RawId
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[RawId] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @temperature@
temperatureSelector :: Selector '[] CDouble
temperatureSelector = mkSelector "temperature"

-- | @Selector@ for @setTemperature:@
setTemperatureSelector :: Selector '[CDouble] ()
setTemperatureSelector = mkSelector "setTemperature:"

-- | @Selector@ for @intensity@
intensitySelector :: Selector '[] CDouble
intensitySelector = mkSelector "intensity"

-- | @Selector@ for @setIntensity:@
setIntensitySelector :: Selector '[CDouble] ()
setIntensitySelector = mkSelector "setIntensity:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @castsShadow@
castsShadowSelector :: Selector '[] Bool
castsShadowSelector = mkSelector "castsShadow"

-- | @Selector@ for @setCastsShadow:@
setCastsShadowSelector :: Selector '[Bool] ()
setCastsShadowSelector = mkSelector "setCastsShadow:"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector '[] RawId
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @setShadowColor:@
setShadowColorSelector :: Selector '[RawId] ()
setShadowColorSelector = mkSelector "setShadowColor:"

-- | @Selector@ for @shadowRadius@
shadowRadiusSelector :: Selector '[] CDouble
shadowRadiusSelector = mkSelector "shadowRadius"

-- | @Selector@ for @setShadowRadius:@
setShadowRadiusSelector :: Selector '[CDouble] ()
setShadowRadiusSelector = mkSelector "setShadowRadius:"

-- | @Selector@ for @shadowSampleCount@
shadowSampleCountSelector :: Selector '[] CULong
shadowSampleCountSelector = mkSelector "shadowSampleCount"

-- | @Selector@ for @setShadowSampleCount:@
setShadowSampleCountSelector :: Selector '[CULong] ()
setShadowSampleCountSelector = mkSelector "setShadowSampleCount:"

-- | @Selector@ for @shadowMode@
shadowModeSelector :: Selector '[] SCNShadowMode
shadowModeSelector = mkSelector "shadowMode"

-- | @Selector@ for @setShadowMode:@
setShadowModeSelector :: Selector '[SCNShadowMode] ()
setShadowModeSelector = mkSelector "setShadowMode:"

-- | @Selector@ for @shadowBias@
shadowBiasSelector :: Selector '[] CDouble
shadowBiasSelector = mkSelector "shadowBias"

-- | @Selector@ for @setShadowBias:@
setShadowBiasSelector :: Selector '[CDouble] ()
setShadowBiasSelector = mkSelector "setShadowBias:"

-- | @Selector@ for @automaticallyAdjustsShadowProjection@
automaticallyAdjustsShadowProjectionSelector :: Selector '[] Bool
automaticallyAdjustsShadowProjectionSelector = mkSelector "automaticallyAdjustsShadowProjection"

-- | @Selector@ for @setAutomaticallyAdjustsShadowProjection:@
setAutomaticallyAdjustsShadowProjectionSelector :: Selector '[Bool] ()
setAutomaticallyAdjustsShadowProjectionSelector = mkSelector "setAutomaticallyAdjustsShadowProjection:"

-- | @Selector@ for @maximumShadowDistance@
maximumShadowDistanceSelector :: Selector '[] CDouble
maximumShadowDistanceSelector = mkSelector "maximumShadowDistance"

-- | @Selector@ for @setMaximumShadowDistance:@
setMaximumShadowDistanceSelector :: Selector '[CDouble] ()
setMaximumShadowDistanceSelector = mkSelector "setMaximumShadowDistance:"

-- | @Selector@ for @forcesBackFaceCasters@
forcesBackFaceCastersSelector :: Selector '[] Bool
forcesBackFaceCastersSelector = mkSelector "forcesBackFaceCasters"

-- | @Selector@ for @setForcesBackFaceCasters:@
setForcesBackFaceCastersSelector :: Selector '[Bool] ()
setForcesBackFaceCastersSelector = mkSelector "setForcesBackFaceCasters:"

-- | @Selector@ for @sampleDistributedShadowMaps@
sampleDistributedShadowMapsSelector :: Selector '[] Bool
sampleDistributedShadowMapsSelector = mkSelector "sampleDistributedShadowMaps"

-- | @Selector@ for @setSampleDistributedShadowMaps:@
setSampleDistributedShadowMapsSelector :: Selector '[Bool] ()
setSampleDistributedShadowMapsSelector = mkSelector "setSampleDistributedShadowMaps:"

-- | @Selector@ for @shadowCascadeCount@
shadowCascadeCountSelector :: Selector '[] CULong
shadowCascadeCountSelector = mkSelector "shadowCascadeCount"

-- | @Selector@ for @setShadowCascadeCount:@
setShadowCascadeCountSelector :: Selector '[CULong] ()
setShadowCascadeCountSelector = mkSelector "setShadowCascadeCount:"

-- | @Selector@ for @shadowCascadeSplittingFactor@
shadowCascadeSplittingFactorSelector :: Selector '[] CDouble
shadowCascadeSplittingFactorSelector = mkSelector "shadowCascadeSplittingFactor"

-- | @Selector@ for @setShadowCascadeSplittingFactor:@
setShadowCascadeSplittingFactorSelector :: Selector '[CDouble] ()
setShadowCascadeSplittingFactorSelector = mkSelector "setShadowCascadeSplittingFactor:"

-- | @Selector@ for @orthographicScale@
orthographicScaleSelector :: Selector '[] CDouble
orthographicScaleSelector = mkSelector "orthographicScale"

-- | @Selector@ for @setOrthographicScale:@
setOrthographicScaleSelector :: Selector '[CDouble] ()
setOrthographicScaleSelector = mkSelector "setOrthographicScale:"

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

-- | @Selector@ for @attenuationStartDistance@
attenuationStartDistanceSelector :: Selector '[] CDouble
attenuationStartDistanceSelector = mkSelector "attenuationStartDistance"

-- | @Selector@ for @setAttenuationStartDistance:@
setAttenuationStartDistanceSelector :: Selector '[CDouble] ()
setAttenuationStartDistanceSelector = mkSelector "setAttenuationStartDistance:"

-- | @Selector@ for @attenuationEndDistance@
attenuationEndDistanceSelector :: Selector '[] CDouble
attenuationEndDistanceSelector = mkSelector "attenuationEndDistance"

-- | @Selector@ for @setAttenuationEndDistance:@
setAttenuationEndDistanceSelector :: Selector '[CDouble] ()
setAttenuationEndDistanceSelector = mkSelector "setAttenuationEndDistance:"

-- | @Selector@ for @attenuationFalloffExponent@
attenuationFalloffExponentSelector :: Selector '[] CDouble
attenuationFalloffExponentSelector = mkSelector "attenuationFalloffExponent"

-- | @Selector@ for @setAttenuationFalloffExponent:@
setAttenuationFalloffExponentSelector :: Selector '[CDouble] ()
setAttenuationFalloffExponentSelector = mkSelector "setAttenuationFalloffExponent:"

-- | @Selector@ for @spotInnerAngle@
spotInnerAngleSelector :: Selector '[] CDouble
spotInnerAngleSelector = mkSelector "spotInnerAngle"

-- | @Selector@ for @setSpotInnerAngle:@
setSpotInnerAngleSelector :: Selector '[CDouble] ()
setSpotInnerAngleSelector = mkSelector "setSpotInnerAngle:"

-- | @Selector@ for @spotOuterAngle@
spotOuterAngleSelector :: Selector '[] CDouble
spotOuterAngleSelector = mkSelector "spotOuterAngle"

-- | @Selector@ for @setSpotOuterAngle:@
setSpotOuterAngleSelector :: Selector '[CDouble] ()
setSpotOuterAngleSelector = mkSelector "setSpotOuterAngle:"

-- | @Selector@ for @IESProfileURL@
iesProfileURLSelector :: Selector '[] (Id NSURL)
iesProfileURLSelector = mkSelector "IESProfileURL"

-- | @Selector@ for @setIESProfileURL:@
setIESProfileURLSelector :: Selector '[Id NSURL] ()
setIESProfileURLSelector = mkSelector "setIESProfileURL:"

-- | @Selector@ for @sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficientsSelector :: Selector '[] (Id NSData)
sphericalHarmonicsCoefficientsSelector = mkSelector "sphericalHarmonicsCoefficients"

-- | @Selector@ for @probeType@
probeTypeSelector :: Selector '[] SCNLightProbeType
probeTypeSelector = mkSelector "probeType"

-- | @Selector@ for @setProbeType:@
setProbeTypeSelector :: Selector '[SCNLightProbeType] ()
setProbeTypeSelector = mkSelector "setProbeType:"

-- | @Selector@ for @probeUpdateType@
probeUpdateTypeSelector :: Selector '[] SCNLightProbeUpdateType
probeUpdateTypeSelector = mkSelector "probeUpdateType"

-- | @Selector@ for @setProbeUpdateType:@
setProbeUpdateTypeSelector :: Selector '[SCNLightProbeUpdateType] ()
setProbeUpdateTypeSelector = mkSelector "setProbeUpdateType:"

-- | @Selector@ for @parallaxCorrectionEnabled@
parallaxCorrectionEnabledSelector :: Selector '[] Bool
parallaxCorrectionEnabledSelector = mkSelector "parallaxCorrectionEnabled"

-- | @Selector@ for @setParallaxCorrectionEnabled:@
setParallaxCorrectionEnabledSelector :: Selector '[Bool] ()
setParallaxCorrectionEnabledSelector = mkSelector "setParallaxCorrectionEnabled:"

-- | @Selector@ for @probeEnvironment@
probeEnvironmentSelector :: Selector '[] (Id SCNMaterialProperty)
probeEnvironmentSelector = mkSelector "probeEnvironment"

-- | @Selector@ for @areaType@
areaTypeSelector :: Selector '[] SCNLightAreaType
areaTypeSelector = mkSelector "areaType"

-- | @Selector@ for @setAreaType:@
setAreaTypeSelector :: Selector '[SCNLightAreaType] ()
setAreaTypeSelector = mkSelector "setAreaType:"

-- | @Selector@ for @areaPolygonVertices@
areaPolygonVerticesSelector :: Selector '[] (Id NSArray)
areaPolygonVerticesSelector = mkSelector "areaPolygonVertices"

-- | @Selector@ for @setAreaPolygonVertices:@
setAreaPolygonVerticesSelector :: Selector '[Id NSArray] ()
setAreaPolygonVerticesSelector = mkSelector "setAreaPolygonVertices:"

-- | @Selector@ for @drawsArea@
drawsAreaSelector :: Selector '[] Bool
drawsAreaSelector = mkSelector "drawsArea"

-- | @Selector@ for @setDrawsArea:@
setDrawsAreaSelector :: Selector '[Bool] ()
setDrawsAreaSelector = mkSelector "setDrawsArea:"

-- | @Selector@ for @doubleSided@
doubleSidedSelector :: Selector '[] Bool
doubleSidedSelector = mkSelector "doubleSided"

-- | @Selector@ for @setDoubleSided:@
setDoubleSidedSelector :: Selector '[Bool] ()
setDoubleSidedSelector = mkSelector "setDoubleSided:"

-- | @Selector@ for @gobo@
goboSelector :: Selector '[] (Id SCNMaterialProperty)
goboSelector = mkSelector "gobo"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector '[] CULong
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector '[CULong] ()
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

