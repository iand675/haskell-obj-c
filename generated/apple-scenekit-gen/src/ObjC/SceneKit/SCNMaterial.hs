{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNMaterial
--
-- A SCNMaterial determines how a geometry is rendered. It encapsulates the colors and textures that define the appearance of 3d geometries.
--
-- Generated bindings for @SCNMaterial@.
module ObjC.SceneKit.SCNMaterial
  ( SCNMaterial
  , IsSCNMaterial(..)
  , material
  , name
  , setName
  , diffuse
  , ambient
  , specular
  , emission
  , transparent
  , reflective
  , multiply
  , normal
  , displacement
  , ambientOcclusion
  , selfIllumination
  , metalness
  , roughness
  , clearCoat
  , clearCoatRoughness
  , clearCoatNormal
  , shininess
  , setShininess
  , transparency
  , setTransparency
  , lightingModelName
  , setLightingModelName
  , litPerPixel
  , setLitPerPixel
  , doubleSided
  , setDoubleSided
  , fillMode
  , setFillMode
  , cullMode
  , setCullMode
  , transparencyMode
  , setTransparencyMode
  , locksAmbientWithDiffuse
  , setLocksAmbientWithDiffuse
  , writesToDepthBuffer
  , setWritesToDepthBuffer
  , colorBufferWriteMask
  , setColorBufferWriteMask
  , readsFromDepthBuffer
  , setReadsFromDepthBuffer
  , fresnelExponent
  , setFresnelExponent
  , blendMode
  , setBlendMode
  , ambientOcclusionSelector
  , ambientSelector
  , blendModeSelector
  , clearCoatNormalSelector
  , clearCoatRoughnessSelector
  , clearCoatSelector
  , colorBufferWriteMaskSelector
  , cullModeSelector
  , diffuseSelector
  , displacementSelector
  , doubleSidedSelector
  , emissionSelector
  , fillModeSelector
  , fresnelExponentSelector
  , lightingModelNameSelector
  , litPerPixelSelector
  , locksAmbientWithDiffuseSelector
  , materialSelector
  , metalnessSelector
  , multiplySelector
  , nameSelector
  , normalSelector
  , readsFromDepthBufferSelector
  , reflectiveSelector
  , roughnessSelector
  , selfIlluminationSelector
  , setBlendModeSelector
  , setColorBufferWriteMaskSelector
  , setCullModeSelector
  , setDoubleSidedSelector
  , setFillModeSelector
  , setFresnelExponentSelector
  , setLightingModelNameSelector
  , setLitPerPixelSelector
  , setLocksAmbientWithDiffuseSelector
  , setNameSelector
  , setReadsFromDepthBufferSelector
  , setShininessSelector
  , setTransparencyModeSelector
  , setTransparencySelector
  , setWritesToDepthBufferSelector
  , shininessSelector
  , specularSelector
  , transparencyModeSelector
  , transparencySelector
  , transparentSelector
  , writesToDepthBufferSelector

  -- * Enum types
  , SCNBlendMode(SCNBlendMode)
  , pattern SCNBlendModeAlpha
  , pattern SCNBlendModeAdd
  , pattern SCNBlendModeSubtract
  , pattern SCNBlendModeMultiply
  , pattern SCNBlendModeScreen
  , pattern SCNBlendModeReplace
  , pattern SCNBlendModeMax
  , SCNColorMask(SCNColorMask)
  , pattern SCNColorMaskNone
  , pattern SCNColorMaskRed
  , pattern SCNColorMaskGreen
  , pattern SCNColorMaskBlue
  , pattern SCNColorMaskAlpha
  , pattern SCNColorMaskAll
  , SCNCullMode(SCNCullMode)
  , pattern SCNCullModeBack
  , pattern SCNCullModeFront
  , SCNFillMode(SCNFillMode)
  , pattern SCNFillModeFill
  , pattern SCNFillModeLines
  , SCNTransparencyMode(SCNTransparencyMode)
  , pattern SCNTransparencyModeAOne
  , pattern SCNTransparencyModeRGBZero
  , pattern SCNTransparencyModeSingleLayer
  , pattern SCNTransparencyModeDualLayer
  , pattern SCNTransparencyModeDefault

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

-- | material
--
-- Creates and initialize a material instance.
--
-- ObjC selector: @+ material@
material :: IO (Id SCNMaterial)
material  =
  do
    cls' <- getRequiredClass "SCNMaterial"
    sendClassMessage cls' materialSelector

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id NSString)
name scnMaterial =
  sendMessage scnMaterial nameSelector

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNMaterial scnMaterial, IsNSString value) => scnMaterial -> value -> IO ()
setName scnMaterial value =
  sendMessage scnMaterial setNameSelector (toNSString value)

-- | diffuse
--
-- Specifies the receiver's diffuse property.
--
-- The diffuse property specifies the amount of light diffusely reflected from the surface. The diffuse light is reflected equally in all directions and is therefore independent of the point of view.
--
-- ObjC selector: @- diffuse@
diffuse :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
diffuse scnMaterial =
  sendMessage scnMaterial diffuseSelector

-- | ambient
--
-- Specifies the receiver's ambient property.
--
-- The ambient property specifies the amount of ambient light to reflect. This property has no visual impact on scenes that have no ambient light. Setting the ambient has no effect if locksAmbientWithDiffuse is set to YES.
--
-- ObjC selector: @- ambient@
ambient :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
ambient scnMaterial =
  sendMessage scnMaterial ambientSelector

-- | specular
--
-- Specifies the receiver's specular property.
--
-- The specular property specifies the amount of light to reflect in a mirror-like manner. The specular intensity increases when the point of view lines up with the direction of the reflected light.
--
-- ObjC selector: @- specular@
specular :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
specular scnMaterial =
  sendMessage scnMaterial specularSelector

-- | emission
--
-- The emission property specifies the amount of light the material emits. This emission does not light up other surfaces in the scene.
--
-- ObjC selector: @- emission@
emission :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
emission scnMaterial =
  sendMessage scnMaterial emissionSelector

-- | transparent
--
-- The transparent property specifies the transparent areas of the material.
--
-- ObjC selector: @- transparent@
transparent :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
transparent scnMaterial =
  sendMessage scnMaterial transparentSelector

-- | reflective
--
-- The reflective property specifies the reflectivity of the surface. The surface will not actually reflect other objects in the scene. This property may be used as a sphere mapping to reflect a precomputed environment.
--
-- ObjC selector: @- reflective@
reflective :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
reflective scnMaterial =
  sendMessage scnMaterial reflectiveSelector

-- | multiply
--
-- The multiply property specifies a color or an image used to multiply the output fragments with. The computed fragments are multiplied with the multiply value to produce the final fragments. This property may be used for shadow maps, to fade out or tint 3d objects.
--
-- ObjC selector: @- multiply@
multiply :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
multiply scnMaterial =
  sendMessage scnMaterial multiplySelector

-- | normal
--
-- The normal property specifies the surface orientation.
--
-- When an image is set on the normal property the material is automatically lit per pixel. Setting a color has no effect.
--
-- ObjC selector: @- normal@
normal :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
normal scnMaterial =
  sendMessage scnMaterial normalSelector

-- | displacement
--
-- The displacement property specifies how vertex are translated in tangent space.
--
-- Pass a grayscale image for a simple 'elevation' or rgb image for a vector displacement.
--
-- ObjC selector: @- displacement@
displacement :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
displacement scnMaterial =
  sendMessage scnMaterial displacementSelector

-- | ambientOcclusion
--
-- The ambientOcclusion property specifies the ambient occlusion of the surface. The ambient occlusion is multiplied with the ambient light, then the result is added to the lighting contribution. This property has no visual impact on scenes that have no ambient light. When an ambient occlusion map is set, the ambient property is ignored.
--
-- ObjC selector: @- ambientOcclusion@
ambientOcclusion :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
ambientOcclusion scnMaterial =
  sendMessage scnMaterial ambientOcclusionSelector

-- | selfIllumination
--
-- The selfIllumination property specifies a texture or a color that is added to the lighting contribution of the surface. When a selfIllumination is set, the emission property is ignored.
--
-- ObjC selector: @- selfIllumination@
selfIllumination :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
selfIllumination scnMaterial =
  sendMessage scnMaterial selfIlluminationSelector

-- | metalness
--
-- The metalness property specifies how metallic the material's surface appears. Lower values (darker colors) cause the material to appear more like a dielectric surface. Higher values (brighter colors) cause the surface to appear more metallic. This property is only used when 'lightingModelName' is 'SCNLightingModelPhysicallyBased'.
--
-- ObjC selector: @- metalness@
metalness :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
metalness scnMaterial =
  sendMessage scnMaterial metalnessSelector

-- | roughness
--
-- The roughness property specifies the apparent smoothness of the surface. Lower values (darker colors) cause the material to appear shiny, with well-defined specular highlights. Higher values (brighter colors) cause specular highlights to spread out and the diffuse property of the material to become more retroreflective. This property is only used when 'lightingModelName' is 'SCNLightingModelPhysicallyBased'.
--
-- ObjC selector: @- roughness@
roughness :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
roughness scnMaterial =
  sendMessage scnMaterial roughnessSelector

-- | clearCoat
--
-- The clearCoat property specifies color and intensity of the coat layer.
--
-- ObjC selector: @- clearCoat@
clearCoat :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
clearCoat scnMaterial =
  sendMessage scnMaterial clearCoatSelector

-- | clearCoatRoughness
--
-- The clearCoat property specifies color and intensity of the coat roughness.
--
-- ObjC selector: @- clearCoatRoughness@
clearCoatRoughness :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
clearCoatRoughness scnMaterial =
  sendMessage scnMaterial clearCoatRoughnessSelector

-- | clearCoatNormal
--
-- The clearCoatNormal property specifies color and intensity of the optional coat normal map.
--
-- ObjC selector: @- clearCoatNormal@
clearCoatNormal :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
clearCoatNormal scnMaterial =
  sendMessage scnMaterial clearCoatNormalSelector

-- | shininess
--
-- Specifies the receiver's shininess value. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- shininess@
shininess :: IsSCNMaterial scnMaterial => scnMaterial -> IO CDouble
shininess scnMaterial =
  sendMessage scnMaterial shininessSelector

-- | shininess
--
-- Specifies the receiver's shininess value. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- setShininess:@
setShininess :: IsSCNMaterial scnMaterial => scnMaterial -> CDouble -> IO ()
setShininess scnMaterial value =
  sendMessage scnMaterial setShininessSelector value

-- | transparency
--
-- Specifies the receiver's transparency value. Defaults to 1.0. Animatable.
--
-- The color of the transparent property is multiplied by this property. The result is then used to produce the final transparency according to the rule defined by the transparencyMode property.
--
-- ObjC selector: @- transparency@
transparency :: IsSCNMaterial scnMaterial => scnMaterial -> IO CDouble
transparency scnMaterial =
  sendMessage scnMaterial transparencySelector

-- | transparency
--
-- Specifies the receiver's transparency value. Defaults to 1.0. Animatable.
--
-- The color of the transparent property is multiplied by this property. The result is then used to produce the final transparency according to the rule defined by the transparencyMode property.
--
-- ObjC selector: @- setTransparency:@
setTransparency :: IsSCNMaterial scnMaterial => scnMaterial -> CDouble -> IO ()
setTransparency scnMaterial value =
  sendMessage scnMaterial setTransparencySelector value

-- | lightingModelName
--
-- Determines the receiver's lighting model. See above for the list of lighting models. Defaults to SCNLightingModelBlinn.
--
-- ObjC selector: @- lightingModelName@
lightingModelName :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id NSString)
lightingModelName scnMaterial =
  sendMessage scnMaterial lightingModelNameSelector

-- | lightingModelName
--
-- Determines the receiver's lighting model. See above for the list of lighting models. Defaults to SCNLightingModelBlinn.
--
-- ObjC selector: @- setLightingModelName:@
setLightingModelName :: (IsSCNMaterial scnMaterial, IsNSString value) => scnMaterial -> value -> IO ()
setLightingModelName scnMaterial value =
  sendMessage scnMaterial setLightingModelNameSelector (toNSString value)

-- | litPerPixel
--
-- Determines whether the receiver is lit per pixel. Defaults to YES. Animatable.
--
-- ObjC selector: @- litPerPixel@
litPerPixel :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
litPerPixel scnMaterial =
  sendMessage scnMaterial litPerPixelSelector

-- | litPerPixel
--
-- Determines whether the receiver is lit per pixel. Defaults to YES. Animatable.
--
-- ObjC selector: @- setLitPerPixel:@
setLitPerPixel :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setLitPerPixel scnMaterial value =
  sendMessage scnMaterial setLitPerPixelSelector value

-- | doubleSided
--
-- Determines whether the receiver is double sided. Defaults to NO. Animatable.
--
-- ObjC selector: @- doubleSided@
doubleSided :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
doubleSided scnMaterial =
  sendMessage scnMaterial doubleSidedSelector

-- | doubleSided
--
-- Determines whether the receiver is double sided. Defaults to NO. Animatable.
--
-- ObjC selector: @- setDoubleSided:@
setDoubleSided :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setDoubleSided scnMaterial value =
  sendMessage scnMaterial setDoubleSidedSelector value

-- | fillMode
--
-- Determines of to how to rasterize the receiver's primitives. Defaults to SCNFillModeFill.
--
-- ObjC selector: @- fillMode@
fillMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNFillMode
fillMode scnMaterial =
  sendMessage scnMaterial fillModeSelector

-- | fillMode
--
-- Determines of to how to rasterize the receiver's primitives. Defaults to SCNFillModeFill.
--
-- ObjC selector: @- setFillMode:@
setFillMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNFillMode -> IO ()
setFillMode scnMaterial value =
  sendMessage scnMaterial setFillModeSelector value

-- | cullMode
--
-- Determines the culling mode of the receiver. Defaults to SCNCullModeBack. Animatable.
--
-- ObjC selector: @- cullMode@
cullMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNCullMode
cullMode scnMaterial =
  sendMessage scnMaterial cullModeSelector

-- | cullMode
--
-- Determines the culling mode of the receiver. Defaults to SCNCullModeBack. Animatable.
--
-- ObjC selector: @- setCullMode:@
setCullMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNCullMode -> IO ()
setCullMode scnMaterial value =
  sendMessage scnMaterial setCullModeSelector value

-- | transparencyMode
--
-- Determines the transparency mode of the receiver. See above for the transparency modes. Defaults to SCNTransparencyModeDefault.
--
-- ObjC selector: @- transparencyMode@
transparencyMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNTransparencyMode
transparencyMode scnMaterial =
  sendMessage scnMaterial transparencyModeSelector

-- | transparencyMode
--
-- Determines the transparency mode of the receiver. See above for the transparency modes. Defaults to SCNTransparencyModeDefault.
--
-- ObjC selector: @- setTransparencyMode:@
setTransparencyMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNTransparencyMode -> IO ()
setTransparencyMode scnMaterial value =
  sendMessage scnMaterial setTransparencyModeSelector value

-- | locksAmbientWithDiffuse
--
-- Makes the ambient property automatically match the diffuse property. Defaults to NO on 10.9 and before, defaults to YES otherwise. Animatable.
--
-- ObjC selector: @- locksAmbientWithDiffuse@
locksAmbientWithDiffuse :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
locksAmbientWithDiffuse scnMaterial =
  sendMessage scnMaterial locksAmbientWithDiffuseSelector

-- | locksAmbientWithDiffuse
--
-- Makes the ambient property automatically match the diffuse property. Defaults to NO on 10.9 and before, defaults to YES otherwise. Animatable.
--
-- ObjC selector: @- setLocksAmbientWithDiffuse:@
setLocksAmbientWithDiffuse :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setLocksAmbientWithDiffuse scnMaterial value =
  sendMessage scnMaterial setLocksAmbientWithDiffuseSelector value

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- writesToDepthBuffer@
writesToDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
writesToDepthBuffer scnMaterial =
  sendMessage scnMaterial writesToDepthBufferSelector

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- setWritesToDepthBuffer:@
setWritesToDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setWritesToDepthBuffer scnMaterial value =
  sendMessage scnMaterial setWritesToDepthBufferSelector value

-- | Determines whether the receiver writes to the color buffer when rendered. Defaults to SCNColorMaskAll.
--
-- ObjC selector: @- colorBufferWriteMask@
colorBufferWriteMask :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNColorMask
colorBufferWriteMask scnMaterial =
  sendMessage scnMaterial colorBufferWriteMaskSelector

-- | Determines whether the receiver writes to the color buffer when rendered. Defaults to SCNColorMaskAll.
--
-- ObjC selector: @- setColorBufferWriteMask:@
setColorBufferWriteMask :: IsSCNMaterial scnMaterial => scnMaterial -> SCNColorMask -> IO ()
setColorBufferWriteMask scnMaterial value =
  sendMessage scnMaterial setColorBufferWriteMaskSelector value

-- | readsFromDepthBuffer
--
-- Determines whether the receiver reads from the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- readsFromDepthBuffer@
readsFromDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
readsFromDepthBuffer scnMaterial =
  sendMessage scnMaterial readsFromDepthBufferSelector

-- | readsFromDepthBuffer
--
-- Determines whether the receiver reads from the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- setReadsFromDepthBuffer:@
setReadsFromDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setReadsFromDepthBuffer scnMaterial value =
  sendMessage scnMaterial setReadsFromDepthBufferSelector value

-- | fresnelExponent
--
-- Specifies the receiver's fresnel exponent value. Defaults to 0.0. Animatable.
--
-- The effect of the reflectivity property is modulated by this property. The fresnelExponent changes the exponent of the reflectance. The bigger the exponent, the more concentrated the reflection is around the edges.
--
-- ObjC selector: @- fresnelExponent@
fresnelExponent :: IsSCNMaterial scnMaterial => scnMaterial -> IO CDouble
fresnelExponent scnMaterial =
  sendMessage scnMaterial fresnelExponentSelector

-- | fresnelExponent
--
-- Specifies the receiver's fresnel exponent value. Defaults to 0.0. Animatable.
--
-- The effect of the reflectivity property is modulated by this property. The fresnelExponent changes the exponent of the reflectance. The bigger the exponent, the more concentrated the reflection is around the edges.
--
-- ObjC selector: @- setFresnelExponent:@
setFresnelExponent :: IsSCNMaterial scnMaterial => scnMaterial -> CDouble -> IO ()
setFresnelExponent scnMaterial value =
  sendMessage scnMaterial setFresnelExponentSelector value

-- | blendMode
--
-- Specifies the receiver's blend mode. Defaults to SCNBlendModeAlpha.
--
-- ObjC selector: @- blendMode@
blendMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNBlendMode
blendMode scnMaterial =
  sendMessage scnMaterial blendModeSelector

-- | blendMode
--
-- Specifies the receiver's blend mode. Defaults to SCNBlendModeAlpha.
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNBlendMode -> IO ()
setBlendMode scnMaterial value =
  sendMessage scnMaterial setBlendModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @material@
materialSelector :: Selector '[] (Id SCNMaterial)
materialSelector = mkSelector "material"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @diffuse@
diffuseSelector :: Selector '[] (Id SCNMaterialProperty)
diffuseSelector = mkSelector "diffuse"

-- | @Selector@ for @ambient@
ambientSelector :: Selector '[] (Id SCNMaterialProperty)
ambientSelector = mkSelector "ambient"

-- | @Selector@ for @specular@
specularSelector :: Selector '[] (Id SCNMaterialProperty)
specularSelector = mkSelector "specular"

-- | @Selector@ for @emission@
emissionSelector :: Selector '[] (Id SCNMaterialProperty)
emissionSelector = mkSelector "emission"

-- | @Selector@ for @transparent@
transparentSelector :: Selector '[] (Id SCNMaterialProperty)
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @reflective@
reflectiveSelector :: Selector '[] (Id SCNMaterialProperty)
reflectiveSelector = mkSelector "reflective"

-- | @Selector@ for @multiply@
multiplySelector :: Selector '[] (Id SCNMaterialProperty)
multiplySelector = mkSelector "multiply"

-- | @Selector@ for @normal@
normalSelector :: Selector '[] (Id SCNMaterialProperty)
normalSelector = mkSelector "normal"

-- | @Selector@ for @displacement@
displacementSelector :: Selector '[] (Id SCNMaterialProperty)
displacementSelector = mkSelector "displacement"

-- | @Selector@ for @ambientOcclusion@
ambientOcclusionSelector :: Selector '[] (Id SCNMaterialProperty)
ambientOcclusionSelector = mkSelector "ambientOcclusion"

-- | @Selector@ for @selfIllumination@
selfIlluminationSelector :: Selector '[] (Id SCNMaterialProperty)
selfIlluminationSelector = mkSelector "selfIllumination"

-- | @Selector@ for @metalness@
metalnessSelector :: Selector '[] (Id SCNMaterialProperty)
metalnessSelector = mkSelector "metalness"

-- | @Selector@ for @roughness@
roughnessSelector :: Selector '[] (Id SCNMaterialProperty)
roughnessSelector = mkSelector "roughness"

-- | @Selector@ for @clearCoat@
clearCoatSelector :: Selector '[] (Id SCNMaterialProperty)
clearCoatSelector = mkSelector "clearCoat"

-- | @Selector@ for @clearCoatRoughness@
clearCoatRoughnessSelector :: Selector '[] (Id SCNMaterialProperty)
clearCoatRoughnessSelector = mkSelector "clearCoatRoughness"

-- | @Selector@ for @clearCoatNormal@
clearCoatNormalSelector :: Selector '[] (Id SCNMaterialProperty)
clearCoatNormalSelector = mkSelector "clearCoatNormal"

-- | @Selector@ for @shininess@
shininessSelector :: Selector '[] CDouble
shininessSelector = mkSelector "shininess"

-- | @Selector@ for @setShininess:@
setShininessSelector :: Selector '[CDouble] ()
setShininessSelector = mkSelector "setShininess:"

-- | @Selector@ for @transparency@
transparencySelector :: Selector '[] CDouble
transparencySelector = mkSelector "transparency"

-- | @Selector@ for @setTransparency:@
setTransparencySelector :: Selector '[CDouble] ()
setTransparencySelector = mkSelector "setTransparency:"

-- | @Selector@ for @lightingModelName@
lightingModelNameSelector :: Selector '[] (Id NSString)
lightingModelNameSelector = mkSelector "lightingModelName"

-- | @Selector@ for @setLightingModelName:@
setLightingModelNameSelector :: Selector '[Id NSString] ()
setLightingModelNameSelector = mkSelector "setLightingModelName:"

-- | @Selector@ for @litPerPixel@
litPerPixelSelector :: Selector '[] Bool
litPerPixelSelector = mkSelector "litPerPixel"

-- | @Selector@ for @setLitPerPixel:@
setLitPerPixelSelector :: Selector '[Bool] ()
setLitPerPixelSelector = mkSelector "setLitPerPixel:"

-- | @Selector@ for @doubleSided@
doubleSidedSelector :: Selector '[] Bool
doubleSidedSelector = mkSelector "doubleSided"

-- | @Selector@ for @setDoubleSided:@
setDoubleSidedSelector :: Selector '[Bool] ()
setDoubleSidedSelector = mkSelector "setDoubleSided:"

-- | @Selector@ for @fillMode@
fillModeSelector :: Selector '[] SCNFillMode
fillModeSelector = mkSelector "fillMode"

-- | @Selector@ for @setFillMode:@
setFillModeSelector :: Selector '[SCNFillMode] ()
setFillModeSelector = mkSelector "setFillMode:"

-- | @Selector@ for @cullMode@
cullModeSelector :: Selector '[] SCNCullMode
cullModeSelector = mkSelector "cullMode"

-- | @Selector@ for @setCullMode:@
setCullModeSelector :: Selector '[SCNCullMode] ()
setCullModeSelector = mkSelector "setCullMode:"

-- | @Selector@ for @transparencyMode@
transparencyModeSelector :: Selector '[] SCNTransparencyMode
transparencyModeSelector = mkSelector "transparencyMode"

-- | @Selector@ for @setTransparencyMode:@
setTransparencyModeSelector :: Selector '[SCNTransparencyMode] ()
setTransparencyModeSelector = mkSelector "setTransparencyMode:"

-- | @Selector@ for @locksAmbientWithDiffuse@
locksAmbientWithDiffuseSelector :: Selector '[] Bool
locksAmbientWithDiffuseSelector = mkSelector "locksAmbientWithDiffuse"

-- | @Selector@ for @setLocksAmbientWithDiffuse:@
setLocksAmbientWithDiffuseSelector :: Selector '[Bool] ()
setLocksAmbientWithDiffuseSelector = mkSelector "setLocksAmbientWithDiffuse:"

-- | @Selector@ for @writesToDepthBuffer@
writesToDepthBufferSelector :: Selector '[] Bool
writesToDepthBufferSelector = mkSelector "writesToDepthBuffer"

-- | @Selector@ for @setWritesToDepthBuffer:@
setWritesToDepthBufferSelector :: Selector '[Bool] ()
setWritesToDepthBufferSelector = mkSelector "setWritesToDepthBuffer:"

-- | @Selector@ for @colorBufferWriteMask@
colorBufferWriteMaskSelector :: Selector '[] SCNColorMask
colorBufferWriteMaskSelector = mkSelector "colorBufferWriteMask"

-- | @Selector@ for @setColorBufferWriteMask:@
setColorBufferWriteMaskSelector :: Selector '[SCNColorMask] ()
setColorBufferWriteMaskSelector = mkSelector "setColorBufferWriteMask:"

-- | @Selector@ for @readsFromDepthBuffer@
readsFromDepthBufferSelector :: Selector '[] Bool
readsFromDepthBufferSelector = mkSelector "readsFromDepthBuffer"

-- | @Selector@ for @setReadsFromDepthBuffer:@
setReadsFromDepthBufferSelector :: Selector '[Bool] ()
setReadsFromDepthBufferSelector = mkSelector "setReadsFromDepthBuffer:"

-- | @Selector@ for @fresnelExponent@
fresnelExponentSelector :: Selector '[] CDouble
fresnelExponentSelector = mkSelector "fresnelExponent"

-- | @Selector@ for @setFresnelExponent:@
setFresnelExponentSelector :: Selector '[CDouble] ()
setFresnelExponentSelector = mkSelector "setFresnelExponent:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector '[] SCNBlendMode
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector '[SCNBlendMode] ()
setBlendModeSelector = mkSelector "setBlendMode:"

