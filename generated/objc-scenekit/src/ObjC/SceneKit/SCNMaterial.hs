{-# LANGUAGE PatternSynonyms #-}
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
  , materialSelector
  , nameSelector
  , setNameSelector
  , diffuseSelector
  , ambientSelector
  , specularSelector
  , emissionSelector
  , transparentSelector
  , reflectiveSelector
  , multiplySelector
  , normalSelector
  , displacementSelector
  , ambientOcclusionSelector
  , selfIlluminationSelector
  , metalnessSelector
  , roughnessSelector
  , clearCoatSelector
  , clearCoatRoughnessSelector
  , clearCoatNormalSelector
  , shininessSelector
  , setShininessSelector
  , transparencySelector
  , setTransparencySelector
  , lightingModelNameSelector
  , setLightingModelNameSelector
  , litPerPixelSelector
  , setLitPerPixelSelector
  , doubleSidedSelector
  , setDoubleSidedSelector
  , fillModeSelector
  , setFillModeSelector
  , cullModeSelector
  , setCullModeSelector
  , transparencyModeSelector
  , setTransparencyModeSelector
  , locksAmbientWithDiffuseSelector
  , setLocksAmbientWithDiffuseSelector
  , writesToDepthBufferSelector
  , setWritesToDepthBufferSelector
  , colorBufferWriteMaskSelector
  , setColorBufferWriteMaskSelector
  , readsFromDepthBufferSelector
  , setReadsFromDepthBufferSelector
  , fresnelExponentSelector
  , setFresnelExponentSelector
  , blendModeSelector
  , setBlendModeSelector

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
    sendClassMsg cls' (mkSelector "material") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- name@
name :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id NSString)
name scnMaterial  =
  sendMsg scnMaterial (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Determines the name of the receiver.
--
-- ObjC selector: @- setName:@
setName :: (IsSCNMaterial scnMaterial, IsNSString value) => scnMaterial -> value -> IO ()
setName scnMaterial  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnMaterial (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | diffuse
--
-- Specifies the receiver's diffuse property.
--
-- The diffuse property specifies the amount of light diffusely reflected from the surface. The diffuse light is reflected equally in all directions and is therefore independent of the point of view.
--
-- ObjC selector: @- diffuse@
diffuse :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
diffuse scnMaterial  =
  sendMsg scnMaterial (mkSelector "diffuse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ambient
--
-- Specifies the receiver's ambient property.
--
-- The ambient property specifies the amount of ambient light to reflect. This property has no visual impact on scenes that have no ambient light. Setting the ambient has no effect if locksAmbientWithDiffuse is set to YES.
--
-- ObjC selector: @- ambient@
ambient :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
ambient scnMaterial  =
  sendMsg scnMaterial (mkSelector "ambient") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | specular
--
-- Specifies the receiver's specular property.
--
-- The specular property specifies the amount of light to reflect in a mirror-like manner. The specular intensity increases when the point of view lines up with the direction of the reflected light.
--
-- ObjC selector: @- specular@
specular :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
specular scnMaterial  =
  sendMsg scnMaterial (mkSelector "specular") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | emission
--
-- The emission property specifies the amount of light the material emits. This emission does not light up other surfaces in the scene.
--
-- ObjC selector: @- emission@
emission :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
emission scnMaterial  =
  sendMsg scnMaterial (mkSelector "emission") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | transparent
--
-- The transparent property specifies the transparent areas of the material.
--
-- ObjC selector: @- transparent@
transparent :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
transparent scnMaterial  =
  sendMsg scnMaterial (mkSelector "transparent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | reflective
--
-- The reflective property specifies the reflectivity of the surface. The surface will not actually reflect other objects in the scene. This property may be used as a sphere mapping to reflect a precomputed environment.
--
-- ObjC selector: @- reflective@
reflective :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
reflective scnMaterial  =
  sendMsg scnMaterial (mkSelector "reflective") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | multiply
--
-- The multiply property specifies a color or an image used to multiply the output fragments with. The computed fragments are multiplied with the multiply value to produce the final fragments. This property may be used for shadow maps, to fade out or tint 3d objects.
--
-- ObjC selector: @- multiply@
multiply :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
multiply scnMaterial  =
  sendMsg scnMaterial (mkSelector "multiply") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | normal
--
-- The normal property specifies the surface orientation.
--
-- When an image is set on the normal property the material is automatically lit per pixel. Setting a color has no effect.
--
-- ObjC selector: @- normal@
normal :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
normal scnMaterial  =
  sendMsg scnMaterial (mkSelector "normal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | displacement
--
-- The displacement property specifies how vertex are translated in tangent space.
--
-- Pass a grayscale image for a simple 'elevation' or rgb image for a vector displacement.
--
-- ObjC selector: @- displacement@
displacement :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
displacement scnMaterial  =
  sendMsg scnMaterial (mkSelector "displacement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ambientOcclusion
--
-- The ambientOcclusion property specifies the ambient occlusion of the surface. The ambient occlusion is multiplied with the ambient light, then the result is added to the lighting contribution. This property has no visual impact on scenes that have no ambient light. When an ambient occlusion map is set, the ambient property is ignored.
--
-- ObjC selector: @- ambientOcclusion@
ambientOcclusion :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
ambientOcclusion scnMaterial  =
  sendMsg scnMaterial (mkSelector "ambientOcclusion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | selfIllumination
--
-- The selfIllumination property specifies a texture or a color that is added to the lighting contribution of the surface. When a selfIllumination is set, the emission property is ignored.
--
-- ObjC selector: @- selfIllumination@
selfIllumination :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
selfIllumination scnMaterial  =
  sendMsg scnMaterial (mkSelector "selfIllumination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | metalness
--
-- The metalness property specifies how metallic the material's surface appears. Lower values (darker colors) cause the material to appear more like a dielectric surface. Higher values (brighter colors) cause the surface to appear more metallic. This property is only used when 'lightingModelName' is 'SCNLightingModelPhysicallyBased'.
--
-- ObjC selector: @- metalness@
metalness :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
metalness scnMaterial  =
  sendMsg scnMaterial (mkSelector "metalness") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | roughness
--
-- The roughness property specifies the apparent smoothness of the surface. Lower values (darker colors) cause the material to appear shiny, with well-defined specular highlights. Higher values (brighter colors) cause specular highlights to spread out and the diffuse property of the material to become more retroreflective. This property is only used when 'lightingModelName' is 'SCNLightingModelPhysicallyBased'.
--
-- ObjC selector: @- roughness@
roughness :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
roughness scnMaterial  =
  sendMsg scnMaterial (mkSelector "roughness") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | clearCoat
--
-- The clearCoat property specifies color and intensity of the coat layer.
--
-- ObjC selector: @- clearCoat@
clearCoat :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
clearCoat scnMaterial  =
  sendMsg scnMaterial (mkSelector "clearCoat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | clearCoatRoughness
--
-- The clearCoat property specifies color and intensity of the coat roughness.
--
-- ObjC selector: @- clearCoatRoughness@
clearCoatRoughness :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
clearCoatRoughness scnMaterial  =
  sendMsg scnMaterial (mkSelector "clearCoatRoughness") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | clearCoatNormal
--
-- The clearCoatNormal property specifies color and intensity of the optional coat normal map.
--
-- ObjC selector: @- clearCoatNormal@
clearCoatNormal :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id SCNMaterialProperty)
clearCoatNormal scnMaterial  =
  sendMsg scnMaterial (mkSelector "clearCoatNormal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | shininess
--
-- Specifies the receiver's shininess value. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- shininess@
shininess :: IsSCNMaterial scnMaterial => scnMaterial -> IO CDouble
shininess scnMaterial  =
  sendMsg scnMaterial (mkSelector "shininess") retCDouble []

-- | shininess
--
-- Specifies the receiver's shininess value. Defaults to 1.0. Animatable.
--
-- ObjC selector: @- setShininess:@
setShininess :: IsSCNMaterial scnMaterial => scnMaterial -> CDouble -> IO ()
setShininess scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setShininess:") retVoid [argCDouble (fromIntegral value)]

-- | transparency
--
-- Specifies the receiver's transparency value. Defaults to 1.0. Animatable.
--
-- The color of the transparent property is multiplied by this property. The result is then used to produce the final transparency according to the rule defined by the transparencyMode property.
--
-- ObjC selector: @- transparency@
transparency :: IsSCNMaterial scnMaterial => scnMaterial -> IO CDouble
transparency scnMaterial  =
  sendMsg scnMaterial (mkSelector "transparency") retCDouble []

-- | transparency
--
-- Specifies the receiver's transparency value. Defaults to 1.0. Animatable.
--
-- The color of the transparent property is multiplied by this property. The result is then used to produce the final transparency according to the rule defined by the transparencyMode property.
--
-- ObjC selector: @- setTransparency:@
setTransparency :: IsSCNMaterial scnMaterial => scnMaterial -> CDouble -> IO ()
setTransparency scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setTransparency:") retVoid [argCDouble (fromIntegral value)]

-- | lightingModelName
--
-- Determines the receiver's lighting model. See above for the list of lighting models. Defaults to SCNLightingModelBlinn.
--
-- ObjC selector: @- lightingModelName@
lightingModelName :: IsSCNMaterial scnMaterial => scnMaterial -> IO (Id NSString)
lightingModelName scnMaterial  =
  sendMsg scnMaterial (mkSelector "lightingModelName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lightingModelName
--
-- Determines the receiver's lighting model. See above for the list of lighting models. Defaults to SCNLightingModelBlinn.
--
-- ObjC selector: @- setLightingModelName:@
setLightingModelName :: (IsSCNMaterial scnMaterial, IsNSString value) => scnMaterial -> value -> IO ()
setLightingModelName scnMaterial  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnMaterial (mkSelector "setLightingModelName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | litPerPixel
--
-- Determines whether the receiver is lit per pixel. Defaults to YES. Animatable.
--
-- ObjC selector: @- litPerPixel@
litPerPixel :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
litPerPixel scnMaterial  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnMaterial (mkSelector "litPerPixel") retCULong []

-- | litPerPixel
--
-- Determines whether the receiver is lit per pixel. Defaults to YES. Animatable.
--
-- ObjC selector: @- setLitPerPixel:@
setLitPerPixel :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setLitPerPixel scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setLitPerPixel:") retVoid [argCULong (if value then 1 else 0)]

-- | doubleSided
--
-- Determines whether the receiver is double sided. Defaults to NO. Animatable.
--
-- ObjC selector: @- doubleSided@
doubleSided :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
doubleSided scnMaterial  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnMaterial (mkSelector "doubleSided") retCULong []

-- | doubleSided
--
-- Determines whether the receiver is double sided. Defaults to NO. Animatable.
--
-- ObjC selector: @- setDoubleSided:@
setDoubleSided :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setDoubleSided scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setDoubleSided:") retVoid [argCULong (if value then 1 else 0)]

-- | fillMode
--
-- Determines of to how to rasterize the receiver's primitives. Defaults to SCNFillModeFill.
--
-- ObjC selector: @- fillMode@
fillMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNFillMode
fillMode scnMaterial  =
  fmap (coerce :: CULong -> SCNFillMode) $ sendMsg scnMaterial (mkSelector "fillMode") retCULong []

-- | fillMode
--
-- Determines of to how to rasterize the receiver's primitives. Defaults to SCNFillModeFill.
--
-- ObjC selector: @- setFillMode:@
setFillMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNFillMode -> IO ()
setFillMode scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setFillMode:") retVoid [argCULong (coerce value)]

-- | cullMode
--
-- Determines the culling mode of the receiver. Defaults to SCNCullModeBack. Animatable.
--
-- ObjC selector: @- cullMode@
cullMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNCullMode
cullMode scnMaterial  =
  fmap (coerce :: CLong -> SCNCullMode) $ sendMsg scnMaterial (mkSelector "cullMode") retCLong []

-- | cullMode
--
-- Determines the culling mode of the receiver. Defaults to SCNCullModeBack. Animatable.
--
-- ObjC selector: @- setCullMode:@
setCullMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNCullMode -> IO ()
setCullMode scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setCullMode:") retVoid [argCLong (coerce value)]

-- | transparencyMode
--
-- Determines the transparency mode of the receiver. See above for the transparency modes. Defaults to SCNTransparencyModeDefault.
--
-- ObjC selector: @- transparencyMode@
transparencyMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNTransparencyMode
transparencyMode scnMaterial  =
  fmap (coerce :: CLong -> SCNTransparencyMode) $ sendMsg scnMaterial (mkSelector "transparencyMode") retCLong []

-- | transparencyMode
--
-- Determines the transparency mode of the receiver. See above for the transparency modes. Defaults to SCNTransparencyModeDefault.
--
-- ObjC selector: @- setTransparencyMode:@
setTransparencyMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNTransparencyMode -> IO ()
setTransparencyMode scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setTransparencyMode:") retVoid [argCLong (coerce value)]

-- | locksAmbientWithDiffuse
--
-- Makes the ambient property automatically match the diffuse property. Defaults to NO on 10.9 and before, defaults to YES otherwise. Animatable.
--
-- ObjC selector: @- locksAmbientWithDiffuse@
locksAmbientWithDiffuse :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
locksAmbientWithDiffuse scnMaterial  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnMaterial (mkSelector "locksAmbientWithDiffuse") retCULong []

-- | locksAmbientWithDiffuse
--
-- Makes the ambient property automatically match the diffuse property. Defaults to NO on 10.9 and before, defaults to YES otherwise. Animatable.
--
-- ObjC selector: @- setLocksAmbientWithDiffuse:@
setLocksAmbientWithDiffuse :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setLocksAmbientWithDiffuse scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setLocksAmbientWithDiffuse:") retVoid [argCULong (if value then 1 else 0)]

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- writesToDepthBuffer@
writesToDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
writesToDepthBuffer scnMaterial  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnMaterial (mkSelector "writesToDepthBuffer") retCULong []

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- setWritesToDepthBuffer:@
setWritesToDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setWritesToDepthBuffer scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setWritesToDepthBuffer:") retVoid [argCULong (if value then 1 else 0)]

-- | Determines whether the receiver writes to the color buffer when rendered. Defaults to SCNColorMaskAll.
--
-- ObjC selector: @- colorBufferWriteMask@
colorBufferWriteMask :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNColorMask
colorBufferWriteMask scnMaterial  =
  fmap (coerce :: CLong -> SCNColorMask) $ sendMsg scnMaterial (mkSelector "colorBufferWriteMask") retCLong []

-- | Determines whether the receiver writes to the color buffer when rendered. Defaults to SCNColorMaskAll.
--
-- ObjC selector: @- setColorBufferWriteMask:@
setColorBufferWriteMask :: IsSCNMaterial scnMaterial => scnMaterial -> SCNColorMask -> IO ()
setColorBufferWriteMask scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setColorBufferWriteMask:") retVoid [argCLong (coerce value)]

-- | readsFromDepthBuffer
--
-- Determines whether the receiver reads from the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- readsFromDepthBuffer@
readsFromDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> IO Bool
readsFromDepthBuffer scnMaterial  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnMaterial (mkSelector "readsFromDepthBuffer") retCULong []

-- | readsFromDepthBuffer
--
-- Determines whether the receiver reads from the depth buffer when rendered. Defaults to YES.
--
-- ObjC selector: @- setReadsFromDepthBuffer:@
setReadsFromDepthBuffer :: IsSCNMaterial scnMaterial => scnMaterial -> Bool -> IO ()
setReadsFromDepthBuffer scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setReadsFromDepthBuffer:") retVoid [argCULong (if value then 1 else 0)]

-- | fresnelExponent
--
-- Specifies the receiver's fresnel exponent value. Defaults to 0.0. Animatable.
--
-- The effect of the reflectivity property is modulated by this property. The fresnelExponent changes the exponent of the reflectance. The bigger the exponent, the more concentrated the reflection is around the edges.
--
-- ObjC selector: @- fresnelExponent@
fresnelExponent :: IsSCNMaterial scnMaterial => scnMaterial -> IO CDouble
fresnelExponent scnMaterial  =
  sendMsg scnMaterial (mkSelector "fresnelExponent") retCDouble []

-- | fresnelExponent
--
-- Specifies the receiver's fresnel exponent value. Defaults to 0.0. Animatable.
--
-- The effect of the reflectivity property is modulated by this property. The fresnelExponent changes the exponent of the reflectance. The bigger the exponent, the more concentrated the reflection is around the edges.
--
-- ObjC selector: @- setFresnelExponent:@
setFresnelExponent :: IsSCNMaterial scnMaterial => scnMaterial -> CDouble -> IO ()
setFresnelExponent scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setFresnelExponent:") retVoid [argCDouble (fromIntegral value)]

-- | blendMode
--
-- Specifies the receiver's blend mode. Defaults to SCNBlendModeAlpha.
--
-- ObjC selector: @- blendMode@
blendMode :: IsSCNMaterial scnMaterial => scnMaterial -> IO SCNBlendMode
blendMode scnMaterial  =
  fmap (coerce :: CLong -> SCNBlendMode) $ sendMsg scnMaterial (mkSelector "blendMode") retCLong []

-- | blendMode
--
-- Specifies the receiver's blend mode. Defaults to SCNBlendModeAlpha.
--
-- ObjC selector: @- setBlendMode:@
setBlendMode :: IsSCNMaterial scnMaterial => scnMaterial -> SCNBlendMode -> IO ()
setBlendMode scnMaterial  value =
  sendMsg scnMaterial (mkSelector "setBlendMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @material@
materialSelector :: Selector
materialSelector = mkSelector "material"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @diffuse@
diffuseSelector :: Selector
diffuseSelector = mkSelector "diffuse"

-- | @Selector@ for @ambient@
ambientSelector :: Selector
ambientSelector = mkSelector "ambient"

-- | @Selector@ for @specular@
specularSelector :: Selector
specularSelector = mkSelector "specular"

-- | @Selector@ for @emission@
emissionSelector :: Selector
emissionSelector = mkSelector "emission"

-- | @Selector@ for @transparent@
transparentSelector :: Selector
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @reflective@
reflectiveSelector :: Selector
reflectiveSelector = mkSelector "reflective"

-- | @Selector@ for @multiply@
multiplySelector :: Selector
multiplySelector = mkSelector "multiply"

-- | @Selector@ for @normal@
normalSelector :: Selector
normalSelector = mkSelector "normal"

-- | @Selector@ for @displacement@
displacementSelector :: Selector
displacementSelector = mkSelector "displacement"

-- | @Selector@ for @ambientOcclusion@
ambientOcclusionSelector :: Selector
ambientOcclusionSelector = mkSelector "ambientOcclusion"

-- | @Selector@ for @selfIllumination@
selfIlluminationSelector :: Selector
selfIlluminationSelector = mkSelector "selfIllumination"

-- | @Selector@ for @metalness@
metalnessSelector :: Selector
metalnessSelector = mkSelector "metalness"

-- | @Selector@ for @roughness@
roughnessSelector :: Selector
roughnessSelector = mkSelector "roughness"

-- | @Selector@ for @clearCoat@
clearCoatSelector :: Selector
clearCoatSelector = mkSelector "clearCoat"

-- | @Selector@ for @clearCoatRoughness@
clearCoatRoughnessSelector :: Selector
clearCoatRoughnessSelector = mkSelector "clearCoatRoughness"

-- | @Selector@ for @clearCoatNormal@
clearCoatNormalSelector :: Selector
clearCoatNormalSelector = mkSelector "clearCoatNormal"

-- | @Selector@ for @shininess@
shininessSelector :: Selector
shininessSelector = mkSelector "shininess"

-- | @Selector@ for @setShininess:@
setShininessSelector :: Selector
setShininessSelector = mkSelector "setShininess:"

-- | @Selector@ for @transparency@
transparencySelector :: Selector
transparencySelector = mkSelector "transparency"

-- | @Selector@ for @setTransparency:@
setTransparencySelector :: Selector
setTransparencySelector = mkSelector "setTransparency:"

-- | @Selector@ for @lightingModelName@
lightingModelNameSelector :: Selector
lightingModelNameSelector = mkSelector "lightingModelName"

-- | @Selector@ for @setLightingModelName:@
setLightingModelNameSelector :: Selector
setLightingModelNameSelector = mkSelector "setLightingModelName:"

-- | @Selector@ for @litPerPixel@
litPerPixelSelector :: Selector
litPerPixelSelector = mkSelector "litPerPixel"

-- | @Selector@ for @setLitPerPixel:@
setLitPerPixelSelector :: Selector
setLitPerPixelSelector = mkSelector "setLitPerPixel:"

-- | @Selector@ for @doubleSided@
doubleSidedSelector :: Selector
doubleSidedSelector = mkSelector "doubleSided"

-- | @Selector@ for @setDoubleSided:@
setDoubleSidedSelector :: Selector
setDoubleSidedSelector = mkSelector "setDoubleSided:"

-- | @Selector@ for @fillMode@
fillModeSelector :: Selector
fillModeSelector = mkSelector "fillMode"

-- | @Selector@ for @setFillMode:@
setFillModeSelector :: Selector
setFillModeSelector = mkSelector "setFillMode:"

-- | @Selector@ for @cullMode@
cullModeSelector :: Selector
cullModeSelector = mkSelector "cullMode"

-- | @Selector@ for @setCullMode:@
setCullModeSelector :: Selector
setCullModeSelector = mkSelector "setCullMode:"

-- | @Selector@ for @transparencyMode@
transparencyModeSelector :: Selector
transparencyModeSelector = mkSelector "transparencyMode"

-- | @Selector@ for @setTransparencyMode:@
setTransparencyModeSelector :: Selector
setTransparencyModeSelector = mkSelector "setTransparencyMode:"

-- | @Selector@ for @locksAmbientWithDiffuse@
locksAmbientWithDiffuseSelector :: Selector
locksAmbientWithDiffuseSelector = mkSelector "locksAmbientWithDiffuse"

-- | @Selector@ for @setLocksAmbientWithDiffuse:@
setLocksAmbientWithDiffuseSelector :: Selector
setLocksAmbientWithDiffuseSelector = mkSelector "setLocksAmbientWithDiffuse:"

-- | @Selector@ for @writesToDepthBuffer@
writesToDepthBufferSelector :: Selector
writesToDepthBufferSelector = mkSelector "writesToDepthBuffer"

-- | @Selector@ for @setWritesToDepthBuffer:@
setWritesToDepthBufferSelector :: Selector
setWritesToDepthBufferSelector = mkSelector "setWritesToDepthBuffer:"

-- | @Selector@ for @colorBufferWriteMask@
colorBufferWriteMaskSelector :: Selector
colorBufferWriteMaskSelector = mkSelector "colorBufferWriteMask"

-- | @Selector@ for @setColorBufferWriteMask:@
setColorBufferWriteMaskSelector :: Selector
setColorBufferWriteMaskSelector = mkSelector "setColorBufferWriteMask:"

-- | @Selector@ for @readsFromDepthBuffer@
readsFromDepthBufferSelector :: Selector
readsFromDepthBufferSelector = mkSelector "readsFromDepthBuffer"

-- | @Selector@ for @setReadsFromDepthBuffer:@
setReadsFromDepthBufferSelector :: Selector
setReadsFromDepthBufferSelector = mkSelector "setReadsFromDepthBuffer:"

-- | @Selector@ for @fresnelExponent@
fresnelExponentSelector :: Selector
fresnelExponentSelector = mkSelector "fresnelExponent"

-- | @Selector@ for @setFresnelExponent:@
setFresnelExponentSelector :: Selector
setFresnelExponentSelector = mkSelector "setFresnelExponent:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector
setBlendModeSelector = mkSelector "setBlendMode:"

