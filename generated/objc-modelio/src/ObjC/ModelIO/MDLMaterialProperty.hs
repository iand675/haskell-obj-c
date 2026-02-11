{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | If a color is encoded in a floatN property, it is to be interpreted as  a Rec 709 color.
--
-- Generated bindings for @MDLMaterialProperty@.
module ObjC.ModelIO.MDLMaterialProperty
  ( MDLMaterialProperty
  , IsMDLMaterialProperty(..)
  , init_
  , initWithName_semantic
  , initWithName_semantic_float
  , initWithName_semantic_URL
  , initWithName_semantic_string
  , initWithName_semantic_textureSampler
  , initWithName_semantic_color
  , setProperties
  , semantic
  , setSemantic
  , type_
  , setType
  , name
  , setName
  , stringValue
  , setStringValue
  , urlValue
  , setURLValue
  , textureSamplerValue
  , setTextureSamplerValue
  , color
  , setColor
  , floatValue
  , setFloatValue
  , luminance
  , setLuminance
  , initSelector
  , initWithName_semanticSelector
  , initWithName_semantic_floatSelector
  , initWithName_semantic_URLSelector
  , initWithName_semantic_stringSelector
  , initWithName_semantic_textureSamplerSelector
  , initWithName_semantic_colorSelector
  , setPropertiesSelector
  , semanticSelector
  , setSemanticSelector
  , typeSelector
  , setTypeSelector
  , nameSelector
  , setNameSelector
  , stringValueSelector
  , setStringValueSelector
  , urlValueSelector
  , setURLValueSelector
  , textureSamplerValueSelector
  , setTextureSamplerValueSelector
  , colorSelector
  , setColorSelector
  , floatValueSelector
  , setFloatValueSelector
  , luminanceSelector
  , setLuminanceSelector

  -- * Enum types
  , MDLMaterialPropertyType(MDLMaterialPropertyType)
  , pattern MDLMaterialPropertyTypeNone
  , pattern MDLMaterialPropertyTypeString
  , pattern MDLMaterialPropertyTypeURL
  , pattern MDLMaterialPropertyTypeTexture
  , pattern MDLMaterialPropertyTypeColor
  , pattern MDLMaterialPropertyTypeFloat
  , pattern MDLMaterialPropertyTypeFloat2
  , pattern MDLMaterialPropertyTypeFloat3
  , pattern MDLMaterialPropertyTypeFloat4
  , pattern MDLMaterialPropertyTypeMatrix44
  , pattern MDLMaterialPropertyTypeBuffer
  , MDLMaterialSemantic(MDLMaterialSemantic)
  , pattern MDLMaterialSemanticBaseColor
  , pattern MDLMaterialSemanticSubsurface
  , pattern MDLMaterialSemanticMetallic
  , pattern MDLMaterialSemanticSpecular
  , pattern MDLMaterialSemanticSpecularExponent
  , pattern MDLMaterialSemanticSpecularTint
  , pattern MDLMaterialSemanticRoughness
  , pattern MDLMaterialSemanticAnisotropic
  , pattern MDLMaterialSemanticAnisotropicRotation
  , pattern MDLMaterialSemanticSheen
  , pattern MDLMaterialSemanticSheenTint
  , pattern MDLMaterialSemanticClearcoat
  , pattern MDLMaterialSemanticClearcoatGloss
  , pattern MDLMaterialSemanticEmission
  , pattern MDLMaterialSemanticBump
  , pattern MDLMaterialSemanticOpacity
  , pattern MDLMaterialSemanticInterfaceIndexOfRefraction
  , pattern MDLMaterialSemanticMaterialIndexOfRefraction
  , pattern MDLMaterialSemanticObjectSpaceNormal
  , pattern MDLMaterialSemanticTangentSpaceNormal
  , pattern MDLMaterialSemanticDisplacement
  , pattern MDLMaterialSemanticDisplacementScale
  , pattern MDLMaterialSemanticAmbientOcclusion
  , pattern MDLMaterialSemanticAmbientOcclusionScale
  , pattern MDLMaterialSemanticNone
  , pattern MDLMaterialSemanticUserDefined

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id MDLMaterialProperty)
init_ mdlMaterialProperty  =
  sendMsg mdlMaterialProperty (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithName:semantic:@
initWithName_semantic :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> IO (Id MDLMaterialProperty)
initWithName_semantic mdlMaterialProperty  name semantic =
withObjCPtr name $ \raw_name ->
    sendMsg mdlMaterialProperty (mkSelector "initWithName:semantic:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce semantic)] >>= ownedObject . castPtr

-- | @- initWithName:semantic:float:@
initWithName_semantic_float :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> CFloat -> IO (Id MDLMaterialProperty)
initWithName_semantic_float mdlMaterialProperty  name semantic value =
withObjCPtr name $ \raw_name ->
    sendMsg mdlMaterialProperty (mkSelector "initWithName:semantic:float:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce semantic), argCFloat (fromIntegral value)] >>= ownedObject . castPtr

-- | @- initWithName:semantic:URL:@
initWithName_semantic_URL :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name, IsNSURL url) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> url -> IO (Id MDLMaterialProperty)
initWithName_semantic_URL mdlMaterialProperty  name semantic url =
withObjCPtr name $ \raw_name ->
  withObjCPtr url $ \raw_url ->
      sendMsg mdlMaterialProperty (mkSelector "initWithName:semantic:URL:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce semantic), argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:semantic:string:@
initWithName_semantic_string :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name, IsNSString string) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> string -> IO (Id MDLMaterialProperty)
initWithName_semantic_string mdlMaterialProperty  name semantic string =
withObjCPtr name $ \raw_name ->
  withObjCPtr string $ \raw_string ->
      sendMsg mdlMaterialProperty (mkSelector "initWithName:semantic:string:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce semantic), argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:semantic:textureSampler:@
initWithName_semantic_textureSampler :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name, IsMDLTextureSampler textureSampler) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> textureSampler -> IO (Id MDLMaterialProperty)
initWithName_semantic_textureSampler mdlMaterialProperty  name semantic textureSampler =
withObjCPtr name $ \raw_name ->
  withObjCPtr textureSampler $ \raw_textureSampler ->
      sendMsg mdlMaterialProperty (mkSelector "initWithName:semantic:textureSampler:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce semantic), argPtr (castPtr raw_textureSampler :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithName:semantic:color:@
initWithName_semantic_color :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> Ptr () -> IO (Id MDLMaterialProperty)
initWithName_semantic_color mdlMaterialProperty  name semantic color =
withObjCPtr name $ \raw_name ->
    sendMsg mdlMaterialProperty (mkSelector "initWithName:semantic:color:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce semantic), argPtr color] >>= ownedObject . castPtr

-- | @- setProperties:@
setProperties :: (IsMDLMaterialProperty mdlMaterialProperty, IsMDLMaterialProperty property) => mdlMaterialProperty -> property -> IO ()
setProperties mdlMaterialProperty  property =
withObjCPtr property $ \raw_property ->
    sendMsg mdlMaterialProperty (mkSelector "setProperties:") retVoid [argPtr (castPtr raw_property :: Ptr ())]

-- | @- semantic@
semantic :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO MDLMaterialSemantic
semantic mdlMaterialProperty  =
  fmap (coerce :: CULong -> MDLMaterialSemantic) $ sendMsg mdlMaterialProperty (mkSelector "semantic") retCULong []

-- | @- setSemantic:@
setSemantic :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> MDLMaterialSemantic -> IO ()
setSemantic mdlMaterialProperty  value =
  sendMsg mdlMaterialProperty (mkSelector "setSemantic:") retVoid [argCULong (coerce value)]

-- | @- type@
type_ :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO MDLMaterialPropertyType
type_ mdlMaterialProperty  =
  fmap (coerce :: CULong -> MDLMaterialPropertyType) $ sendMsg mdlMaterialProperty (mkSelector "type") retCULong []

-- | @- setType:@
setType :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> MDLMaterialPropertyType -> IO ()
setType mdlMaterialProperty  value =
  sendMsg mdlMaterialProperty (mkSelector "setType:") retVoid [argCULong (coerce value)]

-- | See: MDLNamed
--
-- ObjC selector: @- name@
name :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id NSString)
name mdlMaterialProperty  =
  sendMsg mdlMaterialProperty (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | See: MDLNamed
--
-- ObjC selector: @- setName:@
setName :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString value) => mdlMaterialProperty -> value -> IO ()
setName mdlMaterialProperty  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlMaterialProperty (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stringValue@
stringValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id NSString)
stringValue mdlMaterialProperty  =
  sendMsg mdlMaterialProperty (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStringValue:@
setStringValue :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString value) => mdlMaterialProperty -> value -> IO ()
setStringValue mdlMaterialProperty  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlMaterialProperty (mkSelector "setStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URLValue@
urlValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id NSURL)
urlValue mdlMaterialProperty  =
  sendMsg mdlMaterialProperty (mkSelector "URLValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURLValue:@
setURLValue :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSURL value) => mdlMaterialProperty -> value -> IO ()
setURLValue mdlMaterialProperty  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlMaterialProperty (mkSelector "setURLValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textureSamplerValue@
textureSamplerValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id MDLTextureSampler)
textureSamplerValue mdlMaterialProperty  =
  sendMsg mdlMaterialProperty (mkSelector "textureSamplerValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextureSamplerValue:@
setTextureSamplerValue :: (IsMDLMaterialProperty mdlMaterialProperty, IsMDLTextureSampler value) => mdlMaterialProperty -> value -> IO ()
setTextureSamplerValue mdlMaterialProperty  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlMaterialProperty (mkSelector "setTextureSamplerValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- color@
color :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Ptr ())
color mdlMaterialProperty  =
  fmap castPtr $ sendMsg mdlMaterialProperty (mkSelector "color") (retPtr retVoid) []

-- | @- setColor:@
setColor :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> Ptr () -> IO ()
setColor mdlMaterialProperty  value =
  sendMsg mdlMaterialProperty (mkSelector "setColor:") retVoid [argPtr value]

-- | @- floatValue@
floatValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO CFloat
floatValue mdlMaterialProperty  =
  sendMsg mdlMaterialProperty (mkSelector "floatValue") retCFloat []

-- | @- setFloatValue:@
setFloatValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> CFloat -> IO ()
setFloatValue mdlMaterialProperty  value =
  sendMsg mdlMaterialProperty (mkSelector "setFloatValue:") retVoid [argCFloat (fromIntegral value)]

-- | @- luminance@
luminance :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO CFloat
luminance mdlMaterialProperty  =
  sendMsg mdlMaterialProperty (mkSelector "luminance") retCFloat []

-- | @- setLuminance:@
setLuminance :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> CFloat -> IO ()
setLuminance mdlMaterialProperty  value =
  sendMsg mdlMaterialProperty (mkSelector "setLuminance:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:semantic:@
initWithName_semanticSelector :: Selector
initWithName_semanticSelector = mkSelector "initWithName:semantic:"

-- | @Selector@ for @initWithName:semantic:float:@
initWithName_semantic_floatSelector :: Selector
initWithName_semantic_floatSelector = mkSelector "initWithName:semantic:float:"

-- | @Selector@ for @initWithName:semantic:URL:@
initWithName_semantic_URLSelector :: Selector
initWithName_semantic_URLSelector = mkSelector "initWithName:semantic:URL:"

-- | @Selector@ for @initWithName:semantic:string:@
initWithName_semantic_stringSelector :: Selector
initWithName_semantic_stringSelector = mkSelector "initWithName:semantic:string:"

-- | @Selector@ for @initWithName:semantic:textureSampler:@
initWithName_semantic_textureSamplerSelector :: Selector
initWithName_semantic_textureSamplerSelector = mkSelector "initWithName:semantic:textureSampler:"

-- | @Selector@ for @initWithName:semantic:color:@
initWithName_semantic_colorSelector :: Selector
initWithName_semantic_colorSelector = mkSelector "initWithName:semantic:color:"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @semantic@
semanticSelector :: Selector
semanticSelector = mkSelector "semantic"

-- | @Selector@ for @setSemantic:@
setSemanticSelector :: Selector
setSemanticSelector = mkSelector "setSemantic:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @URLValue@
urlValueSelector :: Selector
urlValueSelector = mkSelector "URLValue"

-- | @Selector@ for @setURLValue:@
setURLValueSelector :: Selector
setURLValueSelector = mkSelector "setURLValue:"

-- | @Selector@ for @textureSamplerValue@
textureSamplerValueSelector :: Selector
textureSamplerValueSelector = mkSelector "textureSamplerValue"

-- | @Selector@ for @setTextureSamplerValue:@
setTextureSamplerValueSelector :: Selector
setTextureSamplerValueSelector = mkSelector "setTextureSamplerValue:"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector
setFloatValueSelector = mkSelector "setFloatValue:"

-- | @Selector@ for @luminance@
luminanceSelector :: Selector
luminanceSelector = mkSelector "luminance"

-- | @Selector@ for @setLuminance:@
setLuminanceSelector :: Selector
setLuminanceSelector = mkSelector "setLuminance:"

