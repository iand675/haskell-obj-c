{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , colorSelector
  , floatValueSelector
  , initSelector
  , initWithName_semanticSelector
  , initWithName_semantic_URLSelector
  , initWithName_semantic_colorSelector
  , initWithName_semantic_floatSelector
  , initWithName_semantic_stringSelector
  , initWithName_semantic_textureSamplerSelector
  , luminanceSelector
  , nameSelector
  , semanticSelector
  , setColorSelector
  , setFloatValueSelector
  , setLuminanceSelector
  , setNameSelector
  , setPropertiesSelector
  , setSemanticSelector
  , setStringValueSelector
  , setTextureSamplerValueSelector
  , setTypeSelector
  , setURLValueSelector
  , stringValueSelector
  , textureSamplerValueSelector
  , typeSelector
  , urlValueSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id MDLMaterialProperty)
init_ mdlMaterialProperty =
  sendOwnedMessage mdlMaterialProperty initSelector

-- | @- initWithName:semantic:@
initWithName_semantic :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> IO (Id MDLMaterialProperty)
initWithName_semantic mdlMaterialProperty name semantic =
  sendOwnedMessage mdlMaterialProperty initWithName_semanticSelector (toNSString name) semantic

-- | @- initWithName:semantic:float:@
initWithName_semantic_float :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> CFloat -> IO (Id MDLMaterialProperty)
initWithName_semantic_float mdlMaterialProperty name semantic value =
  sendOwnedMessage mdlMaterialProperty initWithName_semantic_floatSelector (toNSString name) semantic value

-- | @- initWithName:semantic:URL:@
initWithName_semantic_URL :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name, IsNSURL url) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> url -> IO (Id MDLMaterialProperty)
initWithName_semantic_URL mdlMaterialProperty name semantic url =
  sendOwnedMessage mdlMaterialProperty initWithName_semantic_URLSelector (toNSString name) semantic (toNSURL url)

-- | @- initWithName:semantic:string:@
initWithName_semantic_string :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name, IsNSString string) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> string -> IO (Id MDLMaterialProperty)
initWithName_semantic_string mdlMaterialProperty name semantic string =
  sendOwnedMessage mdlMaterialProperty initWithName_semantic_stringSelector (toNSString name) semantic (toNSString string)

-- | @- initWithName:semantic:textureSampler:@
initWithName_semantic_textureSampler :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name, IsMDLTextureSampler textureSampler) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> textureSampler -> IO (Id MDLMaterialProperty)
initWithName_semantic_textureSampler mdlMaterialProperty name semantic textureSampler =
  sendOwnedMessage mdlMaterialProperty initWithName_semantic_textureSamplerSelector (toNSString name) semantic (toMDLTextureSampler textureSampler)

-- | @- initWithName:semantic:color:@
initWithName_semantic_color :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString name) => mdlMaterialProperty -> name -> MDLMaterialSemantic -> Ptr () -> IO (Id MDLMaterialProperty)
initWithName_semantic_color mdlMaterialProperty name semantic color =
  sendOwnedMessage mdlMaterialProperty initWithName_semantic_colorSelector (toNSString name) semantic color

-- | @- setProperties:@
setProperties :: (IsMDLMaterialProperty mdlMaterialProperty, IsMDLMaterialProperty property) => mdlMaterialProperty -> property -> IO ()
setProperties mdlMaterialProperty property =
  sendMessage mdlMaterialProperty setPropertiesSelector (toMDLMaterialProperty property)

-- | @- semantic@
semantic :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO MDLMaterialSemantic
semantic mdlMaterialProperty =
  sendMessage mdlMaterialProperty semanticSelector

-- | @- setSemantic:@
setSemantic :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> MDLMaterialSemantic -> IO ()
setSemantic mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setSemanticSelector value

-- | @- type@
type_ :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO MDLMaterialPropertyType
type_ mdlMaterialProperty =
  sendMessage mdlMaterialProperty typeSelector

-- | @- setType:@
setType :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> MDLMaterialPropertyType -> IO ()
setType mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setTypeSelector value

-- | See: MDLNamed
--
-- ObjC selector: @- name@
name :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id NSString)
name mdlMaterialProperty =
  sendMessage mdlMaterialProperty nameSelector

-- | See: MDLNamed
--
-- ObjC selector: @- setName:@
setName :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString value) => mdlMaterialProperty -> value -> IO ()
setName mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setNameSelector (toNSString value)

-- | @- stringValue@
stringValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id NSString)
stringValue mdlMaterialProperty =
  sendMessage mdlMaterialProperty stringValueSelector

-- | @- setStringValue:@
setStringValue :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSString value) => mdlMaterialProperty -> value -> IO ()
setStringValue mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setStringValueSelector (toNSString value)

-- | @- URLValue@
urlValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id NSURL)
urlValue mdlMaterialProperty =
  sendMessage mdlMaterialProperty urlValueSelector

-- | @- setURLValue:@
setURLValue :: (IsMDLMaterialProperty mdlMaterialProperty, IsNSURL value) => mdlMaterialProperty -> value -> IO ()
setURLValue mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setURLValueSelector (toNSURL value)

-- | @- textureSamplerValue@
textureSamplerValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Id MDLTextureSampler)
textureSamplerValue mdlMaterialProperty =
  sendMessage mdlMaterialProperty textureSamplerValueSelector

-- | @- setTextureSamplerValue:@
setTextureSamplerValue :: (IsMDLMaterialProperty mdlMaterialProperty, IsMDLTextureSampler value) => mdlMaterialProperty -> value -> IO ()
setTextureSamplerValue mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setTextureSamplerValueSelector (toMDLTextureSampler value)

-- | @- color@
color :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO (Ptr ())
color mdlMaterialProperty =
  sendMessage mdlMaterialProperty colorSelector

-- | @- setColor:@
setColor :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> Ptr () -> IO ()
setColor mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setColorSelector value

-- | @- floatValue@
floatValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO CFloat
floatValue mdlMaterialProperty =
  sendMessage mdlMaterialProperty floatValueSelector

-- | @- setFloatValue:@
setFloatValue :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> CFloat -> IO ()
setFloatValue mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setFloatValueSelector value

-- | @- luminance@
luminance :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> IO CFloat
luminance mdlMaterialProperty =
  sendMessage mdlMaterialProperty luminanceSelector

-- | @- setLuminance:@
setLuminance :: IsMDLMaterialProperty mdlMaterialProperty => mdlMaterialProperty -> CFloat -> IO ()
setLuminance mdlMaterialProperty value =
  sendMessage mdlMaterialProperty setLuminanceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MDLMaterialProperty)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:semantic:@
initWithName_semanticSelector :: Selector '[Id NSString, MDLMaterialSemantic] (Id MDLMaterialProperty)
initWithName_semanticSelector = mkSelector "initWithName:semantic:"

-- | @Selector@ for @initWithName:semantic:float:@
initWithName_semantic_floatSelector :: Selector '[Id NSString, MDLMaterialSemantic, CFloat] (Id MDLMaterialProperty)
initWithName_semantic_floatSelector = mkSelector "initWithName:semantic:float:"

-- | @Selector@ for @initWithName:semantic:URL:@
initWithName_semantic_URLSelector :: Selector '[Id NSString, MDLMaterialSemantic, Id NSURL] (Id MDLMaterialProperty)
initWithName_semantic_URLSelector = mkSelector "initWithName:semantic:URL:"

-- | @Selector@ for @initWithName:semantic:string:@
initWithName_semantic_stringSelector :: Selector '[Id NSString, MDLMaterialSemantic, Id NSString] (Id MDLMaterialProperty)
initWithName_semantic_stringSelector = mkSelector "initWithName:semantic:string:"

-- | @Selector@ for @initWithName:semantic:textureSampler:@
initWithName_semantic_textureSamplerSelector :: Selector '[Id NSString, MDLMaterialSemantic, Id MDLTextureSampler] (Id MDLMaterialProperty)
initWithName_semantic_textureSamplerSelector = mkSelector "initWithName:semantic:textureSampler:"

-- | @Selector@ for @initWithName:semantic:color:@
initWithName_semantic_colorSelector :: Selector '[Id NSString, MDLMaterialSemantic, Ptr ()] (Id MDLMaterialProperty)
initWithName_semantic_colorSelector = mkSelector "initWithName:semantic:color:"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector '[Id MDLMaterialProperty] ()
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @semantic@
semanticSelector :: Selector '[] MDLMaterialSemantic
semanticSelector = mkSelector "semantic"

-- | @Selector@ for @setSemantic:@
setSemanticSelector :: Selector '[MDLMaterialSemantic] ()
setSemanticSelector = mkSelector "setSemantic:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MDLMaterialPropertyType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[MDLMaterialPropertyType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector '[Id NSString] ()
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @URLValue@
urlValueSelector :: Selector '[] (Id NSURL)
urlValueSelector = mkSelector "URLValue"

-- | @Selector@ for @setURLValue:@
setURLValueSelector :: Selector '[Id NSURL] ()
setURLValueSelector = mkSelector "setURLValue:"

-- | @Selector@ for @textureSamplerValue@
textureSamplerValueSelector :: Selector '[] (Id MDLTextureSampler)
textureSamplerValueSelector = mkSelector "textureSamplerValue"

-- | @Selector@ for @setTextureSamplerValue:@
setTextureSamplerValueSelector :: Selector '[Id MDLTextureSampler] ()
setTextureSamplerValueSelector = mkSelector "setTextureSamplerValue:"

-- | @Selector@ for @color@
colorSelector :: Selector '[] (Ptr ())
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[Ptr ()] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @floatValue@
floatValueSelector :: Selector '[] CFloat
floatValueSelector = mkSelector "floatValue"

-- | @Selector@ for @setFloatValue:@
setFloatValueSelector :: Selector '[CFloat] ()
setFloatValueSelector = mkSelector "setFloatValue:"

-- | @Selector@ for @luminance@
luminanceSelector :: Selector '[] CFloat
luminanceSelector = mkSelector "luminance"

-- | @Selector@ for @setLuminance:@
setLuminanceSelector :: Selector '[CFloat] ()
setLuminanceSelector = mkSelector "setLuminance:"

