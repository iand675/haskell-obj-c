{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLMaterial@.
module ObjC.ModelIO.MDLMaterial
  ( MDLMaterial
  , IsMDLMaterial(..)
  , initWithName_scatteringFunction
  , setProperty
  , removeProperty
  , propertyNamed
  , propertyWithSemantic
  , propertiesWithSemantic
  , removeAllProperties
  , resolveTexturesWithResolver
  , loadTexturesUsingResolver
  , objectAtIndexedSubscript
  , objectForKeyedSubscript
  , scatteringFunction
  , name
  , setName
  , baseMaterial
  , setBaseMaterial
  , count
  , materialFace
  , setMaterialFace
  , initWithName_scatteringFunctionSelector
  , setPropertySelector
  , removePropertySelector
  , propertyNamedSelector
  , propertyWithSemanticSelector
  , propertiesWithSemanticSelector
  , removeAllPropertiesSelector
  , resolveTexturesWithResolverSelector
  , loadTexturesUsingResolverSelector
  , objectAtIndexedSubscriptSelector
  , objectForKeyedSubscriptSelector
  , scatteringFunctionSelector
  , nameSelector
  , setNameSelector
  , baseMaterialSelector
  , setBaseMaterialSelector
  , countSelector
  , materialFaceSelector
  , setMaterialFaceSelector

  -- * Enum types
  , MDLMaterialFace(MDLMaterialFace)
  , pattern MDLMaterialFaceFront
  , pattern MDLMaterialFaceBack
  , pattern MDLMaterialFaceDoubleSided
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

-- | @- initWithName:scatteringFunction:@
initWithName_scatteringFunction :: (IsMDLMaterial mdlMaterial, IsNSString name, IsMDLScatteringFunction scatteringFunction) => mdlMaterial -> name -> scatteringFunction -> IO (Id MDLMaterial)
initWithName_scatteringFunction mdlMaterial  name scatteringFunction =
withObjCPtr name $ \raw_name ->
  withObjCPtr scatteringFunction $ \raw_scatteringFunction ->
      sendMsg mdlMaterial (mkSelector "initWithName:scatteringFunction:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_scatteringFunction :: Ptr ())] >>= ownedObject . castPtr

-- | @- setProperty:@
setProperty :: (IsMDLMaterial mdlMaterial, IsMDLMaterialProperty property) => mdlMaterial -> property -> IO ()
setProperty mdlMaterial  property =
withObjCPtr property $ \raw_property ->
    sendMsg mdlMaterial (mkSelector "setProperty:") retVoid [argPtr (castPtr raw_property :: Ptr ())]

-- | @- removeProperty:@
removeProperty :: (IsMDLMaterial mdlMaterial, IsMDLMaterialProperty property) => mdlMaterial -> property -> IO ()
removeProperty mdlMaterial  property =
withObjCPtr property $ \raw_property ->
    sendMsg mdlMaterial (mkSelector "removeProperty:") retVoid [argPtr (castPtr raw_property :: Ptr ())]

-- | @- propertyNamed:@
propertyNamed :: (IsMDLMaterial mdlMaterial, IsNSString name) => mdlMaterial -> name -> IO (Id MDLMaterialProperty)
propertyNamed mdlMaterial  name =
withObjCPtr name $ \raw_name ->
    sendMsg mdlMaterial (mkSelector "propertyNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- propertyWithSemantic:@
propertyWithSemantic :: IsMDLMaterial mdlMaterial => mdlMaterial -> MDLMaterialSemantic -> IO (Id MDLMaterialProperty)
propertyWithSemantic mdlMaterial  semantic =
  sendMsg mdlMaterial (mkSelector "propertyWithSemantic:") (retPtr retVoid) [argCULong (coerce semantic)] >>= retainedObject . castPtr

-- | @- propertiesWithSemantic:@
propertiesWithSemantic :: IsMDLMaterial mdlMaterial => mdlMaterial -> MDLMaterialSemantic -> IO (Id NSArray)
propertiesWithSemantic mdlMaterial  semantic =
  sendMsg mdlMaterial (mkSelector "propertiesWithSemantic:") (retPtr retVoid) [argCULong (coerce semantic)] >>= retainedObject . castPtr

-- | @- removeAllProperties@
removeAllProperties :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO ()
removeAllProperties mdlMaterial  =
  sendMsg mdlMaterial (mkSelector "removeAllProperties") retVoid []

-- | @- resolveTexturesWithResolver:@
resolveTexturesWithResolver :: IsMDLMaterial mdlMaterial => mdlMaterial -> RawId -> IO ()
resolveTexturesWithResolver mdlMaterial  resolver =
  sendMsg mdlMaterial (mkSelector "resolveTexturesWithResolver:") retVoid [argPtr (castPtr (unRawId resolver) :: Ptr ())]

-- | @- loadTexturesUsingResolver:@
loadTexturesUsingResolver :: IsMDLMaterial mdlMaterial => mdlMaterial -> RawId -> IO ()
loadTexturesUsingResolver mdlMaterial  resolver =
  sendMsg mdlMaterial (mkSelector "loadTexturesUsingResolver:") retVoid [argPtr (castPtr (unRawId resolver) :: Ptr ())]

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMDLMaterial mdlMaterial => mdlMaterial -> CULong -> IO (Id MDLMaterialProperty)
objectAtIndexedSubscript mdlMaterial  idx =
  sendMsg mdlMaterial (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral idx)] >>= retainedObject . castPtr

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsMDLMaterial mdlMaterial, IsNSString name) => mdlMaterial -> name -> IO (Id MDLMaterialProperty)
objectForKeyedSubscript mdlMaterial  name =
withObjCPtr name $ \raw_name ->
    sendMsg mdlMaterial (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- scatteringFunction@
scatteringFunction :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO (Id MDLScatteringFunction)
scatteringFunction mdlMaterial  =
  sendMsg mdlMaterial (mkSelector "scatteringFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | See: MDLNamed
--
-- ObjC selector: @- name@
name :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO (Id NSString)
name mdlMaterial  =
  sendMsg mdlMaterial (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | See: MDLNamed
--
-- ObjC selector: @- setName:@
setName :: (IsMDLMaterial mdlMaterial, IsNSString value) => mdlMaterial -> value -> IO ()
setName mdlMaterial  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlMaterial (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- baseMaterial@
baseMaterial :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO (Id MDLMaterial)
baseMaterial mdlMaterial  =
  sendMsg mdlMaterial (mkSelector "baseMaterial") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBaseMaterial:@
setBaseMaterial :: (IsMDLMaterial mdlMaterial, IsMDLMaterial value) => mdlMaterial -> value -> IO ()
setBaseMaterial mdlMaterial  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlMaterial (mkSelector "setBaseMaterial:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- count@
count :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO CULong
count mdlMaterial  =
  sendMsg mdlMaterial (mkSelector "count") retCULong []

-- | @- materialFace@
materialFace :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO MDLMaterialFace
materialFace mdlMaterial  =
  fmap (coerce :: CULong -> MDLMaterialFace) $ sendMsg mdlMaterial (mkSelector "materialFace") retCULong []

-- | @- setMaterialFace:@
setMaterialFace :: IsMDLMaterial mdlMaterial => mdlMaterial -> MDLMaterialFace -> IO ()
setMaterialFace mdlMaterial  value =
  sendMsg mdlMaterial (mkSelector "setMaterialFace:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:scatteringFunction:@
initWithName_scatteringFunctionSelector :: Selector
initWithName_scatteringFunctionSelector = mkSelector "initWithName:scatteringFunction:"

-- | @Selector@ for @setProperty:@
setPropertySelector :: Selector
setPropertySelector = mkSelector "setProperty:"

-- | @Selector@ for @removeProperty:@
removePropertySelector :: Selector
removePropertySelector = mkSelector "removeProperty:"

-- | @Selector@ for @propertyNamed:@
propertyNamedSelector :: Selector
propertyNamedSelector = mkSelector "propertyNamed:"

-- | @Selector@ for @propertyWithSemantic:@
propertyWithSemanticSelector :: Selector
propertyWithSemanticSelector = mkSelector "propertyWithSemantic:"

-- | @Selector@ for @propertiesWithSemantic:@
propertiesWithSemanticSelector :: Selector
propertiesWithSemanticSelector = mkSelector "propertiesWithSemantic:"

-- | @Selector@ for @removeAllProperties@
removeAllPropertiesSelector :: Selector
removeAllPropertiesSelector = mkSelector "removeAllProperties"

-- | @Selector@ for @resolveTexturesWithResolver:@
resolveTexturesWithResolverSelector :: Selector
resolveTexturesWithResolverSelector = mkSelector "resolveTexturesWithResolver:"

-- | @Selector@ for @loadTexturesUsingResolver:@
loadTexturesUsingResolverSelector :: Selector
loadTexturesUsingResolverSelector = mkSelector "loadTexturesUsingResolver:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @scatteringFunction@
scatteringFunctionSelector :: Selector
scatteringFunctionSelector = mkSelector "scatteringFunction"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @baseMaterial@
baseMaterialSelector :: Selector
baseMaterialSelector = mkSelector "baseMaterial"

-- | @Selector@ for @setBaseMaterial:@
setBaseMaterialSelector :: Selector
setBaseMaterialSelector = mkSelector "setBaseMaterial:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @materialFace@
materialFaceSelector :: Selector
materialFaceSelector = mkSelector "materialFace"

-- | @Selector@ for @setMaterialFace:@
setMaterialFaceSelector :: Selector
setMaterialFaceSelector = mkSelector "setMaterialFace:"

