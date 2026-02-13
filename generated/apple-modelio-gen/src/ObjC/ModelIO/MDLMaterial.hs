{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , baseMaterialSelector
  , countSelector
  , initWithName_scatteringFunctionSelector
  , loadTexturesUsingResolverSelector
  , materialFaceSelector
  , nameSelector
  , objectAtIndexedSubscriptSelector
  , objectForKeyedSubscriptSelector
  , propertiesWithSemanticSelector
  , propertyNamedSelector
  , propertyWithSemanticSelector
  , removeAllPropertiesSelector
  , removePropertySelector
  , resolveTexturesWithResolverSelector
  , scatteringFunctionSelector
  , setBaseMaterialSelector
  , setMaterialFaceSelector
  , setNameSelector
  , setPropertySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:scatteringFunction:@
initWithName_scatteringFunction :: (IsMDLMaterial mdlMaterial, IsNSString name, IsMDLScatteringFunction scatteringFunction) => mdlMaterial -> name -> scatteringFunction -> IO (Id MDLMaterial)
initWithName_scatteringFunction mdlMaterial name scatteringFunction =
  sendOwnedMessage mdlMaterial initWithName_scatteringFunctionSelector (toNSString name) (toMDLScatteringFunction scatteringFunction)

-- | @- setProperty:@
setProperty :: (IsMDLMaterial mdlMaterial, IsMDLMaterialProperty property) => mdlMaterial -> property -> IO ()
setProperty mdlMaterial property =
  sendMessage mdlMaterial setPropertySelector (toMDLMaterialProperty property)

-- | @- removeProperty:@
removeProperty :: (IsMDLMaterial mdlMaterial, IsMDLMaterialProperty property) => mdlMaterial -> property -> IO ()
removeProperty mdlMaterial property =
  sendMessage mdlMaterial removePropertySelector (toMDLMaterialProperty property)

-- | @- propertyNamed:@
propertyNamed :: (IsMDLMaterial mdlMaterial, IsNSString name) => mdlMaterial -> name -> IO (Id MDLMaterialProperty)
propertyNamed mdlMaterial name =
  sendMessage mdlMaterial propertyNamedSelector (toNSString name)

-- | @- propertyWithSemantic:@
propertyWithSemantic :: IsMDLMaterial mdlMaterial => mdlMaterial -> MDLMaterialSemantic -> IO (Id MDLMaterialProperty)
propertyWithSemantic mdlMaterial semantic =
  sendMessage mdlMaterial propertyWithSemanticSelector semantic

-- | @- propertiesWithSemantic:@
propertiesWithSemantic :: IsMDLMaterial mdlMaterial => mdlMaterial -> MDLMaterialSemantic -> IO (Id NSArray)
propertiesWithSemantic mdlMaterial semantic =
  sendMessage mdlMaterial propertiesWithSemanticSelector semantic

-- | @- removeAllProperties@
removeAllProperties :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO ()
removeAllProperties mdlMaterial =
  sendMessage mdlMaterial removeAllPropertiesSelector

-- | @- resolveTexturesWithResolver:@
resolveTexturesWithResolver :: IsMDLMaterial mdlMaterial => mdlMaterial -> RawId -> IO ()
resolveTexturesWithResolver mdlMaterial resolver =
  sendMessage mdlMaterial resolveTexturesWithResolverSelector resolver

-- | @- loadTexturesUsingResolver:@
loadTexturesUsingResolver :: IsMDLMaterial mdlMaterial => mdlMaterial -> RawId -> IO ()
loadTexturesUsingResolver mdlMaterial resolver =
  sendMessage mdlMaterial loadTexturesUsingResolverSelector resolver

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMDLMaterial mdlMaterial => mdlMaterial -> CULong -> IO (Id MDLMaterialProperty)
objectAtIndexedSubscript mdlMaterial idx =
  sendMessage mdlMaterial objectAtIndexedSubscriptSelector idx

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsMDLMaterial mdlMaterial, IsNSString name) => mdlMaterial -> name -> IO (Id MDLMaterialProperty)
objectForKeyedSubscript mdlMaterial name =
  sendMessage mdlMaterial objectForKeyedSubscriptSelector (toNSString name)

-- | @- scatteringFunction@
scatteringFunction :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO (Id MDLScatteringFunction)
scatteringFunction mdlMaterial =
  sendMessage mdlMaterial scatteringFunctionSelector

-- | See: MDLNamed
--
-- ObjC selector: @- name@
name :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO (Id NSString)
name mdlMaterial =
  sendMessage mdlMaterial nameSelector

-- | See: MDLNamed
--
-- ObjC selector: @- setName:@
setName :: (IsMDLMaterial mdlMaterial, IsNSString value) => mdlMaterial -> value -> IO ()
setName mdlMaterial value =
  sendMessage mdlMaterial setNameSelector (toNSString value)

-- | @- baseMaterial@
baseMaterial :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO (Id MDLMaterial)
baseMaterial mdlMaterial =
  sendMessage mdlMaterial baseMaterialSelector

-- | @- setBaseMaterial:@
setBaseMaterial :: (IsMDLMaterial mdlMaterial, IsMDLMaterial value) => mdlMaterial -> value -> IO ()
setBaseMaterial mdlMaterial value =
  sendMessage mdlMaterial setBaseMaterialSelector (toMDLMaterial value)

-- | @- count@
count :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO CULong
count mdlMaterial =
  sendMessage mdlMaterial countSelector

-- | @- materialFace@
materialFace :: IsMDLMaterial mdlMaterial => mdlMaterial -> IO MDLMaterialFace
materialFace mdlMaterial =
  sendMessage mdlMaterial materialFaceSelector

-- | @- setMaterialFace:@
setMaterialFace :: IsMDLMaterial mdlMaterial => mdlMaterial -> MDLMaterialFace -> IO ()
setMaterialFace mdlMaterial value =
  sendMessage mdlMaterial setMaterialFaceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:scatteringFunction:@
initWithName_scatteringFunctionSelector :: Selector '[Id NSString, Id MDLScatteringFunction] (Id MDLMaterial)
initWithName_scatteringFunctionSelector = mkSelector "initWithName:scatteringFunction:"

-- | @Selector@ for @setProperty:@
setPropertySelector :: Selector '[Id MDLMaterialProperty] ()
setPropertySelector = mkSelector "setProperty:"

-- | @Selector@ for @removeProperty:@
removePropertySelector :: Selector '[Id MDLMaterialProperty] ()
removePropertySelector = mkSelector "removeProperty:"

-- | @Selector@ for @propertyNamed:@
propertyNamedSelector :: Selector '[Id NSString] (Id MDLMaterialProperty)
propertyNamedSelector = mkSelector "propertyNamed:"

-- | @Selector@ for @propertyWithSemantic:@
propertyWithSemanticSelector :: Selector '[MDLMaterialSemantic] (Id MDLMaterialProperty)
propertyWithSemanticSelector = mkSelector "propertyWithSemantic:"

-- | @Selector@ for @propertiesWithSemantic:@
propertiesWithSemanticSelector :: Selector '[MDLMaterialSemantic] (Id NSArray)
propertiesWithSemanticSelector = mkSelector "propertiesWithSemantic:"

-- | @Selector@ for @removeAllProperties@
removeAllPropertiesSelector :: Selector '[] ()
removeAllPropertiesSelector = mkSelector "removeAllProperties"

-- | @Selector@ for @resolveTexturesWithResolver:@
resolveTexturesWithResolverSelector :: Selector '[RawId] ()
resolveTexturesWithResolverSelector = mkSelector "resolveTexturesWithResolver:"

-- | @Selector@ for @loadTexturesUsingResolver:@
loadTexturesUsingResolverSelector :: Selector '[RawId] ()
loadTexturesUsingResolverSelector = mkSelector "loadTexturesUsingResolver:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id MDLMaterialProperty)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id NSString] (Id MDLMaterialProperty)
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @scatteringFunction@
scatteringFunctionSelector :: Selector '[] (Id MDLScatteringFunction)
scatteringFunctionSelector = mkSelector "scatteringFunction"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @baseMaterial@
baseMaterialSelector :: Selector '[] (Id MDLMaterial)
baseMaterialSelector = mkSelector "baseMaterial"

-- | @Selector@ for @setBaseMaterial:@
setBaseMaterialSelector :: Selector '[Id MDLMaterial] ()
setBaseMaterialSelector = mkSelector "setBaseMaterial:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @materialFace@
materialFaceSelector :: Selector '[] MDLMaterialFace
materialFaceSelector = mkSelector "materialFace"

-- | @Selector@ for @setMaterialFace:@
setMaterialFaceSelector :: Selector '[MDLMaterialFace] ()
setMaterialFaceSelector = mkSelector "setMaterialFace:"

