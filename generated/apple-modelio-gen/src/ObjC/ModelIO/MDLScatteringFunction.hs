{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The base scattering function is Lambertian, with a Blinn-Phong specular response. Specular power for Blinn-Phong can be derived from the roughness property using  an approximation.
--
-- Generated bindings for @MDLScatteringFunction@.
module ObjC.ModelIO.MDLScatteringFunction
  ( MDLScatteringFunction
  , IsMDLScatteringFunction(..)
  , name
  , setName
  , baseColor
  , emission
  , specular
  , materialIndexOfRefraction
  , interfaceIndexOfRefraction
  , normal
  , ambientOcclusion
  , ambientOcclusionScale
  , ambientOcclusionScaleSelector
  , ambientOcclusionSelector
  , baseColorSelector
  , emissionSelector
  , interfaceIndexOfRefractionSelector
  , materialIndexOfRefractionSelector
  , nameSelector
  , normalSelector
  , setNameSelector
  , specularSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | See: MDLNamed
--
-- ObjC selector: @- name@
name :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id NSString)
name mdlScatteringFunction =
  sendMessage mdlScatteringFunction nameSelector

-- | See: MDLNamed
--
-- ObjC selector: @- setName:@
setName :: (IsMDLScatteringFunction mdlScatteringFunction, IsNSString value) => mdlScatteringFunction -> value -> IO ()
setName mdlScatteringFunction value =
  sendMessage mdlScatteringFunction setNameSelector (toNSString value)

-- | @- baseColor@
baseColor :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
baseColor mdlScatteringFunction =
  sendMessage mdlScatteringFunction baseColorSelector

-- | @- emission@
emission :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
emission mdlScatteringFunction =
  sendMessage mdlScatteringFunction emissionSelector

-- | @- specular@
specular :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
specular mdlScatteringFunction =
  sendMessage mdlScatteringFunction specularSelector

-- | @- materialIndexOfRefraction@
materialIndexOfRefraction :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
materialIndexOfRefraction mdlScatteringFunction =
  sendMessage mdlScatteringFunction materialIndexOfRefractionSelector

-- | @- interfaceIndexOfRefraction@
interfaceIndexOfRefraction :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
interfaceIndexOfRefraction mdlScatteringFunction =
  sendMessage mdlScatteringFunction interfaceIndexOfRefractionSelector

-- | @- normal@
normal :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
normal mdlScatteringFunction =
  sendMessage mdlScatteringFunction normalSelector

-- | @- ambientOcclusion@
ambientOcclusion :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
ambientOcclusion mdlScatteringFunction =
  sendMessage mdlScatteringFunction ambientOcclusionSelector

-- | @- ambientOcclusionScale@
ambientOcclusionScale :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
ambientOcclusionScale mdlScatteringFunction =
  sendMessage mdlScatteringFunction ambientOcclusionScaleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @baseColor@
baseColorSelector :: Selector '[] (Id MDLMaterialProperty)
baseColorSelector = mkSelector "baseColor"

-- | @Selector@ for @emission@
emissionSelector :: Selector '[] (Id MDLMaterialProperty)
emissionSelector = mkSelector "emission"

-- | @Selector@ for @specular@
specularSelector :: Selector '[] (Id MDLMaterialProperty)
specularSelector = mkSelector "specular"

-- | @Selector@ for @materialIndexOfRefraction@
materialIndexOfRefractionSelector :: Selector '[] (Id MDLMaterialProperty)
materialIndexOfRefractionSelector = mkSelector "materialIndexOfRefraction"

-- | @Selector@ for @interfaceIndexOfRefraction@
interfaceIndexOfRefractionSelector :: Selector '[] (Id MDLMaterialProperty)
interfaceIndexOfRefractionSelector = mkSelector "interfaceIndexOfRefraction"

-- | @Selector@ for @normal@
normalSelector :: Selector '[] (Id MDLMaterialProperty)
normalSelector = mkSelector "normal"

-- | @Selector@ for @ambientOcclusion@
ambientOcclusionSelector :: Selector '[] (Id MDLMaterialProperty)
ambientOcclusionSelector = mkSelector "ambientOcclusion"

-- | @Selector@ for @ambientOcclusionScale@
ambientOcclusionScaleSelector :: Selector '[] (Id MDLMaterialProperty)
ambientOcclusionScaleSelector = mkSelector "ambientOcclusionScale"

