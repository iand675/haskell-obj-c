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
  , nameSelector
  , setNameSelector
  , baseColorSelector
  , emissionSelector
  , specularSelector
  , materialIndexOfRefractionSelector
  , interfaceIndexOfRefractionSelector
  , normalSelector
  , ambientOcclusionSelector
  , ambientOcclusionScaleSelector


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
import ObjC.Foundation.Internal.Classes

-- | See: MDLNamed
--
-- ObjC selector: @- name@
name :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id NSString)
name mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | See: MDLNamed
--
-- ObjC selector: @- setName:@
setName :: (IsMDLScatteringFunction mdlScatteringFunction, IsNSString value) => mdlScatteringFunction -> value -> IO ()
setName mdlScatteringFunction  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlScatteringFunction (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- baseColor@
baseColor :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
baseColor mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "baseColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- emission@
emission :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
emission mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "emission") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- specular@
specular :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
specular mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "specular") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- materialIndexOfRefraction@
materialIndexOfRefraction :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
materialIndexOfRefraction mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "materialIndexOfRefraction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- interfaceIndexOfRefraction@
interfaceIndexOfRefraction :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
interfaceIndexOfRefraction mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "interfaceIndexOfRefraction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- normal@
normal :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
normal mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "normal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ambientOcclusion@
ambientOcclusion :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
ambientOcclusion mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "ambientOcclusion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ambientOcclusionScale@
ambientOcclusionScale :: IsMDLScatteringFunction mdlScatteringFunction => mdlScatteringFunction -> IO (Id MDLMaterialProperty)
ambientOcclusionScale mdlScatteringFunction  =
  sendMsg mdlScatteringFunction (mkSelector "ambientOcclusionScale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @baseColor@
baseColorSelector :: Selector
baseColorSelector = mkSelector "baseColor"

-- | @Selector@ for @emission@
emissionSelector :: Selector
emissionSelector = mkSelector "emission"

-- | @Selector@ for @specular@
specularSelector :: Selector
specularSelector = mkSelector "specular"

-- | @Selector@ for @materialIndexOfRefraction@
materialIndexOfRefractionSelector :: Selector
materialIndexOfRefractionSelector = mkSelector "materialIndexOfRefraction"

-- | @Selector@ for @interfaceIndexOfRefraction@
interfaceIndexOfRefractionSelector :: Selector
interfaceIndexOfRefractionSelector = mkSelector "interfaceIndexOfRefraction"

-- | @Selector@ for @normal@
normalSelector :: Selector
normalSelector = mkSelector "normal"

-- | @Selector@ for @ambientOcclusion@
ambientOcclusionSelector :: Selector
ambientOcclusionSelector = mkSelector "ambientOcclusion"

-- | @Selector@ for @ambientOcclusionScale@
ambientOcclusionScaleSelector :: Selector
ambientOcclusionScaleSelector = mkSelector "ambientOcclusionScale"

