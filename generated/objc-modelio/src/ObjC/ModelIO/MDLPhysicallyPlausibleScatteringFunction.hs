{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLPhysicallyPlausibleScatteringFunction@.
module ObjC.ModelIO.MDLPhysicallyPlausibleScatteringFunction
  ( MDLPhysicallyPlausibleScatteringFunction
  , IsMDLPhysicallyPlausibleScatteringFunction(..)
  , version
  , subsurface
  , metallic
  , specularAmount
  , specularTint
  , roughness
  , anisotropic
  , anisotropicRotation
  , sheen
  , sheenTint
  , clearcoat
  , clearcoatGloss
  , versionSelector
  , subsurfaceSelector
  , metallicSelector
  , specularAmountSelector
  , specularTintSelector
  , roughnessSelector
  , anisotropicSelector
  , anisotropicRotationSelector
  , sheenSelector
  , sheenTintSelector
  , clearcoatSelector
  , clearcoatGlossSelector


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

-- | @- version@
version :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO CLong
version mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "version") retCLong []

-- | @- subsurface@
subsurface :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
subsurface mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "subsurface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- metallic@
metallic :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
metallic mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "metallic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- specularAmount@
specularAmount :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
specularAmount mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "specularAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- specularTint@
specularTint :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
specularTint mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "specularTint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- roughness@
roughness :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
roughness mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "roughness") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- anisotropic@
anisotropic :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
anisotropic mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "anisotropic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- anisotropicRotation@
anisotropicRotation :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
anisotropicRotation mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "anisotropicRotation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sheen@
sheen :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
sheen mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "sheen") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sheenTint@
sheenTint :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
sheenTint mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "sheenTint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- clearcoat@
clearcoat :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
clearcoat mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "clearcoat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- clearcoatGloss@
clearcoatGloss :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
clearcoatGloss mdlPhysicallyPlausibleScatteringFunction  =
  sendMsg mdlPhysicallyPlausibleScatteringFunction (mkSelector "clearcoatGloss") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @subsurface@
subsurfaceSelector :: Selector
subsurfaceSelector = mkSelector "subsurface"

-- | @Selector@ for @metallic@
metallicSelector :: Selector
metallicSelector = mkSelector "metallic"

-- | @Selector@ for @specularAmount@
specularAmountSelector :: Selector
specularAmountSelector = mkSelector "specularAmount"

-- | @Selector@ for @specularTint@
specularTintSelector :: Selector
specularTintSelector = mkSelector "specularTint"

-- | @Selector@ for @roughness@
roughnessSelector :: Selector
roughnessSelector = mkSelector "roughness"

-- | @Selector@ for @anisotropic@
anisotropicSelector :: Selector
anisotropicSelector = mkSelector "anisotropic"

-- | @Selector@ for @anisotropicRotation@
anisotropicRotationSelector :: Selector
anisotropicRotationSelector = mkSelector "anisotropicRotation"

-- | @Selector@ for @sheen@
sheenSelector :: Selector
sheenSelector = mkSelector "sheen"

-- | @Selector@ for @sheenTint@
sheenTintSelector :: Selector
sheenTintSelector = mkSelector "sheenTint"

-- | @Selector@ for @clearcoat@
clearcoatSelector :: Selector
clearcoatSelector = mkSelector "clearcoat"

-- | @Selector@ for @clearcoatGloss@
clearcoatGlossSelector :: Selector
clearcoatGlossSelector = mkSelector "clearcoatGloss"

