{-# LANGUAGE DataKinds #-}
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
  , anisotropicRotationSelector
  , anisotropicSelector
  , clearcoatGlossSelector
  , clearcoatSelector
  , metallicSelector
  , roughnessSelector
  , sheenSelector
  , sheenTintSelector
  , specularAmountSelector
  , specularTintSelector
  , subsurfaceSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- version@
version :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO CLong
version mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction versionSelector

-- | @- subsurface@
subsurface :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
subsurface mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction subsurfaceSelector

-- | @- metallic@
metallic :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
metallic mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction metallicSelector

-- | @- specularAmount@
specularAmount :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
specularAmount mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction specularAmountSelector

-- | @- specularTint@
specularTint :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
specularTint mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction specularTintSelector

-- | @- roughness@
roughness :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
roughness mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction roughnessSelector

-- | @- anisotropic@
anisotropic :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
anisotropic mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction anisotropicSelector

-- | @- anisotropicRotation@
anisotropicRotation :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
anisotropicRotation mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction anisotropicRotationSelector

-- | @- sheen@
sheen :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
sheen mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction sheenSelector

-- | @- sheenTint@
sheenTint :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
sheenTint mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction sheenTintSelector

-- | @- clearcoat@
clearcoat :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
clearcoat mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction clearcoatSelector

-- | @- clearcoatGloss@
clearcoatGloss :: IsMDLPhysicallyPlausibleScatteringFunction mdlPhysicallyPlausibleScatteringFunction => mdlPhysicallyPlausibleScatteringFunction -> IO (Id MDLMaterialProperty)
clearcoatGloss mdlPhysicallyPlausibleScatteringFunction =
  sendMessage mdlPhysicallyPlausibleScatteringFunction clearcoatGlossSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @version@
versionSelector :: Selector '[] CLong
versionSelector = mkSelector "version"

-- | @Selector@ for @subsurface@
subsurfaceSelector :: Selector '[] (Id MDLMaterialProperty)
subsurfaceSelector = mkSelector "subsurface"

-- | @Selector@ for @metallic@
metallicSelector :: Selector '[] (Id MDLMaterialProperty)
metallicSelector = mkSelector "metallic"

-- | @Selector@ for @specularAmount@
specularAmountSelector :: Selector '[] (Id MDLMaterialProperty)
specularAmountSelector = mkSelector "specularAmount"

-- | @Selector@ for @specularTint@
specularTintSelector :: Selector '[] (Id MDLMaterialProperty)
specularTintSelector = mkSelector "specularTint"

-- | @Selector@ for @roughness@
roughnessSelector :: Selector '[] (Id MDLMaterialProperty)
roughnessSelector = mkSelector "roughness"

-- | @Selector@ for @anisotropic@
anisotropicSelector :: Selector '[] (Id MDLMaterialProperty)
anisotropicSelector = mkSelector "anisotropic"

-- | @Selector@ for @anisotropicRotation@
anisotropicRotationSelector :: Selector '[] (Id MDLMaterialProperty)
anisotropicRotationSelector = mkSelector "anisotropicRotation"

-- | @Selector@ for @sheen@
sheenSelector :: Selector '[] (Id MDLMaterialProperty)
sheenSelector = mkSelector "sheen"

-- | @Selector@ for @sheenTint@
sheenTintSelector :: Selector '[] (Id MDLMaterialProperty)
sheenTintSelector = mkSelector "sheenTint"

-- | @Selector@ for @clearcoat@
clearcoatSelector :: Selector '[] (Id MDLMaterialProperty)
clearcoatSelector = mkSelector "clearcoat"

-- | @Selector@ for @clearcoatGloss@
clearcoatGlossSelector :: Selector '[] (Id MDLMaterialProperty)
clearcoatGlossSelector = mkSelector "clearcoatGloss"

