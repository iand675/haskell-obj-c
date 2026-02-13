{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLLightProbe@.
module ObjC.ModelIO.MDLLightProbe
  ( MDLLightProbe
  , IsMDLLightProbe(..)
  , initWithReflectiveTexture_irradianceTexture
  , generateSphericalHarmonicsFromIrradiance
  , lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemap
  , reflectiveTexture
  , irradianceTexture
  , sphericalHarmonicsLevel
  , sphericalHarmonicsCoefficients
  , generateSphericalHarmonicsFromIrradianceSelector
  , initWithReflectiveTexture_irradianceTextureSelector
  , irradianceTextureSelector
  , lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemapSelector
  , reflectiveTextureSelector
  , sphericalHarmonicsCoefficientsSelector
  , sphericalHarmonicsLevelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithReflectiveTexture:irradianceTexture:@
initWithReflectiveTexture_irradianceTexture :: (IsMDLLightProbe mdlLightProbe, IsMDLTexture reflectiveTexture, IsMDLTexture irradianceTexture) => mdlLightProbe -> reflectiveTexture -> irradianceTexture -> IO (Id MDLLightProbe)
initWithReflectiveTexture_irradianceTexture mdlLightProbe reflectiveTexture irradianceTexture =
  sendOwnedMessage mdlLightProbe initWithReflectiveTexture_irradianceTextureSelector (toMDLTexture reflectiveTexture) (toMDLTexture irradianceTexture)

-- | @- generateSphericalHarmonicsFromIrradiance:@
generateSphericalHarmonicsFromIrradiance :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> CULong -> IO ()
generateSphericalHarmonicsFromIrradiance mdlLightProbe sphericalHarmonicsLevel =
  sendMessage mdlLightProbe generateSphericalHarmonicsFromIrradianceSelector sphericalHarmonicsLevel

-- | @+ lightProbeWithTextureSize:forLocation:lightsToConsider:objectsToConsider:reflectiveCubemap:irradianceCubemap:@
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemap :: (IsMDLTransform transform, IsNSArray lightsToConsider, IsNSArray objectsToConsider, IsMDLTexture reflectiveCubemap, IsMDLTexture irradianceCubemap) => CLong -> transform -> lightsToConsider -> objectsToConsider -> reflectiveCubemap -> irradianceCubemap -> IO (Id MDLLightProbe)
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemap textureSize transform lightsToConsider objectsToConsider reflectiveCubemap irradianceCubemap =
  do
    cls' <- getRequiredClass "MDLLightProbe"
    sendClassMessage cls' lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemapSelector textureSize (toMDLTransform transform) (toNSArray lightsToConsider) (toNSArray objectsToConsider) (toMDLTexture reflectiveCubemap) (toMDLTexture irradianceCubemap)

-- | @- reflectiveTexture@
reflectiveTexture :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO (Id MDLTexture)
reflectiveTexture mdlLightProbe =
  sendMessage mdlLightProbe reflectiveTextureSelector

-- | @- irradianceTexture@
irradianceTexture :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO (Id MDLTexture)
irradianceTexture mdlLightProbe =
  sendMessage mdlLightProbe irradianceTextureSelector

-- | @- sphericalHarmonicsLevel@
sphericalHarmonicsLevel :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO CULong
sphericalHarmonicsLevel mdlLightProbe =
  sendMessage mdlLightProbe sphericalHarmonicsLevelSelector

-- | @- sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficients :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO (Id NSData)
sphericalHarmonicsCoefficients mdlLightProbe =
  sendMessage mdlLightProbe sphericalHarmonicsCoefficientsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReflectiveTexture:irradianceTexture:@
initWithReflectiveTexture_irradianceTextureSelector :: Selector '[Id MDLTexture, Id MDLTexture] (Id MDLLightProbe)
initWithReflectiveTexture_irradianceTextureSelector = mkSelector "initWithReflectiveTexture:irradianceTexture:"

-- | @Selector@ for @generateSphericalHarmonicsFromIrradiance:@
generateSphericalHarmonicsFromIrradianceSelector :: Selector '[CULong] ()
generateSphericalHarmonicsFromIrradianceSelector = mkSelector "generateSphericalHarmonicsFromIrradiance:"

-- | @Selector@ for @lightProbeWithTextureSize:forLocation:lightsToConsider:objectsToConsider:reflectiveCubemap:irradianceCubemap:@
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemapSelector :: Selector '[CLong, Id MDLTransform, Id NSArray, Id NSArray, Id MDLTexture, Id MDLTexture] (Id MDLLightProbe)
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemapSelector = mkSelector "lightProbeWithTextureSize:forLocation:lightsToConsider:objectsToConsider:reflectiveCubemap:irradianceCubemap:"

-- | @Selector@ for @reflectiveTexture@
reflectiveTextureSelector :: Selector '[] (Id MDLTexture)
reflectiveTextureSelector = mkSelector "reflectiveTexture"

-- | @Selector@ for @irradianceTexture@
irradianceTextureSelector :: Selector '[] (Id MDLTexture)
irradianceTextureSelector = mkSelector "irradianceTexture"

-- | @Selector@ for @sphericalHarmonicsLevel@
sphericalHarmonicsLevelSelector :: Selector '[] CULong
sphericalHarmonicsLevelSelector = mkSelector "sphericalHarmonicsLevel"

-- | @Selector@ for @sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficientsSelector :: Selector '[] (Id NSData)
sphericalHarmonicsCoefficientsSelector = mkSelector "sphericalHarmonicsCoefficients"

