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
  , initWithReflectiveTexture_irradianceTextureSelector
  , generateSphericalHarmonicsFromIrradianceSelector
  , lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemapSelector
  , reflectiveTextureSelector
  , irradianceTextureSelector
  , sphericalHarmonicsLevelSelector
  , sphericalHarmonicsCoefficientsSelector


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

-- | @- initWithReflectiveTexture:irradianceTexture:@
initWithReflectiveTexture_irradianceTexture :: (IsMDLLightProbe mdlLightProbe, IsMDLTexture reflectiveTexture, IsMDLTexture irradianceTexture) => mdlLightProbe -> reflectiveTexture -> irradianceTexture -> IO (Id MDLLightProbe)
initWithReflectiveTexture_irradianceTexture mdlLightProbe  reflectiveTexture irradianceTexture =
withObjCPtr reflectiveTexture $ \raw_reflectiveTexture ->
  withObjCPtr irradianceTexture $ \raw_irradianceTexture ->
      sendMsg mdlLightProbe (mkSelector "initWithReflectiveTexture:irradianceTexture:") (retPtr retVoid) [argPtr (castPtr raw_reflectiveTexture :: Ptr ()), argPtr (castPtr raw_irradianceTexture :: Ptr ())] >>= ownedObject . castPtr

-- | @- generateSphericalHarmonicsFromIrradiance:@
generateSphericalHarmonicsFromIrradiance :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> CULong -> IO ()
generateSphericalHarmonicsFromIrradiance mdlLightProbe  sphericalHarmonicsLevel =
  sendMsg mdlLightProbe (mkSelector "generateSphericalHarmonicsFromIrradiance:") retVoid [argCULong (fromIntegral sphericalHarmonicsLevel)]

-- | @+ lightProbeWithTextureSize:forLocation:lightsToConsider:objectsToConsider:reflectiveCubemap:irradianceCubemap:@
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemap :: (IsMDLTransform transform, IsNSArray lightsToConsider, IsNSArray objectsToConsider, IsMDLTexture reflectiveCubemap, IsMDLTexture irradianceCubemap) => CLong -> transform -> lightsToConsider -> objectsToConsider -> reflectiveCubemap -> irradianceCubemap -> IO (Id MDLLightProbe)
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemap textureSize transform lightsToConsider objectsToConsider reflectiveCubemap irradianceCubemap =
  do
    cls' <- getRequiredClass "MDLLightProbe"
    withObjCPtr transform $ \raw_transform ->
      withObjCPtr lightsToConsider $ \raw_lightsToConsider ->
        withObjCPtr objectsToConsider $ \raw_objectsToConsider ->
          withObjCPtr reflectiveCubemap $ \raw_reflectiveCubemap ->
            withObjCPtr irradianceCubemap $ \raw_irradianceCubemap ->
              sendClassMsg cls' (mkSelector "lightProbeWithTextureSize:forLocation:lightsToConsider:objectsToConsider:reflectiveCubemap:irradianceCubemap:") (retPtr retVoid) [argCLong (fromIntegral textureSize), argPtr (castPtr raw_transform :: Ptr ()), argPtr (castPtr raw_lightsToConsider :: Ptr ()), argPtr (castPtr raw_objectsToConsider :: Ptr ()), argPtr (castPtr raw_reflectiveCubemap :: Ptr ()), argPtr (castPtr raw_irradianceCubemap :: Ptr ())] >>= retainedObject . castPtr

-- | @- reflectiveTexture@
reflectiveTexture :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO (Id MDLTexture)
reflectiveTexture mdlLightProbe  =
  sendMsg mdlLightProbe (mkSelector "reflectiveTexture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- irradianceTexture@
irradianceTexture :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO (Id MDLTexture)
irradianceTexture mdlLightProbe  =
  sendMsg mdlLightProbe (mkSelector "irradianceTexture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sphericalHarmonicsLevel@
sphericalHarmonicsLevel :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO CULong
sphericalHarmonicsLevel mdlLightProbe  =
  sendMsg mdlLightProbe (mkSelector "sphericalHarmonicsLevel") retCULong []

-- | @- sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficients :: IsMDLLightProbe mdlLightProbe => mdlLightProbe -> IO (Id NSData)
sphericalHarmonicsCoefficients mdlLightProbe  =
  sendMsg mdlLightProbe (mkSelector "sphericalHarmonicsCoefficients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReflectiveTexture:irradianceTexture:@
initWithReflectiveTexture_irradianceTextureSelector :: Selector
initWithReflectiveTexture_irradianceTextureSelector = mkSelector "initWithReflectiveTexture:irradianceTexture:"

-- | @Selector@ for @generateSphericalHarmonicsFromIrradiance:@
generateSphericalHarmonicsFromIrradianceSelector :: Selector
generateSphericalHarmonicsFromIrradianceSelector = mkSelector "generateSphericalHarmonicsFromIrradiance:"

-- | @Selector@ for @lightProbeWithTextureSize:forLocation:lightsToConsider:objectsToConsider:reflectiveCubemap:irradianceCubemap:@
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemapSelector :: Selector
lightProbeWithTextureSize_forLocation_lightsToConsider_objectsToConsider_reflectiveCubemap_irradianceCubemapSelector = mkSelector "lightProbeWithTextureSize:forLocation:lightsToConsider:objectsToConsider:reflectiveCubemap:irradianceCubemap:"

-- | @Selector@ for @reflectiveTexture@
reflectiveTextureSelector :: Selector
reflectiveTextureSelector = mkSelector "reflectiveTexture"

-- | @Selector@ for @irradianceTexture@
irradianceTextureSelector :: Selector
irradianceTextureSelector = mkSelector "irradianceTexture"

-- | @Selector@ for @sphericalHarmonicsLevel@
sphericalHarmonicsLevelSelector :: Selector
sphericalHarmonicsLevelSelector = mkSelector "sphericalHarmonicsLevel"

-- | @Selector@ for @sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficientsSelector :: Selector
sphericalHarmonicsCoefficientsSelector = mkSelector "sphericalHarmonicsCoefficients"

