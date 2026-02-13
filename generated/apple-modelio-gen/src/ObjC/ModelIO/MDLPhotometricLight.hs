{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLPhotometricLight
--
-- A light created from measurements at various angles.
--
-- lightCubeMap A cube map that can be sampled at various directions to
--
-- learn the intensity of the light in that direction.
--
-- sphericalHarmonicsLevel The value generateSphericalHarmonicsFromLight:
--
-- used to calculate the spherical harmonics coefficients
--
-- sphericalHarmonicsCoefficients The spherical harmonic coefficiencts
--
-- calculated by generateSphericalHarmonicsFromLight:
--
-- Generated bindings for @MDLPhotometricLight@.
module ObjC.ModelIO.MDLPhotometricLight
  ( MDLPhotometricLight
  , IsMDLPhotometricLight(..)
  , initWithIESProfile
  , generateSphericalHarmonicsFromLight
  , generateCubemapFromLight
  , generateTexture
  , lightCubeMap
  , sphericalHarmonicsLevel
  , sphericalHarmonicsCoefficients
  , generateCubemapFromLightSelector
  , generateSphericalHarmonicsFromLightSelector
  , generateTextureSelector
  , initWithIESProfileSelector
  , lightCubeMapSelector
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

-- | @- initWithIESProfile:@
initWithIESProfile :: (IsMDLPhotometricLight mdlPhotometricLight, IsNSURL url) => mdlPhotometricLight -> url -> IO (Id MDLPhotometricLight)
initWithIESProfile mdlPhotometricLight url =
  sendOwnedMessage mdlPhotometricLight initWithIESProfileSelector (toNSURL url)

-- | @- generateSphericalHarmonicsFromLight:@
generateSphericalHarmonicsFromLight :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> CULong -> IO ()
generateSphericalHarmonicsFromLight mdlPhotometricLight sphericalHarmonicsLevel =
  sendMessage mdlPhotometricLight generateSphericalHarmonicsFromLightSelector sphericalHarmonicsLevel

-- | @- generateCubemapFromLight:@
generateCubemapFromLight :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> CULong -> IO ()
generateCubemapFromLight mdlPhotometricLight textureSize =
  sendMessage mdlPhotometricLight generateCubemapFromLightSelector textureSize

-- | generateTexture
--
-- Generate an IES compliant MDLTexture 1D when the number of horizontal angles is one and the innerConeAngle is < 180 2D when the previous statement fails and innerConeAngle < 89 3D in all other cases the parameter textureSize is the size in pixels of the texture image. For a size of N, 1D generates an Nx1 image, 2D generates an NxN image, 3D generates an Nx(N*6) image (i.e. cubemap).
--
-- ObjC selector: @- generateTexture:@
generateTexture :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> CULong -> IO (Id MDLTexture)
generateTexture mdlPhotometricLight textureSize =
  sendMessage mdlPhotometricLight generateTextureSelector textureSize

-- | @- lightCubeMap@
lightCubeMap :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> IO (Id MDLTexture)
lightCubeMap mdlPhotometricLight =
  sendMessage mdlPhotometricLight lightCubeMapSelector

-- | @- sphericalHarmonicsLevel@
sphericalHarmonicsLevel :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> IO CULong
sphericalHarmonicsLevel mdlPhotometricLight =
  sendMessage mdlPhotometricLight sphericalHarmonicsLevelSelector

-- | @- sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficients :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> IO (Id NSData)
sphericalHarmonicsCoefficients mdlPhotometricLight =
  sendMessage mdlPhotometricLight sphericalHarmonicsCoefficientsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIESProfile:@
initWithIESProfileSelector :: Selector '[Id NSURL] (Id MDLPhotometricLight)
initWithIESProfileSelector = mkSelector "initWithIESProfile:"

-- | @Selector@ for @generateSphericalHarmonicsFromLight:@
generateSphericalHarmonicsFromLightSelector :: Selector '[CULong] ()
generateSphericalHarmonicsFromLightSelector = mkSelector "generateSphericalHarmonicsFromLight:"

-- | @Selector@ for @generateCubemapFromLight:@
generateCubemapFromLightSelector :: Selector '[CULong] ()
generateCubemapFromLightSelector = mkSelector "generateCubemapFromLight:"

-- | @Selector@ for @generateTexture:@
generateTextureSelector :: Selector '[CULong] (Id MDLTexture)
generateTextureSelector = mkSelector "generateTexture:"

-- | @Selector@ for @lightCubeMap@
lightCubeMapSelector :: Selector '[] (Id MDLTexture)
lightCubeMapSelector = mkSelector "lightCubeMap"

-- | @Selector@ for @sphericalHarmonicsLevel@
sphericalHarmonicsLevelSelector :: Selector '[] CULong
sphericalHarmonicsLevelSelector = mkSelector "sphericalHarmonicsLevel"

-- | @Selector@ for @sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficientsSelector :: Selector '[] (Id NSData)
sphericalHarmonicsCoefficientsSelector = mkSelector "sphericalHarmonicsCoefficients"

