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
  , initWithIESProfileSelector
  , generateSphericalHarmonicsFromLightSelector
  , generateCubemapFromLightSelector
  , generateTextureSelector
  , lightCubeMapSelector
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

-- | @- initWithIESProfile:@
initWithIESProfile :: (IsMDLPhotometricLight mdlPhotometricLight, IsNSURL url) => mdlPhotometricLight -> url -> IO (Id MDLPhotometricLight)
initWithIESProfile mdlPhotometricLight  url =
withObjCPtr url $ \raw_url ->
    sendMsg mdlPhotometricLight (mkSelector "initWithIESProfile:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- generateSphericalHarmonicsFromLight:@
generateSphericalHarmonicsFromLight :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> CULong -> IO ()
generateSphericalHarmonicsFromLight mdlPhotometricLight  sphericalHarmonicsLevel =
  sendMsg mdlPhotometricLight (mkSelector "generateSphericalHarmonicsFromLight:") retVoid [argCULong (fromIntegral sphericalHarmonicsLevel)]

-- | @- generateCubemapFromLight:@
generateCubemapFromLight :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> CULong -> IO ()
generateCubemapFromLight mdlPhotometricLight  textureSize =
  sendMsg mdlPhotometricLight (mkSelector "generateCubemapFromLight:") retVoid [argCULong (fromIntegral textureSize)]

-- | generateTexture
--
-- Generate an IES compliant MDLTexture 1D when the number of horizontal angles is one and the innerConeAngle is < 180 2D when the previous statement fails and innerConeAngle < 89 3D in all other cases the parameter textureSize is the size in pixels of the texture image. For a size of N, 1D generates an Nx1 image, 2D generates an NxN image, 3D generates an Nx(N*6) image (i.e. cubemap).
--
-- ObjC selector: @- generateTexture:@
generateTexture :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> CULong -> IO (Id MDLTexture)
generateTexture mdlPhotometricLight  textureSize =
  sendMsg mdlPhotometricLight (mkSelector "generateTexture:") (retPtr retVoid) [argCULong (fromIntegral textureSize)] >>= retainedObject . castPtr

-- | @- lightCubeMap@
lightCubeMap :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> IO (Id MDLTexture)
lightCubeMap mdlPhotometricLight  =
  sendMsg mdlPhotometricLight (mkSelector "lightCubeMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sphericalHarmonicsLevel@
sphericalHarmonicsLevel :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> IO CULong
sphericalHarmonicsLevel mdlPhotometricLight  =
  sendMsg mdlPhotometricLight (mkSelector "sphericalHarmonicsLevel") retCULong []

-- | @- sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficients :: IsMDLPhotometricLight mdlPhotometricLight => mdlPhotometricLight -> IO (Id NSData)
sphericalHarmonicsCoefficients mdlPhotometricLight  =
  sendMsg mdlPhotometricLight (mkSelector "sphericalHarmonicsCoefficients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIESProfile:@
initWithIESProfileSelector :: Selector
initWithIESProfileSelector = mkSelector "initWithIESProfile:"

-- | @Selector@ for @generateSphericalHarmonicsFromLight:@
generateSphericalHarmonicsFromLightSelector :: Selector
generateSphericalHarmonicsFromLightSelector = mkSelector "generateSphericalHarmonicsFromLight:"

-- | @Selector@ for @generateCubemapFromLight:@
generateCubemapFromLightSelector :: Selector
generateCubemapFromLightSelector = mkSelector "generateCubemapFromLight:"

-- | @Selector@ for @generateTexture:@
generateTextureSelector :: Selector
generateTextureSelector = mkSelector "generateTexture:"

-- | @Selector@ for @lightCubeMap@
lightCubeMapSelector :: Selector
lightCubeMapSelector = mkSelector "lightCubeMap"

-- | @Selector@ for @sphericalHarmonicsLevel@
sphericalHarmonicsLevelSelector :: Selector
sphericalHarmonicsLevelSelector = mkSelector "sphericalHarmonicsLevel"

-- | @Selector@ for @sphericalHarmonicsCoefficients@
sphericalHarmonicsCoefficientsSelector :: Selector
sphericalHarmonicsCoefficientsSelector = mkSelector "sphericalHarmonicsCoefficients"

