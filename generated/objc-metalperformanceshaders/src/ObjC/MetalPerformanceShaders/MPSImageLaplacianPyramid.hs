{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageLaplacianPyramid
--
-- Laplacian pyramid levels are constructed as difference between the current source level and 2x interpolated version of the              half-resolution source level immediately above it.
--
-- LaplacianMipLevel[l] := GaussianMipLevel[l] – Interpolate(GaussianMipLevel[l + 1])
--
-- The Interpolate function is the classical 2x signal interpolation procedure applied                  to all color channels of the source mip-level in both dimensions.                  It is logically equivalent to the following two-step process :                      1) Zero-stuffing (sometimes called "upsampling").                         It is the process of interleaving source pixel values with zero values:                         dst.at(x, y) := src.at(x, y) if even(x) and even(y) else 0                      2) Filtering (sometimes called "interpolation").                         It is the same procedure as implemented by the MPSImageConvolution class,                         using filter weights provided by the initializer methods inherited from MPSImagePyramid.
--
-- The source for Laplacian pyramid construction is typically produced              by the Gaussian pyramid algorithm -- a closely related image processing technique,              but the Laplacian pyramid construction itself makes no assumptions neither about               the data stored in the source texture nor about the interpolation filter weights,              so Gaussian pyramid is just a conventional name for the source texture.
--
-- Please refer to the classical "The Laplacian Pyramid as a Compact Image Code" whitepaper               by Burt & Anderson, originally published in 532 IEEE TRANSACTIONS ON COMMUNICATIONS, VOL. COM-3l, NO. 4, APRIL 1983              for more detailed discussion.
--
-- Since the subtraction operation extends the value range of LaplacianMipLevelRaw              relative to the value range of GaussianMipLevel (even for the case of              normalized interpolation filter), in order to avoid unwanted range clamping              when working with normalized texture types, laplacianBias and laplacianScale class properties              specify point-wise linear mapping of the LaplacianMipLevelRaw result data              into the value range of the destination texture :                  LaplacianRangeScale(pixel, laplacianBias, laplacianScale) := laplacianBias + pixel * laplacianScale,                  LaplacianMipLevelStored[j]                                := LaplacianRangeScale(LaplacianMipLevel[j], laplacianBias, laplacianScale),                  with the default values being laplacianBias = 0.0, laplacianScale = 1.0
--
-- Limitations of the current software revision :                 1) In-place operation is not supported, e.g. source and destination textures need                     to have separate storage and can't be aliased.                 2) The number of channels, bit depth and resolution of the source and destination textures need to match.                 3) Values of the offset and clipRect properties are fixed to the defaults provided by MPSUnaryImageKernel                     (from which they are inherited), corresponding to no offset applied to the source and unbounded region of interest                    in every destination mip-level; all updates to these properties are ignored.
--
-- Generated bindings for @MPSImageLaplacianPyramid@.
module ObjC.MetalPerformanceShaders.MPSImageLaplacianPyramid
  ( MPSImageLaplacianPyramid
  , IsMPSImageLaplacianPyramid(..)
  , laplacianBias
  , setLaplacianBias
  , laplacianScale
  , setLaplacianScale
  , laplacianBiasSelector
  , setLaplacianBiasSelector
  , laplacianScaleSelector
  , setLaplacianScaleSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- laplacianBias@
laplacianBias :: IsMPSImageLaplacianPyramid mpsImageLaplacianPyramid => mpsImageLaplacianPyramid -> IO CFloat
laplacianBias mpsImageLaplacianPyramid  =
  sendMsg mpsImageLaplacianPyramid (mkSelector "laplacianBias") retCFloat []

-- | @- setLaplacianBias:@
setLaplacianBias :: IsMPSImageLaplacianPyramid mpsImageLaplacianPyramid => mpsImageLaplacianPyramid -> CFloat -> IO ()
setLaplacianBias mpsImageLaplacianPyramid  value =
  sendMsg mpsImageLaplacianPyramid (mkSelector "setLaplacianBias:") retVoid [argCFloat (fromIntegral value)]

-- | @- laplacianScale@
laplacianScale :: IsMPSImageLaplacianPyramid mpsImageLaplacianPyramid => mpsImageLaplacianPyramid -> IO CFloat
laplacianScale mpsImageLaplacianPyramid  =
  sendMsg mpsImageLaplacianPyramid (mkSelector "laplacianScale") retCFloat []

-- | @- setLaplacianScale:@
setLaplacianScale :: IsMPSImageLaplacianPyramid mpsImageLaplacianPyramid => mpsImageLaplacianPyramid -> CFloat -> IO ()
setLaplacianScale mpsImageLaplacianPyramid  value =
  sendMsg mpsImageLaplacianPyramid (mkSelector "setLaplacianScale:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @laplacianBias@
laplacianBiasSelector :: Selector
laplacianBiasSelector = mkSelector "laplacianBias"

-- | @Selector@ for @setLaplacianBias:@
setLaplacianBiasSelector :: Selector
setLaplacianBiasSelector = mkSelector "setLaplacianBias:"

-- | @Selector@ for @laplacianScale@
laplacianScaleSelector :: Selector
laplacianScaleSelector = mkSelector "laplacianScale"

-- | @Selector@ for @setLaplacianScale:@
setLaplacianScaleSelector :: Selector
setLaplacianScaleSelector = mkSelector "setLaplacianScale:"

