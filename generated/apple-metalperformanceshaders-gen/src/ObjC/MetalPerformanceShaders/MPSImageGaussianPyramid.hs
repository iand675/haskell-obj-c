{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageGaussianPyramid
--
-- A Gaussian image pyramid is constructed as follows:              The mipmap level zero is the source of the operation and is left untouched and              the subsequent mipmap levels are constructed from it recursively:
--
-- mip[ level = n + 1 ] = Downsample( filter( mip[ level = n ] ) ), where
--
-- "filter()" applies a filter with the specified convolution kernel and              "Downsample()" removes odd rows and columns from the input image.              The default convolution filter kernel for this operation is
--
-- k = w w^T, where w = [ 1/16,  1/4,  3/8,  1/4,  1/16 ]^T,
--
-- but the user may also tweak this kernel with a centerWeight parameter: 'a':
--
-- k = w w^T, where w = [ (1/4 - a/2),  1/4,  a,  1/4,  (1/4 - a/2) ]^T
--
-- or the user can provide a completely custom kernel.
--
-- This procedure is continued until every mipmap level present in the image texture are              filled with the pyramid levels.
--
-- In case of the Gaussian pyramid the user must run the operation in-place using:              MPSUnaryImageKernel::encodeToCommandBuffer:inPlaceTexture:fallbackCopyAllocator:,              where the fallback allocator is ignored.
--
-- Generated bindings for @MPSImageGaussianPyramid@.
module ObjC.MetalPerformanceShaders.MPSImageGaussianPyramid
  ( MPSImageGaussianPyramid
  , IsMPSImageGaussianPyramid(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

