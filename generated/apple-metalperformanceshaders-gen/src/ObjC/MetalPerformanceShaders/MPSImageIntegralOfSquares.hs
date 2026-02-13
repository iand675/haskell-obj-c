{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageIntegralOfSquares
--
-- The MPSImageIntegralOfSquares calculates the sum of squared pixels over a specified region in the image.              The value at each position is the sum of all squared pixels in a source image rectangle, sumRect:
--
-- sumRect.origin = MPSUnaryImageKernel.offset                  sumRect.size = dest_position - MPSUnaryImageKernel.clipRect.origin
--
-- If the channels in the source image are normalized, half-float or floating values,              the destination image is recommended to be a 32-bit floating-point image.              If the channels in the source image are integer values, it is recommended that              an appropriate 32-bit integer image destination format is used.
--
-- This kernel accepts uint and int textures in addition to unorm and floating-point textures.
--
-- Generated bindings for @MPSImageIntegralOfSquares@.
module ObjC.MetalPerformanceShaders.MPSImageIntegralOfSquares
  ( MPSImageIntegralOfSquares
  , IsMPSImageIntegralOfSquares(..)


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

