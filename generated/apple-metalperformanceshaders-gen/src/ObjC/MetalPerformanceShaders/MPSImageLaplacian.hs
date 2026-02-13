{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageLaplacian
--
-- The MPSImageLaplacian is an optimized variant of the MPSImageConvolution filter provided primarily for ease of use.              This filter uses an optimized convolution filter with a 3 x 3 kernel with the following weights:                  [ 0  1  0                    1 -4  1                    0  1  0 ]
--
-- The optimized convolution filter used by MPSImageLaplacian can also be used by creating a MPSImageConvolution              object with kernelWidth = 3, kernelHeight = 3 and weights as specified above.
--
-- Generated bindings for @MPSImageLaplacian@.
module ObjC.MetalPerformanceShaders.MPSImageLaplacian
  ( MPSImageLaplacian
  , IsMPSImageLaplacian(..)
  , bias
  , setBias
  , biasSelector
  , setBiasSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | bias
--
-- The bias is a value to be added to convolved pixel before it is converted back to the storage format.               It can be used to convert negative values into a representable range for a unsigned MTLPixelFormat.               For example, many edge detection filters produce results in the range [-k,k]. By scaling the filter               weights by 0.5/k and adding 0.5, the results will be in range [0,1] suitable for use with unorm formats.               It can be used in combination with renormalization of the filter weights to do video ranging as part               of the convolution effect. It can also just be used to increase the brightness of the image.
--
-- Default value is 0.0f.
--
-- ObjC selector: @- bias@
bias :: IsMPSImageLaplacian mpsImageLaplacian => mpsImageLaplacian -> IO CFloat
bias mpsImageLaplacian =
  sendMessage mpsImageLaplacian biasSelector

-- | bias
--
-- The bias is a value to be added to convolved pixel before it is converted back to the storage format.               It can be used to convert negative values into a representable range for a unsigned MTLPixelFormat.               For example, many edge detection filters produce results in the range [-k,k]. By scaling the filter               weights by 0.5/k and adding 0.5, the results will be in range [0,1] suitable for use with unorm formats.               It can be used in combination with renormalization of the filter weights to do video ranging as part               of the convolution effect. It can also just be used to increase the brightness of the image.
--
-- Default value is 0.0f.
--
-- ObjC selector: @- setBias:@
setBias :: IsMPSImageLaplacian mpsImageLaplacian => mpsImageLaplacian -> CFloat -> IO ()
setBias mpsImageLaplacian value =
  sendMessage mpsImageLaplacian setBiasSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bias@
biasSelector :: Selector '[] CFloat
biasSelector = mkSelector "bias"

-- | @Selector@ for @setBias:@
setBiasSelector :: Selector '[CFloat] ()
setBiasSelector = mkSelector "setBias:"

