{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNSubPixelConvolutionDescriptor can be used to create MPSCNNConvolution object that does sub pixel upsamling                    and reshaping opeartion as described in                        http://www.cv-foundation.org/openaccess/content_cvpr_2016/papers/Shi_Real-Time_Single_Image_CVPR_2016_paper.pdf
--
-- Conceptually MPSCNNConvolution with subPixelScaleFactor > 1 can be thought of as filter performing regular CNN convolution producing N output feature channels at each pixel of                   an intermediate MPSImage followed by a kernel that rearranges/reshapes these N channels at each pixel of intermediate MPSImage into a pixel block of                   size subPixelScaleFactor x subPixelScaleFactor with N/(subPixelScaleFactor * subPixelScaleFactor) featureChannels at each pixel of this pixel block. Thus each pixel in intermedaite                   MPSImage with N channels map to subPixelScaleFactor x subPixelScaleFactor pixel block in final destination MPSImage with N/(subPixelScaleFactor * subPixelScaleFactor) featureChannels.                   MPSCNNConvolution with subPixelScaleFactor > 1 fuses the convolution and reshaping operation into single compute kernel thus not only saving DRAM roundtrip but also memory                   needed for intermediate MPSImage had these operation done separately.                   Let N be the value of outputFeatureChannels property and let r = subPixelScaleFactor.                   Conceptually Convolution will produce intermedaite image Io of dimensions (treated as 3D tensor) width x height x N where                              width = (clipRect.size.width + r - 1) / r                              height = (clipRect.size.height + r -1) / r                   Reshaping happens as follows
--
-- Destination[clipRect.origin.x+x][clipRect.origin.y+y][c] = Io[ floor(x/r) ][ floor(y/r) ][ (N/r^2) * ( r * mod(y,r) + mod(x,r) ) + c ]
-- where x in [0,clipRect.size.width-1], y in [0,clipRect.size.height-1], c in [0,N/r^2 - 1]
--
-- The following conditions must be met:                   1) N (outputFeatureChannels) must be multiple of r^2 (subPixelScaleFactor * subPixelScaleFactor).                   2) The destination MPSImage to encode call must have at least N/r^2 + destinationFeatureChannelOffset channels.                   3) Number of feature channels in reshaped output image (N/r^2) can be any value when groups = 1 but must be multiple of 4 when groups > 1.
--
-- Generated bindings for @MPSCNNSubPixelConvolutionDescriptor@.
module ObjC.MetalPerformanceShaders.MPSCNNSubPixelConvolutionDescriptor
  ( MPSCNNSubPixelConvolutionDescriptor
  , IsMPSCNNSubPixelConvolutionDescriptor(..)
  , subPixelScaleFactor
  , setSubPixelScaleFactor
  , subPixelScaleFactorSelector
  , setSubPixelScaleFactorSelector


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

-- | subPixelScaleFactor
--
-- Upsampling scale factor. Each pixel in input is upsampled into a subPixelScaleFactor x subPixelScaleFactor pixel block by rearranging                 the outputFeatureChannels as described above. Default value is 1.
--
-- ObjC selector: @- subPixelScaleFactor@
subPixelScaleFactor :: IsMPSCNNSubPixelConvolutionDescriptor mpscnnSubPixelConvolutionDescriptor => mpscnnSubPixelConvolutionDescriptor -> IO CULong
subPixelScaleFactor mpscnnSubPixelConvolutionDescriptor  =
  sendMsg mpscnnSubPixelConvolutionDescriptor (mkSelector "subPixelScaleFactor") retCULong []

-- | subPixelScaleFactor
--
-- Upsampling scale factor. Each pixel in input is upsampled into a subPixelScaleFactor x subPixelScaleFactor pixel block by rearranging                 the outputFeatureChannels as described above. Default value is 1.
--
-- ObjC selector: @- setSubPixelScaleFactor:@
setSubPixelScaleFactor :: IsMPSCNNSubPixelConvolutionDescriptor mpscnnSubPixelConvolutionDescriptor => mpscnnSubPixelConvolutionDescriptor -> CULong -> IO ()
setSubPixelScaleFactor mpscnnSubPixelConvolutionDescriptor  value =
  sendMsg mpscnnSubPixelConvolutionDescriptor (mkSelector "setSubPixelScaleFactor:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @subPixelScaleFactor@
subPixelScaleFactorSelector :: Selector
subPixelScaleFactorSelector = mkSelector "subPixelScaleFactor"

-- | @Selector@ for @setSubPixelScaleFactor:@
setSubPixelScaleFactorSelector :: Selector
setSubPixelScaleFactorSelector = mkSelector "setSubPixelScaleFactor:"

