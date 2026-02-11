{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDepthWiseConvolutionDescriptor can be used to create MPSCNNConvolution object that does depthwise convolution
--
-- Depthwise convolution applies different filter to each input feature channel i.e. no cross channel mixing.                    Number of outputFeatureChannels can be greater than number of inputFeatureChannels, in which case convolution                    expects channelMultipler = outputFeactureChannels/inputFeatureChannels number of filters for each input channel.                    This means channelMultipler filters are applied to each input feature channel producing channelMultipler output feature channels.                    All channelMultipler output feature channels produced by single input feature channel are stored togather in output image i.e.                              output[x,y,k*channelMultiplier + q] = input[x,y,k] * filter[k,q]                    where * here denotes convolution.                    group must be 1.                    Weights array returned by MPSCNNConvolutionDataProvier is interpreted as                              Weights [inputFeatureChannels] [channelMultiplier] [kH] [kW]                            = Weights [ inputFeatureChannels * channelMultiplier ] [kH] [kW]                            = Weights [ outputFeatureChannels ] [kH] [kW]
--
-- Currently only channel multipler of 1 is supported i.e. inputFeatureChannels == outputFeatureChannels
--
-- Generated bindings for @MPSCNNDepthWiseConvolutionDescriptor@.
module ObjC.MetalPerformanceShaders.MPSCNNDepthWiseConvolutionDescriptor
  ( MPSCNNDepthWiseConvolutionDescriptor
  , IsMPSCNNDepthWiseConvolutionDescriptor(..)
  , channelMultiplier
  , channelMultiplierSelector


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

-- | channelMultiplier
--
-- Ratio of outputFeactureChannel to inputFeatureChannels for depthwise convolution i.e. how many output feature channels are                 produced by each input channel.
--
-- ObjC selector: @- channelMultiplier@
channelMultiplier :: IsMPSCNNDepthWiseConvolutionDescriptor mpscnnDepthWiseConvolutionDescriptor => mpscnnDepthWiseConvolutionDescriptor -> IO CULong
channelMultiplier mpscnnDepthWiseConvolutionDescriptor  =
  sendMsg mpscnnDepthWiseConvolutionDescriptor (mkSelector "channelMultiplier") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channelMultiplier@
channelMultiplierSelector :: Selector
channelMultiplierSelector = mkSelector "channelMultiplier"

