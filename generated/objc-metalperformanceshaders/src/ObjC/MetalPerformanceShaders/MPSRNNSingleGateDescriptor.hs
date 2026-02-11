{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNSingleGateDescriptor
--
-- This depends on Metal.framework
--
-- The MPSRNNSingleGateDescriptor specifies a simple recurrent block/layer descriptor.              The RNN layer initialized with a MPSRNNSingleGateDescriptor transforms the input data (image or matrix),              and previous output with a set of filters, each producing one feature map in the new output data.              The user may provide the RNN unit a single input or a sequence of inputs.
--
-- Description of operation:
--
-- Let x_j be the input data (at time index t of sequence,                          j index containing quadruplet: batch index, x,y and feature index (x=y=0 for matrices)).              Let h0_j be the recurrent input (previous output) data from previous time step (at time index t-1 of sequence).              Let h1_i be the output data produced at this time step.
--
-- Let W_ij, U_ij be the weights for input and recurrent input data respectively              Let b_i be a bias term
--
-- Let gi(x) be a neuron activation function
--
-- Then the new output image h1_i data is computed as follows:
--
-- h1_i = gi( W_ij * x_j + U_ij * h0_j  + b_i )
--
-- The '*' stands for convolution (see MPSRNNImageInferenceLayer) or matrix-vector/matrix multiplication              (see MPSRNNMatrixInferenceLayer).              Summation is over index j (except for the batch index), but there is no summation over              repeated index i - the output index.              Note that for validity all intermediate images have to be of same size and the U matrix has to be square              (ie. outputFeatureChannels == inputFeatureChannels in those). Also the bias terms are scalars wrt. spatial dimensions.
--
-- Generated bindings for @MPSRNNSingleGateDescriptor@.
module ObjC.MetalPerformanceShaders.MPSRNNSingleGateDescriptor
  ( MPSRNNSingleGateDescriptor
  , IsMPSRNNSingleGateDescriptor(..)
  , createRNNSingleGateDescriptorWithInputFeatureChannels_outputFeatureChannels
  , createRNNSingleGateDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector


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

-- | Creates a MPSRNNSingleGateDescriptor
--
-- @inputFeatureChannels@ — The number of feature channels in the input image/matrix. Must be >= 1.
--
-- @outputFeatureChannels@ — The number of feature channels in the output image/matrix. Must be >= 1.
--
-- Returns: A valid MPSRNNSingleGateDescriptor object or nil, if failure.
--
-- ObjC selector: @+ createRNNSingleGateDescriptorWithInputFeatureChannels:outputFeatureChannels:@
createRNNSingleGateDescriptorWithInputFeatureChannels_outputFeatureChannels :: CULong -> CULong -> IO (Id MPSRNNSingleGateDescriptor)
createRNNSingleGateDescriptorWithInputFeatureChannels_outputFeatureChannels inputFeatureChannels outputFeatureChannels =
  do
    cls' <- getRequiredClass "MPSRNNSingleGateDescriptor"
    sendClassMsg cls' (mkSelector "createRNNSingleGateDescriptorWithInputFeatureChannels:outputFeatureChannels:") (retPtr retVoid) [argCULong (fromIntegral inputFeatureChannels), argCULong (fromIntegral outputFeatureChannels)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createRNNSingleGateDescriptorWithInputFeatureChannels:outputFeatureChannels:@
createRNNSingleGateDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector :: Selector
createRNNSingleGateDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector = mkSelector "createRNNSingleGateDescriptorWithInputFeatureChannels:outputFeatureChannels:"

