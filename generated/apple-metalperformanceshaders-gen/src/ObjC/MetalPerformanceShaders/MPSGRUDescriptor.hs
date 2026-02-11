{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSGRUDescriptor
--
-- This depends on Metal.framework
--
-- The MPSGRUDescriptor specifies a GRU (Gated Recurrent Unit) block/layer descriptor.              The RNN layer initialized with a MPSGRUDescriptor transforms the input data (image or matrix),              and previous output with a set of filters, each producing one feature map in              the output data according to the Gated unit formulae detailed below.              The user may provide the GRU unit a single input or a sequence of inputs. The layer also supports              p-norm gating (Detailed in: https://arxiv.org/abs/1608.03639 ).
--
-- Description of operation:
--
-- Let x_j be the input data (at time index t of sequence,                          j index containing quadruplet: batch index, x,y and feature index (x=y=0 for matrices)).              Let h0_j be the recurrent input (previous output) data from previous time step (at time index t-1 of sequence).              Let h_i be the proposed new output.              Let h1_i be the output data produced at this time step.
--
-- Let Wz_ij, Uz_ij, be the input gate weights for input and recurrent input data respectively              Let bi_i be the bias for the input gate
--
-- Let Wr_ij, Ur_ij be the recurrent gate weights for input and recurrent input data respectively              Let br_i be the bias for the recurrent gate
--
-- Let Wh_ij, Uh_ij, Vh_ij, be the output gate weights for input, recurrent gate and input gate respectively              Let bh_i be the bias for the output gate
--
-- Let gz(x), gr(x), gh(x) be the neuron activation function for the input, recurrent and output gates              Let p > 0 be a scalar variable (typicall p >= 1.0) that defines the p-norm gating norm value.
--
-- Then the output of the Gated Recurrent Unit layer is computed as follows:
--
-- z_i = gz(  Wz_ij * x_j  +  Uz_ij * h0_j  +  bz_i  )                      r_i = gr(  Wr_ij * x_j  +  Ur_ij * h0_j  +  br_i  )                      c_i =      Uh_ij * (r_j h0_j)  +  Vh_ij * (z_j h0_j)                      h_i = gh(  Wh_ij * x_j  + c_i + bh_i  )
--
-- h1_i = ( 1 - z_i ^ p)^(1/p) h_i + z_i h0_i
--
-- The '*' stands for convolution (see MPSRNNImageInferenceLayer) or matrix-vector/matrix multiplication              (see MPSRNNMatrixInferenceLayer).              Summation is over index j (except for the batch index), but there is no summation over              repeated index i - the output index.              Note that for validity all intermediate images have to be of same size and all U and V matrices have to be square              (ie. outputFeatureChannels == inputFeatureChannels in those). Also the bias terms are scalars wrt. spatial dimensions.              The conventional GRU block is achieved by setting Vh = 0 (nil) and the so-called Minimal Gated Unit is achieved with Uh = 0.              (The Minimal Gated Unit is detailed in: https://arxiv.org/abs/1603.09420 and there they call z_i the value of the forget gate).
--
-- Generated bindings for @MPSGRUDescriptor@.
module ObjC.MetalPerformanceShaders.MPSGRUDescriptor
  ( MPSGRUDescriptor
  , IsMPSGRUDescriptor(..)
  , createGRUDescriptorWithInputFeatureChannels_outputFeatureChannels
  , inputGateInputWeights
  , setInputGateInputWeights
  , inputGateRecurrentWeights
  , setInputGateRecurrentWeights
  , recurrentGateInputWeights
  , setRecurrentGateInputWeights
  , recurrentGateRecurrentWeights
  , setRecurrentGateRecurrentWeights
  , outputGateInputWeights
  , setOutputGateInputWeights
  , outputGateRecurrentWeights
  , setOutputGateRecurrentWeights
  , outputGateInputGateWeights
  , setOutputGateInputGateWeights
  , gatePnormValue
  , setGatePnormValue
  , flipOutputGates
  , setFlipOutputGates
  , createGRUDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector
  , inputGateInputWeightsSelector
  , setInputGateInputWeightsSelector
  , inputGateRecurrentWeightsSelector
  , setInputGateRecurrentWeightsSelector
  , recurrentGateInputWeightsSelector
  , setRecurrentGateInputWeightsSelector
  , recurrentGateRecurrentWeightsSelector
  , setRecurrentGateRecurrentWeightsSelector
  , outputGateInputWeightsSelector
  , setOutputGateInputWeightsSelector
  , outputGateRecurrentWeightsSelector
  , setOutputGateRecurrentWeightsSelector
  , outputGateInputGateWeightsSelector
  , setOutputGateInputGateWeightsSelector
  , gatePnormValueSelector
  , setGatePnormValueSelector
  , flipOutputGatesSelector
  , setFlipOutputGatesSelector


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

-- | Creates a GRU descriptor.
--
-- @inputFeatureChannels@ — The number of feature channels in the input image/matrix. Must be >= 1.
--
-- @outputFeatureChannels@ — The number of feature channels in the output image/matrix. Must be >= 1.
--
-- Returns: A valid MPSGRUDescriptor object or nil, if failure.
--
-- ObjC selector: @+ createGRUDescriptorWithInputFeatureChannels:outputFeatureChannels:@
createGRUDescriptorWithInputFeatureChannels_outputFeatureChannels :: CULong -> CULong -> IO (Id MPSGRUDescriptor)
createGRUDescriptorWithInputFeatureChannels_outputFeatureChannels inputFeatureChannels outputFeatureChannels =
  do
    cls' <- getRequiredClass "MPSGRUDescriptor"
    sendClassMsg cls' (mkSelector "createGRUDescriptorWithInputFeatureChannels:outputFeatureChannels:") (retPtr retVoid) [argCULong inputFeatureChannels, argCULong outputFeatureChannels] >>= retainedObject . castPtr

-- | inputGateInputWeights
--
-- Contains weights 'Wz_ij', bias 'bz_i' and neuron 'gz' from the GRU formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- inputGateInputWeights@
inputGateInputWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO RawId
inputGateInputWeights mpsgruDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsgruDescriptor (mkSelector "inputGateInputWeights") (retPtr retVoid) []

-- | inputGateInputWeights
--
-- Contains weights 'Wz_ij', bias 'bz_i' and neuron 'gz' from the GRU formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- setInputGateInputWeights:@
setInputGateInputWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> RawId -> IO ()
setInputGateInputWeights mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setInputGateInputWeights:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | inputGateRecurrentWeights
--
-- Contains weights 'Uz_ij' from the GRU formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- inputGateRecurrentWeights@
inputGateRecurrentWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO RawId
inputGateRecurrentWeights mpsgruDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsgruDescriptor (mkSelector "inputGateRecurrentWeights") (retPtr retVoid) []

-- | inputGateRecurrentWeights
--
-- Contains weights 'Uz_ij' from the GRU formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setInputGateRecurrentWeights:@
setInputGateRecurrentWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> RawId -> IO ()
setInputGateRecurrentWeights mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setInputGateRecurrentWeights:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | recurrentGateInputWeights
--
-- Contains weights 'Wr_ij', bias 'br_i' and neuron 'gr' from the GRU formula.              If nil then assumed zero weights, bias and no neuron (identity mapping).Defaults to nil.
--
-- ObjC selector: @- recurrentGateInputWeights@
recurrentGateInputWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO RawId
recurrentGateInputWeights mpsgruDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsgruDescriptor (mkSelector "recurrentGateInputWeights") (retPtr retVoid) []

-- | recurrentGateInputWeights
--
-- Contains weights 'Wr_ij', bias 'br_i' and neuron 'gr' from the GRU formula.              If nil then assumed zero weights, bias and no neuron (identity mapping).Defaults to nil.
--
-- ObjC selector: @- setRecurrentGateInputWeights:@
setRecurrentGateInputWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> RawId -> IO ()
setRecurrentGateInputWeights mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setRecurrentGateInputWeights:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | recurrentGateRecurrentWeights
--
-- Contains weights 'Ur_ij' from the GRU formula.              If nil then assumed zero weights.Defaults to nil.
--
-- ObjC selector: @- recurrentGateRecurrentWeights@
recurrentGateRecurrentWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO RawId
recurrentGateRecurrentWeights mpsgruDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsgruDescriptor (mkSelector "recurrentGateRecurrentWeights") (retPtr retVoid) []

-- | recurrentGateRecurrentWeights
--
-- Contains weights 'Ur_ij' from the GRU formula.              If nil then assumed zero weights.Defaults to nil.
--
-- ObjC selector: @- setRecurrentGateRecurrentWeights:@
setRecurrentGateRecurrentWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> RawId -> IO ()
setRecurrentGateRecurrentWeights mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setRecurrentGateRecurrentWeights:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | outputGateInputWeights
--
-- Contains weights 'Wh_ij', bias 'bh_i' and neuron 'gh' from the GRU formula.              If nil then assumed zero weights, bias and no neuron (identity mapping).Defaults to nil.
--
-- ObjC selector: @- outputGateInputWeights@
outputGateInputWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO RawId
outputGateInputWeights mpsgruDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsgruDescriptor (mkSelector "outputGateInputWeights") (retPtr retVoid) []

-- | outputGateInputWeights
--
-- Contains weights 'Wh_ij', bias 'bh_i' and neuron 'gh' from the GRU formula.              If nil then assumed zero weights, bias and no neuron (identity mapping).Defaults to nil.
--
-- ObjC selector: @- setOutputGateInputWeights:@
setOutputGateInputWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> RawId -> IO ()
setOutputGateInputWeights mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setOutputGateInputWeights:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | outputGateRecurrentWeights
--
-- Contains weights 'Uh_ij' from the GRU formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- outputGateRecurrentWeights@
outputGateRecurrentWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO RawId
outputGateRecurrentWeights mpsgruDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsgruDescriptor (mkSelector "outputGateRecurrentWeights") (retPtr retVoid) []

-- | outputGateRecurrentWeights
--
-- Contains weights 'Uh_ij' from the GRU formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setOutputGateRecurrentWeights:@
setOutputGateRecurrentWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> RawId -> IO ()
setOutputGateRecurrentWeights mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setOutputGateRecurrentWeights:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | outputGateInputGateWeights
--
-- Contains weights 'Vh_ij' - can be used to implement the "Minimally Gated Unit".              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- outputGateInputGateWeights@
outputGateInputGateWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO RawId
outputGateInputGateWeights mpsgruDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mpsgruDescriptor (mkSelector "outputGateInputGateWeights") (retPtr retVoid) []

-- | outputGateInputGateWeights
--
-- Contains weights 'Vh_ij' - can be used to implement the "Minimally Gated Unit".              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setOutputGateInputGateWeights:@
setOutputGateInputGateWeights :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> RawId -> IO ()
setOutputGateInputGateWeights mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setOutputGateInputGateWeights:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | gatePnormValue
--
-- The p-norm gating norm value as specified by the GRU formulae. Defaults to 1.0f.
--
-- ObjC selector: @- gatePnormValue@
gatePnormValue :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO CFloat
gatePnormValue mpsgruDescriptor  =
    sendMsg mpsgruDescriptor (mkSelector "gatePnormValue") retCFloat []

-- | gatePnormValue
--
-- The p-norm gating norm value as specified by the GRU formulae. Defaults to 1.0f.
--
-- ObjC selector: @- setGatePnormValue:@
setGatePnormValue :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> CFloat -> IO ()
setGatePnormValue mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setGatePnormValue:") retVoid [argCFloat value]

-- | flipOutputGates
--
-- If YES then the GRU-block output formula is changed to:                  h1_i = ( 1 - z_i ^ p)^(1/p) h0_i + z_i h_i.              Defaults to NO.
--
-- ObjC selector: @- flipOutputGates@
flipOutputGates :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> IO Bool
flipOutputGates mpsgruDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsgruDescriptor (mkSelector "flipOutputGates") retCULong []

-- | flipOutputGates
--
-- If YES then the GRU-block output formula is changed to:                  h1_i = ( 1 - z_i ^ p)^(1/p) h0_i + z_i h_i.              Defaults to NO.
--
-- ObjC selector: @- setFlipOutputGates:@
setFlipOutputGates :: IsMPSGRUDescriptor mpsgruDescriptor => mpsgruDescriptor -> Bool -> IO ()
setFlipOutputGates mpsgruDescriptor  value =
    sendMsg mpsgruDescriptor (mkSelector "setFlipOutputGates:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createGRUDescriptorWithInputFeatureChannels:outputFeatureChannels:@
createGRUDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector :: Selector
createGRUDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector = mkSelector "createGRUDescriptorWithInputFeatureChannels:outputFeatureChannels:"

-- | @Selector@ for @inputGateInputWeights@
inputGateInputWeightsSelector :: Selector
inputGateInputWeightsSelector = mkSelector "inputGateInputWeights"

-- | @Selector@ for @setInputGateInputWeights:@
setInputGateInputWeightsSelector :: Selector
setInputGateInputWeightsSelector = mkSelector "setInputGateInputWeights:"

-- | @Selector@ for @inputGateRecurrentWeights@
inputGateRecurrentWeightsSelector :: Selector
inputGateRecurrentWeightsSelector = mkSelector "inputGateRecurrentWeights"

-- | @Selector@ for @setInputGateRecurrentWeights:@
setInputGateRecurrentWeightsSelector :: Selector
setInputGateRecurrentWeightsSelector = mkSelector "setInputGateRecurrentWeights:"

-- | @Selector@ for @recurrentGateInputWeights@
recurrentGateInputWeightsSelector :: Selector
recurrentGateInputWeightsSelector = mkSelector "recurrentGateInputWeights"

-- | @Selector@ for @setRecurrentGateInputWeights:@
setRecurrentGateInputWeightsSelector :: Selector
setRecurrentGateInputWeightsSelector = mkSelector "setRecurrentGateInputWeights:"

-- | @Selector@ for @recurrentGateRecurrentWeights@
recurrentGateRecurrentWeightsSelector :: Selector
recurrentGateRecurrentWeightsSelector = mkSelector "recurrentGateRecurrentWeights"

-- | @Selector@ for @setRecurrentGateRecurrentWeights:@
setRecurrentGateRecurrentWeightsSelector :: Selector
setRecurrentGateRecurrentWeightsSelector = mkSelector "setRecurrentGateRecurrentWeights:"

-- | @Selector@ for @outputGateInputWeights@
outputGateInputWeightsSelector :: Selector
outputGateInputWeightsSelector = mkSelector "outputGateInputWeights"

-- | @Selector@ for @setOutputGateInputWeights:@
setOutputGateInputWeightsSelector :: Selector
setOutputGateInputWeightsSelector = mkSelector "setOutputGateInputWeights:"

-- | @Selector@ for @outputGateRecurrentWeights@
outputGateRecurrentWeightsSelector :: Selector
outputGateRecurrentWeightsSelector = mkSelector "outputGateRecurrentWeights"

-- | @Selector@ for @setOutputGateRecurrentWeights:@
setOutputGateRecurrentWeightsSelector :: Selector
setOutputGateRecurrentWeightsSelector = mkSelector "setOutputGateRecurrentWeights:"

-- | @Selector@ for @outputGateInputGateWeights@
outputGateInputGateWeightsSelector :: Selector
outputGateInputGateWeightsSelector = mkSelector "outputGateInputGateWeights"

-- | @Selector@ for @setOutputGateInputGateWeights:@
setOutputGateInputGateWeightsSelector :: Selector
setOutputGateInputGateWeightsSelector = mkSelector "setOutputGateInputGateWeights:"

-- | @Selector@ for @gatePnormValue@
gatePnormValueSelector :: Selector
gatePnormValueSelector = mkSelector "gatePnormValue"

-- | @Selector@ for @setGatePnormValue:@
setGatePnormValueSelector :: Selector
setGatePnormValueSelector = mkSelector "setGatePnormValue:"

-- | @Selector@ for @flipOutputGates@
flipOutputGatesSelector :: Selector
flipOutputGatesSelector = mkSelector "flipOutputGates"

-- | @Selector@ for @setFlipOutputGates:@
setFlipOutputGatesSelector :: Selector
setFlipOutputGatesSelector = mkSelector "setFlipOutputGates:"

