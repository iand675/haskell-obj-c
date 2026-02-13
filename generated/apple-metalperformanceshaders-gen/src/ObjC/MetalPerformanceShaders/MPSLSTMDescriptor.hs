{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSLSTMDescriptor
--
-- This depends on Metal.framework
--
-- The MPSLSTMDescriptor specifies a LSTM block/layer descriptor.              The RNN layer initialized with a MPSLSTMDescriptor transforms the input data (image or matrix),              the memory cell data and previous output with a set of filters, each producing one feature map in              the output data and memory cell, according to the LSTM formulae detailed below.              The user may provide the LSTM unit a single input or a sequence of inputs.
--
-- Description of operation:
--
-- Let x_j be the input data (at time index t of sequence,                          j index containing quadruplet: batch index, x,y and feature index (x=y=0 for matrices)).              Let h0_j be the recurrent input (previous output) data from previous time step (at time index t-1 of sequence).              Let h1_i be the output data produced at this time step.              Let c0_j be the previous memory cell data (at time index t-1 of sequence).              Let c1_i be the new memory cell data (at time index t-1 of sequence).
--
-- Let Wi_ij, Ui_ij, Vi_ij, be the input gate weights for input, recurrent input and memory cell (peephole) data respectively              Let bi_i be the bias for the input gate
--
-- Let Wf_ij, Uf_ij, Vf_ij, be the forget gate weights for input, recurrent input and memory cell data respectively              Let bf_i be the bias for the forget gate
--
-- Let Wo_ij, Uo_ij, Vo_ij, be the output gate weights for input, recurrent input and memory cell data respectively              Let bo_i be the bias for the output gate
--
-- Let Wc_ij, Uc_ij, Vc_ij, be the memory cell gate weights for input, recurrent input and memory cell data respectively              Let bc_i be the bias for the memory cell gate
--
-- Let gi(x), gf(x), go(x), gc(x) be neuron activation function for the input, forget, output gate and memory cell gate              Let gh(x) be the activation function applied to result memory cell data
--
-- Then the new memory cell data c1_j and output image h1_i are computed as follows:
--
-- I_i = gi(  Wi_ij * x_j  +  Ui_ij * h0_j  +  Vi_ij * c0_j  + bi_i  )                      F_i = gf(  Wf_ij * x_j  +  Uf_ij * h0_j  +  Vf_ij * c0_j  + bf_i  )                      C_i = gc(  Wc_ij * x_j  +  Uc_ij * h0_j  +  Vc_ij * c0_j  + bc_i  )
--
-- c1_i = F_i c0_i  +  I_i C_i
--
-- O_i = go(  Wo_ij * x_j  +  Uo_ij * h0_j  +  Vo_ij * c1_j  + bo_i  )
--
-- h1_i = O_i gh( c1_i )
--
-- The '*' stands for convolution (see MPSRNNImageInferenceLayer) or matrix-vector/matrix multiplication              (see MPSRNNMatrixInferenceLayer).              Summation is over index j (except for the batch index), but there is no summation over              repeated index i - the output index.              Note that for validity all intermediate images have to be of same size and all U and V matrices have to be square              (ie. outputFeatureChannels == inputFeatureChannels in those). Also the bias terms are scalars wrt. spatial dimensions.
--
-- Generated bindings for @MPSLSTMDescriptor@.
module ObjC.MetalPerformanceShaders.MPSLSTMDescriptor
  ( MPSLSTMDescriptor
  , IsMPSLSTMDescriptor(..)
  , createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannels
  , memoryWeightsAreDiagonal
  , setMemoryWeightsAreDiagonal
  , inputGateInputWeights
  , setInputGateInputWeights
  , inputGateRecurrentWeights
  , setInputGateRecurrentWeights
  , inputGateMemoryWeights
  , setInputGateMemoryWeights
  , forgetGateInputWeights
  , setForgetGateInputWeights
  , forgetGateRecurrentWeights
  , setForgetGateRecurrentWeights
  , forgetGateMemoryWeights
  , setForgetGateMemoryWeights
  , outputGateInputWeights
  , setOutputGateInputWeights
  , outputGateRecurrentWeights
  , setOutputGateRecurrentWeights
  , outputGateMemoryWeights
  , setOutputGateMemoryWeights
  , cellGateInputWeights
  , setCellGateInputWeights
  , cellGateRecurrentWeights
  , setCellGateRecurrentWeights
  , cellGateMemoryWeights
  , setCellGateMemoryWeights
  , cellToOutputNeuronType
  , setCellToOutputNeuronType
  , cellToOutputNeuronParamA
  , setCellToOutputNeuronParamA
  , cellToOutputNeuronParamB
  , setCellToOutputNeuronParamB
  , cellToOutputNeuronParamC
  , setCellToOutputNeuronParamC
  , cellGateInputWeightsSelector
  , cellGateMemoryWeightsSelector
  , cellGateRecurrentWeightsSelector
  , cellToOutputNeuronParamASelector
  , cellToOutputNeuronParamBSelector
  , cellToOutputNeuronParamCSelector
  , cellToOutputNeuronTypeSelector
  , createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector
  , forgetGateInputWeightsSelector
  , forgetGateMemoryWeightsSelector
  , forgetGateRecurrentWeightsSelector
  , inputGateInputWeightsSelector
  , inputGateMemoryWeightsSelector
  , inputGateRecurrentWeightsSelector
  , memoryWeightsAreDiagonalSelector
  , outputGateInputWeightsSelector
  , outputGateMemoryWeightsSelector
  , outputGateRecurrentWeightsSelector
  , setCellGateInputWeightsSelector
  , setCellGateMemoryWeightsSelector
  , setCellGateRecurrentWeightsSelector
  , setCellToOutputNeuronParamASelector
  , setCellToOutputNeuronParamBSelector
  , setCellToOutputNeuronParamCSelector
  , setCellToOutputNeuronTypeSelector
  , setForgetGateInputWeightsSelector
  , setForgetGateMemoryWeightsSelector
  , setForgetGateRecurrentWeightsSelector
  , setInputGateInputWeightsSelector
  , setInputGateMemoryWeightsSelector
  , setInputGateRecurrentWeightsSelector
  , setMemoryWeightsAreDiagonalSelector
  , setOutputGateInputWeightsSelector
  , setOutputGateMemoryWeightsSelector
  , setOutputGateRecurrentWeightsSelector

  -- * Enum types
  , MPSCNNNeuronType(MPSCNNNeuronType)
  , pattern MPSCNNNeuronTypeNone
  , pattern MPSCNNNeuronTypeReLU
  , pattern MPSCNNNeuronTypeLinear
  , pattern MPSCNNNeuronTypeSigmoid
  , pattern MPSCNNNeuronTypeHardSigmoid
  , pattern MPSCNNNeuronTypeTanH
  , pattern MPSCNNNeuronTypeAbsolute
  , pattern MPSCNNNeuronTypeSoftPlus
  , pattern MPSCNNNeuronTypeSoftSign
  , pattern MPSCNNNeuronTypeELU
  , pattern MPSCNNNeuronTypePReLU
  , pattern MPSCNNNeuronTypeReLUN
  , pattern MPSCNNNeuronTypePower
  , pattern MPSCNNNeuronTypeExponential
  , pattern MPSCNNNeuronTypeLogarithm
  , pattern MPSCNNNeuronTypeGeLU
  , pattern MPSCNNNeuronTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a LSTM descriptor.
--
-- @inputFeatureChannels@ — The number of feature channels in the input image/matrix. Must be >= 1.
--
-- @outputFeatureChannels@ — The number of feature channels in the output image/matrix. Must be >= 1.
--
-- Returns: A valid MPSNNLSTMDescriptor object or nil, if failure.
--
-- ObjC selector: @+ createLSTMDescriptorWithInputFeatureChannels:outputFeatureChannels:@
createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannels :: CULong -> CULong -> IO (Id MPSLSTMDescriptor)
createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannels inputFeatureChannels outputFeatureChannels =
  do
    cls' <- getRequiredClass "MPSLSTMDescriptor"
    sendClassMessage cls' createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector inputFeatureChannels outputFeatureChannels

-- | memoryWeightsAreDiagonal
--
-- If YES, then the 'peephole' weight matrices will be diagonal matrices represented as              vectors of length the number of features in memory cells, that will be multiplied pointwise              with the peephole matrix or image in order to achieve the diagonal (nonmixing) update.              Defaults to NO.
--
-- ObjC selector: @- memoryWeightsAreDiagonal@
memoryWeightsAreDiagonal :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO Bool
memoryWeightsAreDiagonal mpslstmDescriptor =
  sendMessage mpslstmDescriptor memoryWeightsAreDiagonalSelector

-- | memoryWeightsAreDiagonal
--
-- If YES, then the 'peephole' weight matrices will be diagonal matrices represented as              vectors of length the number of features in memory cells, that will be multiplied pointwise              with the peephole matrix or image in order to achieve the diagonal (nonmixing) update.              Defaults to NO.
--
-- ObjC selector: @- setMemoryWeightsAreDiagonal:@
setMemoryWeightsAreDiagonal :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> Bool -> IO ()
setMemoryWeightsAreDiagonal mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setMemoryWeightsAreDiagonalSelector value

-- | inputGateInputWeights
--
-- Contains weights 'Wi_ij', bias 'bi_i' and neuron 'gi' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- inputGateInputWeights@
inputGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
inputGateInputWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor inputGateInputWeightsSelector

-- | inputGateInputWeights
--
-- Contains weights 'Wi_ij', bias 'bi_i' and neuron 'gi' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- setInputGateInputWeights:@
setInputGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setInputGateInputWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setInputGateInputWeightsSelector value

-- | inputGateRecurrentWeights
--
-- Contains weights 'Ui_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- inputGateRecurrentWeights@
inputGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
inputGateRecurrentWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor inputGateRecurrentWeightsSelector

-- | inputGateRecurrentWeights
--
-- Contains weights 'Ui_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setInputGateRecurrentWeights:@
setInputGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setInputGateRecurrentWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setInputGateRecurrentWeightsSelector value

-- | inputGateMemoryWeights
--
-- Contains weights 'Vi_ij' - the 'peephole' weights - from the LSTM formula.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- inputGateMemoryWeights@
inputGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
inputGateMemoryWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor inputGateMemoryWeightsSelector

-- | inputGateMemoryWeights
--
-- Contains weights 'Vi_ij' - the 'peephole' weights - from the LSTM formula.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setInputGateMemoryWeights:@
setInputGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setInputGateMemoryWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setInputGateMemoryWeightsSelector value

-- | forgetGateInputWeights
--
-- Contains weights 'Wf_ij', bias 'bf_i' and neuron 'gf' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping).Defaults to nil.
--
-- ObjC selector: @- forgetGateInputWeights@
forgetGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
forgetGateInputWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor forgetGateInputWeightsSelector

-- | forgetGateInputWeights
--
-- Contains weights 'Wf_ij', bias 'bf_i' and neuron 'gf' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping).Defaults to nil.
--
-- ObjC selector: @- setForgetGateInputWeights:@
setForgetGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setForgetGateInputWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setForgetGateInputWeightsSelector value

-- | forgetGateRecurrentWeights
--
-- Contains weights 'Uf_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- forgetGateRecurrentWeights@
forgetGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
forgetGateRecurrentWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor forgetGateRecurrentWeightsSelector

-- | forgetGateRecurrentWeights
--
-- Contains weights 'Uf_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setForgetGateRecurrentWeights:@
setForgetGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setForgetGateRecurrentWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setForgetGateRecurrentWeightsSelector value

-- | forgetGateMemoryWeights
--
-- Contains weights 'Vf_ij' - the 'peephole' weights - from the LSTM formula.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- forgetGateMemoryWeights@
forgetGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
forgetGateMemoryWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor forgetGateMemoryWeightsSelector

-- | forgetGateMemoryWeights
--
-- Contains weights 'Vf_ij' - the 'peephole' weights - from the LSTM formula.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setForgetGateMemoryWeights:@
setForgetGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setForgetGateMemoryWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setForgetGateMemoryWeightsSelector value

-- | outputGateInputWeights
--
-- Contains weights 'Wo_ij', bias 'bo_i' and neuron 'go' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- outputGateInputWeights@
outputGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
outputGateInputWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor outputGateInputWeightsSelector

-- | outputGateInputWeights
--
-- Contains weights 'Wo_ij', bias 'bo_i' and neuron 'go' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- setOutputGateInputWeights:@
setOutputGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setOutputGateInputWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setOutputGateInputWeightsSelector value

-- | outputGateRecurrentWeights
--
-- Contains weights 'Uo_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- outputGateRecurrentWeights@
outputGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
outputGateRecurrentWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor outputGateRecurrentWeightsSelector

-- | outputGateRecurrentWeights
--
-- Contains weights 'Uo_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setOutputGateRecurrentWeights:@
setOutputGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setOutputGateRecurrentWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setOutputGateRecurrentWeightsSelector value

-- | outputGateMemoryWeights
--
-- Contains weights 'Vo_ij' - the 'peephole' weights - from the LSTM.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- outputGateMemoryWeights@
outputGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
outputGateMemoryWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor outputGateMemoryWeightsSelector

-- | outputGateMemoryWeights
--
-- Contains weights 'Vo_ij' - the 'peephole' weights - from the LSTM.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setOutputGateMemoryWeights:@
setOutputGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setOutputGateMemoryWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setOutputGateMemoryWeightsSelector value

-- | cellGateInputWeights
--
-- Contains weights 'Wc_ij', bias 'bc_i' and neuron 'gc' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- cellGateInputWeights@
cellGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
cellGateInputWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor cellGateInputWeightsSelector

-- | cellGateInputWeights
--
-- Contains weights 'Wc_ij', bias 'bc_i' and neuron 'gc' from the LSTM formula.              If nil then assumed zero weights, bias and no neuron (identity mapping). Defaults to nil.
--
-- ObjC selector: @- setCellGateInputWeights:@
setCellGateInputWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setCellGateInputWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setCellGateInputWeightsSelector value

-- | cellGateRecurrentWeights
--
-- Contains weights 'Uc_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- cellGateRecurrentWeights@
cellGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
cellGateRecurrentWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor cellGateRecurrentWeightsSelector

-- | cellGateRecurrentWeights
--
-- Contains weights 'Uc_ij' from the LSTM formula.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setCellGateRecurrentWeights:@
setCellGateRecurrentWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setCellGateRecurrentWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setCellGateRecurrentWeightsSelector value

-- | cellGateMemoryWeights
--
-- Contains weights 'Vc_ij' - the 'peephole' weights - from the LSTM formula.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- cellGateMemoryWeights@
cellGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO RawId
cellGateMemoryWeights mpslstmDescriptor =
  sendMessage mpslstmDescriptor cellGateMemoryWeightsSelector

-- | cellGateMemoryWeights
--
-- Contains weights 'Vc_ij' - the 'peephole' weights - from the LSTM formula.              if YES == memoryWeightsAreDiagonal, then the number of weights used is the number of features                  in the memory cell image/matrix.              If nil then assumed zero weights. Defaults to nil.
--
-- ObjC selector: @- setCellGateMemoryWeights:@
setCellGateMemoryWeights :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> RawId -> IO ()
setCellGateMemoryWeights mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setCellGateMemoryWeightsSelector value

-- | cellToOutputNeuronType
--
-- Neuron type definition for 'gh', see MPSCNNNeuronType. Defaults to MPSCNNNeuronTypeTanH.
--
-- ObjC selector: @- cellToOutputNeuronType@
cellToOutputNeuronType :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO MPSCNNNeuronType
cellToOutputNeuronType mpslstmDescriptor =
  sendMessage mpslstmDescriptor cellToOutputNeuronTypeSelector

-- | cellToOutputNeuronType
--
-- Neuron type definition for 'gh', see MPSCNNNeuronType. Defaults to MPSCNNNeuronTypeTanH.
--
-- ObjC selector: @- setCellToOutputNeuronType:@
setCellToOutputNeuronType :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> MPSCNNNeuronType -> IO ()
setCellToOutputNeuronType mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setCellToOutputNeuronTypeSelector value

-- | cellToOutputNeuronParamA
--
-- Neuron parameter A for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- cellToOutputNeuronParamA@
cellToOutputNeuronParamA :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO CFloat
cellToOutputNeuronParamA mpslstmDescriptor =
  sendMessage mpslstmDescriptor cellToOutputNeuronParamASelector

-- | cellToOutputNeuronParamA
--
-- Neuron parameter A for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- setCellToOutputNeuronParamA:@
setCellToOutputNeuronParamA :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> CFloat -> IO ()
setCellToOutputNeuronParamA mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setCellToOutputNeuronParamASelector value

-- | cellToOutputNeuronParamB
--
-- Neuron parameter B for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- cellToOutputNeuronParamB@
cellToOutputNeuronParamB :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO CFloat
cellToOutputNeuronParamB mpslstmDescriptor =
  sendMessage mpslstmDescriptor cellToOutputNeuronParamBSelector

-- | cellToOutputNeuronParamB
--
-- Neuron parameter B for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- setCellToOutputNeuronParamB:@
setCellToOutputNeuronParamB :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> CFloat -> IO ()
setCellToOutputNeuronParamB mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setCellToOutputNeuronParamBSelector value

-- | cellToOutputNeuronParamC
--
-- Neuron parameter C for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- cellToOutputNeuronParamC@
cellToOutputNeuronParamC :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO CFloat
cellToOutputNeuronParamC mpslstmDescriptor =
  sendMessage mpslstmDescriptor cellToOutputNeuronParamCSelector

-- | cellToOutputNeuronParamC
--
-- Neuron parameter C for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- setCellToOutputNeuronParamC:@
setCellToOutputNeuronParamC :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> CFloat -> IO ()
setCellToOutputNeuronParamC mpslstmDescriptor value =
  sendMessage mpslstmDescriptor setCellToOutputNeuronParamCSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createLSTMDescriptorWithInputFeatureChannels:outputFeatureChannels:@
createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector :: Selector '[CULong, CULong] (Id MPSLSTMDescriptor)
createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector = mkSelector "createLSTMDescriptorWithInputFeatureChannels:outputFeatureChannels:"

-- | @Selector@ for @memoryWeightsAreDiagonal@
memoryWeightsAreDiagonalSelector :: Selector '[] Bool
memoryWeightsAreDiagonalSelector = mkSelector "memoryWeightsAreDiagonal"

-- | @Selector@ for @setMemoryWeightsAreDiagonal:@
setMemoryWeightsAreDiagonalSelector :: Selector '[Bool] ()
setMemoryWeightsAreDiagonalSelector = mkSelector "setMemoryWeightsAreDiagonal:"

-- | @Selector@ for @inputGateInputWeights@
inputGateInputWeightsSelector :: Selector '[] RawId
inputGateInputWeightsSelector = mkSelector "inputGateInputWeights"

-- | @Selector@ for @setInputGateInputWeights:@
setInputGateInputWeightsSelector :: Selector '[RawId] ()
setInputGateInputWeightsSelector = mkSelector "setInputGateInputWeights:"

-- | @Selector@ for @inputGateRecurrentWeights@
inputGateRecurrentWeightsSelector :: Selector '[] RawId
inputGateRecurrentWeightsSelector = mkSelector "inputGateRecurrentWeights"

-- | @Selector@ for @setInputGateRecurrentWeights:@
setInputGateRecurrentWeightsSelector :: Selector '[RawId] ()
setInputGateRecurrentWeightsSelector = mkSelector "setInputGateRecurrentWeights:"

-- | @Selector@ for @inputGateMemoryWeights@
inputGateMemoryWeightsSelector :: Selector '[] RawId
inputGateMemoryWeightsSelector = mkSelector "inputGateMemoryWeights"

-- | @Selector@ for @setInputGateMemoryWeights:@
setInputGateMemoryWeightsSelector :: Selector '[RawId] ()
setInputGateMemoryWeightsSelector = mkSelector "setInputGateMemoryWeights:"

-- | @Selector@ for @forgetGateInputWeights@
forgetGateInputWeightsSelector :: Selector '[] RawId
forgetGateInputWeightsSelector = mkSelector "forgetGateInputWeights"

-- | @Selector@ for @setForgetGateInputWeights:@
setForgetGateInputWeightsSelector :: Selector '[RawId] ()
setForgetGateInputWeightsSelector = mkSelector "setForgetGateInputWeights:"

-- | @Selector@ for @forgetGateRecurrentWeights@
forgetGateRecurrentWeightsSelector :: Selector '[] RawId
forgetGateRecurrentWeightsSelector = mkSelector "forgetGateRecurrentWeights"

-- | @Selector@ for @setForgetGateRecurrentWeights:@
setForgetGateRecurrentWeightsSelector :: Selector '[RawId] ()
setForgetGateRecurrentWeightsSelector = mkSelector "setForgetGateRecurrentWeights:"

-- | @Selector@ for @forgetGateMemoryWeights@
forgetGateMemoryWeightsSelector :: Selector '[] RawId
forgetGateMemoryWeightsSelector = mkSelector "forgetGateMemoryWeights"

-- | @Selector@ for @setForgetGateMemoryWeights:@
setForgetGateMemoryWeightsSelector :: Selector '[RawId] ()
setForgetGateMemoryWeightsSelector = mkSelector "setForgetGateMemoryWeights:"

-- | @Selector@ for @outputGateInputWeights@
outputGateInputWeightsSelector :: Selector '[] RawId
outputGateInputWeightsSelector = mkSelector "outputGateInputWeights"

-- | @Selector@ for @setOutputGateInputWeights:@
setOutputGateInputWeightsSelector :: Selector '[RawId] ()
setOutputGateInputWeightsSelector = mkSelector "setOutputGateInputWeights:"

-- | @Selector@ for @outputGateRecurrentWeights@
outputGateRecurrentWeightsSelector :: Selector '[] RawId
outputGateRecurrentWeightsSelector = mkSelector "outputGateRecurrentWeights"

-- | @Selector@ for @setOutputGateRecurrentWeights:@
setOutputGateRecurrentWeightsSelector :: Selector '[RawId] ()
setOutputGateRecurrentWeightsSelector = mkSelector "setOutputGateRecurrentWeights:"

-- | @Selector@ for @outputGateMemoryWeights@
outputGateMemoryWeightsSelector :: Selector '[] RawId
outputGateMemoryWeightsSelector = mkSelector "outputGateMemoryWeights"

-- | @Selector@ for @setOutputGateMemoryWeights:@
setOutputGateMemoryWeightsSelector :: Selector '[RawId] ()
setOutputGateMemoryWeightsSelector = mkSelector "setOutputGateMemoryWeights:"

-- | @Selector@ for @cellGateInputWeights@
cellGateInputWeightsSelector :: Selector '[] RawId
cellGateInputWeightsSelector = mkSelector "cellGateInputWeights"

-- | @Selector@ for @setCellGateInputWeights:@
setCellGateInputWeightsSelector :: Selector '[RawId] ()
setCellGateInputWeightsSelector = mkSelector "setCellGateInputWeights:"

-- | @Selector@ for @cellGateRecurrentWeights@
cellGateRecurrentWeightsSelector :: Selector '[] RawId
cellGateRecurrentWeightsSelector = mkSelector "cellGateRecurrentWeights"

-- | @Selector@ for @setCellGateRecurrentWeights:@
setCellGateRecurrentWeightsSelector :: Selector '[RawId] ()
setCellGateRecurrentWeightsSelector = mkSelector "setCellGateRecurrentWeights:"

-- | @Selector@ for @cellGateMemoryWeights@
cellGateMemoryWeightsSelector :: Selector '[] RawId
cellGateMemoryWeightsSelector = mkSelector "cellGateMemoryWeights"

-- | @Selector@ for @setCellGateMemoryWeights:@
setCellGateMemoryWeightsSelector :: Selector '[RawId] ()
setCellGateMemoryWeightsSelector = mkSelector "setCellGateMemoryWeights:"

-- | @Selector@ for @cellToOutputNeuronType@
cellToOutputNeuronTypeSelector :: Selector '[] MPSCNNNeuronType
cellToOutputNeuronTypeSelector = mkSelector "cellToOutputNeuronType"

-- | @Selector@ for @setCellToOutputNeuronType:@
setCellToOutputNeuronTypeSelector :: Selector '[MPSCNNNeuronType] ()
setCellToOutputNeuronTypeSelector = mkSelector "setCellToOutputNeuronType:"

-- | @Selector@ for @cellToOutputNeuronParamA@
cellToOutputNeuronParamASelector :: Selector '[] CFloat
cellToOutputNeuronParamASelector = mkSelector "cellToOutputNeuronParamA"

-- | @Selector@ for @setCellToOutputNeuronParamA:@
setCellToOutputNeuronParamASelector :: Selector '[CFloat] ()
setCellToOutputNeuronParamASelector = mkSelector "setCellToOutputNeuronParamA:"

-- | @Selector@ for @cellToOutputNeuronParamB@
cellToOutputNeuronParamBSelector :: Selector '[] CFloat
cellToOutputNeuronParamBSelector = mkSelector "cellToOutputNeuronParamB"

-- | @Selector@ for @setCellToOutputNeuronParamB:@
setCellToOutputNeuronParamBSelector :: Selector '[CFloat] ()
setCellToOutputNeuronParamBSelector = mkSelector "setCellToOutputNeuronParamB:"

-- | @Selector@ for @cellToOutputNeuronParamC@
cellToOutputNeuronParamCSelector :: Selector '[] CFloat
cellToOutputNeuronParamCSelector = mkSelector "cellToOutputNeuronParamC"

-- | @Selector@ for @setCellToOutputNeuronParamC:@
setCellToOutputNeuronParamCSelector :: Selector '[CFloat] ()
setCellToOutputNeuronParamCSelector = mkSelector "setCellToOutputNeuronParamC:"

