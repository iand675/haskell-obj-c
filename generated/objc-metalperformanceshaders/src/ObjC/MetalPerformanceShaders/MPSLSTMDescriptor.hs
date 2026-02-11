{-# LANGUAGE PatternSynonyms #-}
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
  , cellToOutputNeuronType
  , setCellToOutputNeuronType
  , cellToOutputNeuronParamA
  , setCellToOutputNeuronParamA
  , cellToOutputNeuronParamB
  , setCellToOutputNeuronParamB
  , cellToOutputNeuronParamC
  , setCellToOutputNeuronParamC
  , createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector
  , memoryWeightsAreDiagonalSelector
  , setMemoryWeightsAreDiagonalSelector
  , cellToOutputNeuronTypeSelector
  , setCellToOutputNeuronTypeSelector
  , cellToOutputNeuronParamASelector
  , setCellToOutputNeuronParamASelector
  , cellToOutputNeuronParamBSelector
  , setCellToOutputNeuronParamBSelector
  , cellToOutputNeuronParamCSelector
  , setCellToOutputNeuronParamCSelector

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
    sendClassMsg cls' (mkSelector "createLSTMDescriptorWithInputFeatureChannels:outputFeatureChannels:") (retPtr retVoid) [argCULong (fromIntegral inputFeatureChannels), argCULong (fromIntegral outputFeatureChannels)] >>= retainedObject . castPtr

-- | memoryWeightsAreDiagonal
--
-- If YES, then the 'peephole' weight matrices will be diagonal matrices represented as              vectors of length the number of features in memory cells, that will be multiplied pointwise              with the peephole matrix or image in order to achieve the diagonal (nonmixing) update.              Defaults to NO.
--
-- ObjC selector: @- memoryWeightsAreDiagonal@
memoryWeightsAreDiagonal :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO Bool
memoryWeightsAreDiagonal mpslstmDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpslstmDescriptor (mkSelector "memoryWeightsAreDiagonal") retCULong []

-- | memoryWeightsAreDiagonal
--
-- If YES, then the 'peephole' weight matrices will be diagonal matrices represented as              vectors of length the number of features in memory cells, that will be multiplied pointwise              with the peephole matrix or image in order to achieve the diagonal (nonmixing) update.              Defaults to NO.
--
-- ObjC selector: @- setMemoryWeightsAreDiagonal:@
setMemoryWeightsAreDiagonal :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> Bool -> IO ()
setMemoryWeightsAreDiagonal mpslstmDescriptor  value =
  sendMsg mpslstmDescriptor (mkSelector "setMemoryWeightsAreDiagonal:") retVoid [argCULong (if value then 1 else 0)]

-- | cellToOutputNeuronType
--
-- Neuron type definition for 'gh', see MPSCNNNeuronType. Defaults to MPSCNNNeuronTypeTanH.
--
-- ObjC selector: @- cellToOutputNeuronType@
cellToOutputNeuronType :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO MPSCNNNeuronType
cellToOutputNeuronType mpslstmDescriptor  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpslstmDescriptor (mkSelector "cellToOutputNeuronType") retCInt []

-- | cellToOutputNeuronType
--
-- Neuron type definition for 'gh', see MPSCNNNeuronType. Defaults to MPSCNNNeuronTypeTanH.
--
-- ObjC selector: @- setCellToOutputNeuronType:@
setCellToOutputNeuronType :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> MPSCNNNeuronType -> IO ()
setCellToOutputNeuronType mpslstmDescriptor  value =
  sendMsg mpslstmDescriptor (mkSelector "setCellToOutputNeuronType:") retVoid [argCInt (coerce value)]

-- | cellToOutputNeuronParamA
--
-- Neuron parameter A for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- cellToOutputNeuronParamA@
cellToOutputNeuronParamA :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO CFloat
cellToOutputNeuronParamA mpslstmDescriptor  =
  sendMsg mpslstmDescriptor (mkSelector "cellToOutputNeuronParamA") retCFloat []

-- | cellToOutputNeuronParamA
--
-- Neuron parameter A for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- setCellToOutputNeuronParamA:@
setCellToOutputNeuronParamA :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> CFloat -> IO ()
setCellToOutputNeuronParamA mpslstmDescriptor  value =
  sendMsg mpslstmDescriptor (mkSelector "setCellToOutputNeuronParamA:") retVoid [argCFloat (fromIntegral value)]

-- | cellToOutputNeuronParamB
--
-- Neuron parameter B for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- cellToOutputNeuronParamB@
cellToOutputNeuronParamB :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO CFloat
cellToOutputNeuronParamB mpslstmDescriptor  =
  sendMsg mpslstmDescriptor (mkSelector "cellToOutputNeuronParamB") retCFloat []

-- | cellToOutputNeuronParamB
--
-- Neuron parameter B for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- setCellToOutputNeuronParamB:@
setCellToOutputNeuronParamB :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> CFloat -> IO ()
setCellToOutputNeuronParamB mpslstmDescriptor  value =
  sendMsg mpslstmDescriptor (mkSelector "setCellToOutputNeuronParamB:") retVoid [argCFloat (fromIntegral value)]

-- | cellToOutputNeuronParamC
--
-- Neuron parameter C for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- cellToOutputNeuronParamC@
cellToOutputNeuronParamC :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> IO CFloat
cellToOutputNeuronParamC mpslstmDescriptor  =
  sendMsg mpslstmDescriptor (mkSelector "cellToOutputNeuronParamC") retCFloat []

-- | cellToOutputNeuronParamC
--
-- Neuron parameter C for 'gh'. Defaults to 1.0f.
--
-- ObjC selector: @- setCellToOutputNeuronParamC:@
setCellToOutputNeuronParamC :: IsMPSLSTMDescriptor mpslstmDescriptor => mpslstmDescriptor -> CFloat -> IO ()
setCellToOutputNeuronParamC mpslstmDescriptor  value =
  sendMsg mpslstmDescriptor (mkSelector "setCellToOutputNeuronParamC:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createLSTMDescriptorWithInputFeatureChannels:outputFeatureChannels:@
createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector :: Selector
createLSTMDescriptorWithInputFeatureChannels_outputFeatureChannelsSelector = mkSelector "createLSTMDescriptorWithInputFeatureChannels:outputFeatureChannels:"

-- | @Selector@ for @memoryWeightsAreDiagonal@
memoryWeightsAreDiagonalSelector :: Selector
memoryWeightsAreDiagonalSelector = mkSelector "memoryWeightsAreDiagonal"

-- | @Selector@ for @setMemoryWeightsAreDiagonal:@
setMemoryWeightsAreDiagonalSelector :: Selector
setMemoryWeightsAreDiagonalSelector = mkSelector "setMemoryWeightsAreDiagonal:"

-- | @Selector@ for @cellToOutputNeuronType@
cellToOutputNeuronTypeSelector :: Selector
cellToOutputNeuronTypeSelector = mkSelector "cellToOutputNeuronType"

-- | @Selector@ for @setCellToOutputNeuronType:@
setCellToOutputNeuronTypeSelector :: Selector
setCellToOutputNeuronTypeSelector = mkSelector "setCellToOutputNeuronType:"

-- | @Selector@ for @cellToOutputNeuronParamA@
cellToOutputNeuronParamASelector :: Selector
cellToOutputNeuronParamASelector = mkSelector "cellToOutputNeuronParamA"

-- | @Selector@ for @setCellToOutputNeuronParamA:@
setCellToOutputNeuronParamASelector :: Selector
setCellToOutputNeuronParamASelector = mkSelector "setCellToOutputNeuronParamA:"

-- | @Selector@ for @cellToOutputNeuronParamB@
cellToOutputNeuronParamBSelector :: Selector
cellToOutputNeuronParamBSelector = mkSelector "cellToOutputNeuronParamB"

-- | @Selector@ for @setCellToOutputNeuronParamB:@
setCellToOutputNeuronParamBSelector :: Selector
setCellToOutputNeuronParamBSelector = mkSelector "setCellToOutputNeuronParamB:"

-- | @Selector@ for @cellToOutputNeuronParamC@
cellToOutputNeuronParamCSelector :: Selector
cellToOutputNeuronParamCSelector = mkSelector "cellToOutputNeuronParamC"

-- | @Selector@ for @setCellToOutputNeuronParamC:@
setCellToOutputNeuronParamCSelector :: Selector
setCellToOutputNeuronParamCSelector = mkSelector "setCellToOutputNeuronParamC:"

