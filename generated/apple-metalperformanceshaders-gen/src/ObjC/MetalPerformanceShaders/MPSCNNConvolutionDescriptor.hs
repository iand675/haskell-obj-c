{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolutionDescriptor
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionDescriptor specifies a convolution descriptor
--
-- Generated bindings for @MPSCNNConvolutionDescriptor@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionDescriptor
  ( MPSCNNConvolutionDescriptor
  , IsMPSCNNConvolutionDescriptor(..)
  , encodeWithCoder
  , initWithCoder
  , cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilter
  , cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels
  , setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilon
  , setNeuronType_parameterA_parameterB
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , setNeuronToPReLUWithParametersA
  , kernelWidth
  , setKernelWidth
  , kernelHeight
  , setKernelHeight
  , inputFeatureChannels
  , setInputFeatureChannels
  , outputFeatureChannels
  , setOutputFeatureChannels
  , strideInPixelsX
  , setStrideInPixelsX
  , strideInPixelsY
  , setStrideInPixelsY
  , groups
  , setGroups
  , dilationRateX
  , setDilationRateX
  , dilationRateY
  , setDilationRateY
  , fusedNeuronDescriptor
  , setFusedNeuronDescriptor
  , neuron
  , setNeuron
  , supportsSecureCoding
  , cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannelsSelector
  , cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilterSelector
  , dilationRateXSelector
  , dilationRateYSelector
  , encodeWithCoderSelector
  , fusedNeuronDescriptorSelector
  , groupsSelector
  , initWithCoderSelector
  , inputFeatureChannelsSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronSelector
  , neuronTypeSelector
  , outputFeatureChannelsSelector
  , setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilonSelector
  , setDilationRateXSelector
  , setDilationRateYSelector
  , setFusedNeuronDescriptorSelector
  , setGroupsSelector
  , setInputFeatureChannelsSelector
  , setKernelHeightSelector
  , setKernelWidthSelector
  , setNeuronSelector
  , setNeuronToPReLUWithParametersASelector
  , setNeuronType_parameterA_parameterBSelector
  , setOutputFeatureChannelsSelector
  , setStrideInPixelsXSelector
  , setStrideInPixelsYSelector
  , strideInPixelsXSelector
  , strideInPixelsYSelector
  , supportsSecureCodingSelector

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

-- | <NSSecureCoding> support
--
-- ObjC selector: @- encodeWithCoder:@
encodeWithCoder :: (IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor, IsNSCoder aCoder) => mpscnnConvolutionDescriptor -> aCoder -> IO ()
encodeWithCoder mpscnnConvolutionDescriptor aCoder =
  sendMessage mpscnnConvolutionDescriptor encodeWithCoderSelector (toNSCoder aCoder)

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor, IsNSCoder aDecoder) => mpscnnConvolutionDescriptor -> aDecoder -> IO (Id MPSCNNConvolutionDescriptor)
initWithCoder mpscnnConvolutionDescriptor aDecoder =
  sendOwnedMessage mpscnnConvolutionDescriptor initWithCoderSelector (toNSCoder aDecoder)

-- | This method is deprecated. Please use neuronType, neuronParameterA and neuronParameterB properites to fuse              neuron with convolution.
--
-- @kernelWidth@ — The width of the filter window.  Must be > 0. Large values will take a long time.
--
-- @kernelHeight@ — The height of the filter window.   Must be > 0. Large values will take a long time.
--
-- @inputFeatureChannels@ — The number of feature channels in the input image. Must be >= 1.
--
-- @outputFeatureChannels@ — The number of feature channels in the output image. Must be >= 1.
--
-- @neuronFilter@ — An optional neuron filter that can be applied to the output of convolution.
--
-- Returns: A valid MPSCNNConvolutionDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:neuronFilter:@
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilter :: CULong -> CULong -> CULong -> CULong -> Const (Id MPSCNNNeuron) -> IO (Id MPSCNNConvolutionDescriptor)
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilter kernelWidth kernelHeight inputFeatureChannels outputFeatureChannels neuronFilter =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionDescriptor"
    sendClassMessage cls' cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilterSelector kernelWidth kernelHeight inputFeatureChannels outputFeatureChannels neuronFilter

-- | Creates a convolution descriptor.
--
-- @kernelWidth@ — The width of the filter window.  Must be > 0. Large values will take a long time.
--
-- @kernelHeight@ — The height of the filter window.   Must be > 0. Large values will take a long time.
--
-- @inputFeatureChannels@ — The number of feature channels in the input image. Must be >= 1.
--
-- @outputFeatureChannels@ — The number of feature channels in the output image. Must be >= 1.
--
-- Returns: A valid MPSCNNConvolutionDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:@
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels :: CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNConvolutionDescriptor)
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels kernelWidth kernelHeight inputFeatureChannels outputFeatureChannels =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionDescriptor"
    sendClassMessage cls' cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannelsSelector kernelWidth kernelHeight inputFeatureChannels outputFeatureChannels

-- | Adds batch normalization for inference, it copies all the float arrays provided, expecting               outputFeatureChannels elements in each.
--
-- This method will be used to pass in batch normalization parameters to the convolution during the              init call. For inference we modify weights and bias going in convolution or Fully Connected layer to combine              and optimize the layers.
--
-- w: weights for a corresponding output feature channel              b: bias for a corresponding output feature channel              W: batch normalized weights for a corresponding output feature channel              B: batch normalized bias for a corresponding output feature channel
--
-- I = gamma / sqrt(variance + epsilon), J = beta - ( I * mean )
--
-- W = w * I              B = b * I + J
--
-- Every convolution has (OutputFeatureChannel * kernelWidth * kernelHeight * InputFeatureChannel) weights
--
-- I, J are calculated, for every output feature channel separately to get the corresponding weights and bias              Thus, I, J are calculated and then used for every (kernelWidth * kernelHeight * InputFeatureChannel)              weights, and this is done OutputFeatureChannel number of times for each output channel.
--
-- thus, internally, batch normalized weights are computed as:
--
-- W[no][i][j][ni] = w[no][i][j][ni] * I[no]
--
-- no: index into outputFeatureChannel              i : index into kernel Height              j : index into kernel Width              ni: index into inputFeatureChannel
--
-- One usually doesn't see a bias term and batch normalization together as batch normalization potentially cancels              out the bias term after training, but in MPS if the user provides it, batch normalization will use the above               formula to incorporate it, if user does not have bias terms then put a float array of zeroes in the convolution              init for bias terms of each output feature channel.
--
-- this comes from:              https://arxiv.org/pdf/1502.03167v3.pdf
--
-- Note: in certain cases the batch normalization parameters will be cached by the MPSNNGraph              or the MPSCNNConvolution. If the batch normalization parameters change after either is made,              behavior is undefined.
--
-- @mean@ — Pointer to an array of floats of mean for each output feature channel
--
-- @variance@ — Pointer to an array of floats of variance for each output feature channel
--
-- @gamma@ — Pointer to an array of floats of gamma for each output feature channel
--
-- @beta@ — Pointer to an array of floats of beta for each output feature channel
--
-- @epsilon@ — A small float value used to have numerical stability in the code
--
-- ObjC selector: @- setBatchNormalizationParametersForInferenceWithMean:variance:gamma:beta:epsilon:@
setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilon :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> Const CFloat -> IO ()
setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilon mpscnnConvolutionDescriptor mean variance gamma beta epsilon =
  sendMessage mpscnnConvolutionDescriptor setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilonSelector mean variance gamma beta epsilon

-- | Adds a neuron activation function to convolution descriptor.
--
-- This mathod can be used to add a neuron activation funtion of given type with              associated scalar parameters A and B that are shared across all output channels.              Neuron activation fucntion is applied to output of convolution. This is a per-pixel              operation that is fused with convolution kernel itself for best performance.              Note that this method can only be used to fuse neuron of kind for which parameters              A and B are shared across all channels of convoution output. It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions.
--
-- Note: in certain cases, the neuron descriptor will be cached by the MPSNNGraph or the              MPSCNNConvolution. If the neuron type changes after either is made, behavior is undefined.
--
-- @neuronType@ — type of neuron activation function. For full list see MPSCNNNeuronType.h
--
-- @parameterA@ — parameterA of neuron activation that is shared across all channels of convolution output.
--
-- @parameterB@ — parameterB of neuron activation that is shared across all channels of convolution output.
--
-- ObjC selector: @- setNeuronType:parameterA:parameterB:@
setNeuronType_parameterA_parameterB :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> MPSCNNNeuronType -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB mpscnnConvolutionDescriptor neuronType parameterA parameterB =
  sendMessage mpscnnConvolutionDescriptor setNeuronType_parameterA_parameterBSelector neuronType parameterA parameterB

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO MPSCNNNeuronType
neuronType mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor neuronTypeSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CFloat
neuronParameterA mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor neuronParameterASelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CFloat
neuronParameterB mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor neuronParameterBSelector

-- | Add per-channel neuron parameters A for PReLu neuron activation functions.
--
-- This method sets the neuron to PReLU, zeros parameters A and B and sets the per-channel              neuron parameters A to an array containing a unique value of A for each output feature              channel.
--
-- If the neuron function is f(v,a,b), it will apply
--
-- OutputImage(x,y,i) = f( ConvolutionResult(x,y,i), A[i], B[i] ) where i in [0,outputFeatureChannels-1]
--
-- See https://arxiv.org/pdf/1502.01852.pdf for details.
--
-- All other neuron types, where parameter A              and parameter B are shared across channels must be set using              -setNeuronOfType:parameterA:parameterB:
--
-- If batch normalization parameters are set, batch normalization will occur before              neuron application i.e. output of convolution is first batch normalized followed              by neuron activation. This function automatically sets neuronType to MPSCNNNeuronTypePReLU.
--
-- Note: in certain cases the neuron descriptor will be cached by the MPSNNGraph or the              MPSCNNConvolution. If the neuron type changes after either is made, behavior is undefined.
--
-- @A@ — An array containing per-channel float values for neuron parameter A.                      Number of entries must be equal to outputFeatureChannels.
--
-- ObjC selector: @- setNeuronToPReLUWithParametersA:@
setNeuronToPReLUWithParametersA :: (IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor, IsNSData a) => mpscnnConvolutionDescriptor -> a -> IO ()
setNeuronToPReLUWithParametersA mpscnnConvolutionDescriptor a =
  sendMessage mpscnnConvolutionDescriptor setNeuronToPReLUWithParametersASelector (toNSData a)

-- | kernelWidth
--
-- The width of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the left edge of the filter window is given              by offset.x - (kernelWidth>>1)
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
kernelWidth mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor kernelWidthSelector

-- | kernelWidth
--
-- The width of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the left edge of the filter window is given              by offset.x - (kernelWidth>>1)
--
-- ObjC selector: @- setKernelWidth:@
setKernelWidth :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setKernelWidth mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setKernelWidthSelector value

-- | kernelHeight
--
-- The height of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the top edge of the filter window is given              by offset.y - (kernelHeight>>1)
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
kernelHeight mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor kernelHeightSelector

-- | kernelHeight
--
-- The height of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the top edge of the filter window is given              by offset.y - (kernelHeight>>1)
--
-- ObjC selector: @- setKernelHeight:@
setKernelHeight :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setKernelHeight mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setKernelHeightSelector value

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
inputFeatureChannels mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor inputFeatureChannelsSelector

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- setInputFeatureChannels:@
setInputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setInputFeatureChannels mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setInputFeatureChannelsSelector value

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
outputFeatureChannels mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor outputFeatureChannelsSelector

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- setOutputFeatureChannels:@
setOutputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setOutputFeatureChannels mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setOutputFeatureChannelsSelector value

-- | strideInPixelsX
--
-- The output stride (downsampling factor) in the x dimension. The default value is 1.
--
-- ObjC selector: @- strideInPixelsX@
strideInPixelsX :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
strideInPixelsX mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor strideInPixelsXSelector

-- | strideInPixelsX
--
-- The output stride (downsampling factor) in the x dimension. The default value is 1.
--
-- ObjC selector: @- setStrideInPixelsX:@
setStrideInPixelsX :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setStrideInPixelsX mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setStrideInPixelsXSelector value

-- | strideInPixelsY
--
-- The output stride (downsampling factor) in the y dimension. The default value is 1.
--
-- ObjC selector: @- strideInPixelsY@
strideInPixelsY :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
strideInPixelsY mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor strideInPixelsYSelector

-- | strideInPixelsY
--
-- The output stride (downsampling factor) in the y dimension. The default value is 1.
--
-- ObjC selector: @- setStrideInPixelsY:@
setStrideInPixelsY :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setStrideInPixelsY mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setStrideInPixelsYSelector value

-- | groups
--
-- Number of groups input and output channels are divided into. The default value is 1.              Groups lets you reduce the parameterization. If groups is set to n, input is divided into n              groups with inputFeatureChannels/n channels in each group. Similarly output is divided into              n groups with outputFeatureChannels/n channels in each group. ith group in input is only              connected to ith group in output so number of weights (parameters) needed is reduced by factor              of n. Both inputFeatureChannels and outputFeatureChannels must be divisible by n and number of              channels in each group must be multiple of 4.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
groups mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor groupsSelector

-- | groups
--
-- Number of groups input and output channels are divided into. The default value is 1.              Groups lets you reduce the parameterization. If groups is set to n, input is divided into n              groups with inputFeatureChannels/n channels in each group. Similarly output is divided into              n groups with outputFeatureChannels/n channels in each group. ith group in input is only              connected to ith group in output so number of weights (parameters) needed is reduced by factor              of n. Both inputFeatureChannels and outputFeatureChannels must be divisible by n and number of              channels in each group must be multiple of 4.
--
-- ObjC selector: @- setGroups:@
setGroups :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setGroups mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setGroupsSelector value

-- | dilationRateX
--
-- dilationRateX property can be used to implement dilated convolution as described in                          https://arxiv.org/pdf/1511.07122v3.pdf                 to aggregate global information in dense prediction problems.                 Default value is 1. When set to value > 1, original kernel width, kW is dilated to
--
-- kW_Dilated = (kW-1)*dilationRateX + 1
--
-- by inserting d-1 zeros between consecutive entries in each row of the original kernel.                  The kernel is centered based on kW_Dilated.
--
-- ObjC selector: @- dilationRateX@
dilationRateX :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
dilationRateX mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor dilationRateXSelector

-- | dilationRateX
--
-- dilationRateX property can be used to implement dilated convolution as described in                          https://arxiv.org/pdf/1511.07122v3.pdf                 to aggregate global information in dense prediction problems.                 Default value is 1. When set to value > 1, original kernel width, kW is dilated to
--
-- kW_Dilated = (kW-1)*dilationRateX + 1
--
-- by inserting d-1 zeros between consecutive entries in each row of the original kernel.                  The kernel is centered based on kW_Dilated.
--
-- ObjC selector: @- setDilationRateX:@
setDilationRateX :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setDilationRateX mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setDilationRateXSelector value

-- | dilationRateY
--
-- dilationRateY property can be used to implement dilated convolution as described in                          https://arxiv.org/pdf/1511.07122v3.pdf                 to aggregate global information in dense prediction problems.                 Default value is 1. When set to value > 1, original kernel height, kH is dilated to
--
-- kH_Dilated = (kH-1)*dilationRateY + 1
--
-- by inserting d-1 rows of zeros between consecutive row of the original kernel.                 The kernel is centered based on kH_Dilated.
--
-- ObjC selector: @- dilationRateY@
dilationRateY :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
dilationRateY mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor dilationRateYSelector

-- | dilationRateY
--
-- dilationRateY property can be used to implement dilated convolution as described in                          https://arxiv.org/pdf/1511.07122v3.pdf                 to aggregate global information in dense prediction problems.                 Default value is 1. When set to value > 1, original kernel height, kH is dilated to
--
-- kH_Dilated = (kH-1)*dilationRateY + 1
--
-- by inserting d-1 rows of zeros between consecutive row of the original kernel.                 The kernel is centered based on kH_Dilated.
--
-- ObjC selector: @- setDilationRateY:@
setDilationRateY :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setDilationRateY mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setDilationRateYSelector value

-- | fusedNeuronDescriptor
--
-- This mathod can be used to add a neuron activation funtion of given type with              associated scalar parameters A and B that are shared across all output channels.              Neuron activation fucntion is applied to output of convolution. This is a per-pixel              operation that is fused with convolution kernel itself for best performance.              Note that this method can only be used to fuse neuron of kind for which parameters              A and B are shared across all channels of convoution output. It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions. Default is descriptor with neuronType MPSCNNNeuronTypeNone.
--
-- Note: in certain cases the neuron descriptor will be cached by the MPSNNGraph or the              MPSCNNConvolution. If the neuron type changes after either is made, behavior is undefined.
--
-- ObjC selector: @- fusedNeuronDescriptor@
fusedNeuronDescriptor :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO (Id MPSNNNeuronDescriptor)
fusedNeuronDescriptor mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor fusedNeuronDescriptorSelector

-- | fusedNeuronDescriptor
--
-- This mathod can be used to add a neuron activation funtion of given type with              associated scalar parameters A and B that are shared across all output channels.              Neuron activation fucntion is applied to output of convolution. This is a per-pixel              operation that is fused with convolution kernel itself for best performance.              Note that this method can only be used to fuse neuron of kind for which parameters              A and B are shared across all channels of convoution output. It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions. Default is descriptor with neuronType MPSCNNNeuronTypeNone.
--
-- Note: in certain cases the neuron descriptor will be cached by the MPSNNGraph or the              MPSCNNConvolution. If the neuron type changes after either is made, behavior is undefined.
--
-- ObjC selector: @- setFusedNeuronDescriptor:@
setFusedNeuronDescriptor :: (IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor, IsMPSNNNeuronDescriptor value) => mpscnnConvolutionDescriptor -> value -> IO ()
setFusedNeuronDescriptor mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setFusedNeuronDescriptorSelector (toMPSNNNeuronDescriptor value)

-- | neuron
--
-- MPSCNNNeuron filter to be applied as part of convolution. This is applied after BatchNormalization in the end.              Default is nil.              This is deprecated. You dont need to create MPSCNNNeuron object to fuse with convolution. Use neuron properties              in this descriptor.
--
-- ObjC selector: @- neuron@
neuron :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO RawId
neuron mpscnnConvolutionDescriptor =
  sendMessage mpscnnConvolutionDescriptor neuronSelector

-- | neuron
--
-- MPSCNNNeuron filter to be applied as part of convolution. This is applied after BatchNormalization in the end.              Default is nil.              This is deprecated. You dont need to create MPSCNNNeuron object to fuse with convolution. Use neuron properties              in this descriptor.
--
-- ObjC selector: @- setNeuron:@
setNeuron :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> RawId -> IO ()
setNeuron mpscnnConvolutionDescriptor value =
  sendMessage mpscnnConvolutionDescriptor setNeuronSelector value

-- | <NSSecureCoding> support
--
-- ObjC selector: @+ supportsSecureCoding@
supportsSecureCoding :: IO Bool
supportsSecureCoding  =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionDescriptor"
    sendClassMessage cls' supportsSecureCodingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector '[Id NSCoder] ()
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id MPSCNNConvolutionDescriptor)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:neuronFilter:@
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilterSelector :: Selector '[CULong, CULong, CULong, CULong, Const (Id MPSCNNNeuron)] (Id MPSCNNConvolutionDescriptor)
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilterSelector = mkSelector "cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:neuronFilter:"

-- | @Selector@ for @cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:@
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannelsSelector :: Selector '[CULong, CULong, CULong, CULong] (Id MPSCNNConvolutionDescriptor)
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannelsSelector = mkSelector "cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:"

-- | @Selector@ for @setBatchNormalizationParametersForInferenceWithMean:variance:gamma:beta:epsilon:@
setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilonSelector :: Selector '[Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), Const (Ptr CFloat), Const CFloat] ()
setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilonSelector = mkSelector "setBatchNormalizationParametersForInferenceWithMean:variance:gamma:beta:epsilon:"

-- | @Selector@ for @setNeuronType:parameterA:parameterB:@
setNeuronType_parameterA_parameterBSelector :: Selector '[MPSCNNNeuronType, CFloat, CFloat] ()
setNeuronType_parameterA_parameterBSelector = mkSelector "setNeuronType:parameterA:parameterB:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector '[] MPSCNNNeuronType
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector '[] CFloat
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector '[] CFloat
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @setNeuronToPReLUWithParametersA:@
setNeuronToPReLUWithParametersASelector :: Selector '[Id NSData] ()
setNeuronToPReLUWithParametersASelector = mkSelector "setNeuronToPReLUWithParametersA:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector '[CULong] ()
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector '[CULong] ()
setKernelHeightSelector = mkSelector "setKernelHeight:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector '[] CULong
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @setInputFeatureChannels:@
setInputFeatureChannelsSelector :: Selector '[CULong] ()
setInputFeatureChannelsSelector = mkSelector "setInputFeatureChannels:"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector '[] CULong
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @setOutputFeatureChannels:@
setOutputFeatureChannelsSelector :: Selector '[CULong] ()
setOutputFeatureChannelsSelector = mkSelector "setOutputFeatureChannels:"

-- | @Selector@ for @strideInPixelsX@
strideInPixelsXSelector :: Selector '[] CULong
strideInPixelsXSelector = mkSelector "strideInPixelsX"

-- | @Selector@ for @setStrideInPixelsX:@
setStrideInPixelsXSelector :: Selector '[CULong] ()
setStrideInPixelsXSelector = mkSelector "setStrideInPixelsX:"

-- | @Selector@ for @strideInPixelsY@
strideInPixelsYSelector :: Selector '[] CULong
strideInPixelsYSelector = mkSelector "strideInPixelsY"

-- | @Selector@ for @setStrideInPixelsY:@
setStrideInPixelsYSelector :: Selector '[CULong] ()
setStrideInPixelsYSelector = mkSelector "setStrideInPixelsY:"

-- | @Selector@ for @groups@
groupsSelector :: Selector '[] CULong
groupsSelector = mkSelector "groups"

-- | @Selector@ for @setGroups:@
setGroupsSelector :: Selector '[CULong] ()
setGroupsSelector = mkSelector "setGroups:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector '[] CULong
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @setDilationRateX:@
setDilationRateXSelector :: Selector '[CULong] ()
setDilationRateXSelector = mkSelector "setDilationRateX:"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector '[] CULong
dilationRateYSelector = mkSelector "dilationRateY"

-- | @Selector@ for @setDilationRateY:@
setDilationRateYSelector :: Selector '[CULong] ()
setDilationRateYSelector = mkSelector "setDilationRateY:"

-- | @Selector@ for @fusedNeuronDescriptor@
fusedNeuronDescriptorSelector :: Selector '[] (Id MPSNNNeuronDescriptor)
fusedNeuronDescriptorSelector = mkSelector "fusedNeuronDescriptor"

-- | @Selector@ for @setFusedNeuronDescriptor:@
setFusedNeuronDescriptorSelector :: Selector '[Id MPSNNNeuronDescriptor] ()
setFusedNeuronDescriptorSelector = mkSelector "setFusedNeuronDescriptor:"

-- | @Selector@ for @neuron@
neuronSelector :: Selector '[] RawId
neuronSelector = mkSelector "neuron"

-- | @Selector@ for @setNeuron:@
setNeuronSelector :: Selector '[RawId] ()
setNeuronSelector = mkSelector "setNeuron:"

-- | @Selector@ for @supportsSecureCoding@
supportsSecureCodingSelector :: Selector '[] Bool
supportsSecureCodingSelector = mkSelector "supportsSecureCoding"

