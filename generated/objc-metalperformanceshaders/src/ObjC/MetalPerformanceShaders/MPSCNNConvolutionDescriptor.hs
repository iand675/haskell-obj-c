{-# LANGUAGE PatternSynonyms #-}
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
  , supportsSecureCoding
  , encodeWithCoderSelector
  , initWithCoderSelector
  , cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilterSelector
  , cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannelsSelector
  , setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilonSelector
  , setNeuronType_parameterA_parameterBSelector
  , neuronTypeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , setNeuronToPReLUWithParametersASelector
  , kernelWidthSelector
  , setKernelWidthSelector
  , kernelHeightSelector
  , setKernelHeightSelector
  , inputFeatureChannelsSelector
  , setInputFeatureChannelsSelector
  , outputFeatureChannelsSelector
  , setOutputFeatureChannelsSelector
  , strideInPixelsXSelector
  , setStrideInPixelsXSelector
  , strideInPixelsYSelector
  , setStrideInPixelsYSelector
  , groupsSelector
  , setGroupsSelector
  , dilationRateXSelector
  , setDilationRateXSelector
  , dilationRateYSelector
  , setDilationRateYSelector
  , fusedNeuronDescriptorSelector
  , setFusedNeuronDescriptorSelector
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

-- | <NSSecureCoding> support
--
-- ObjC selector: @- encodeWithCoder:@
encodeWithCoder :: (IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor, IsNSCoder aCoder) => mpscnnConvolutionDescriptor -> aCoder -> IO ()
encodeWithCoder mpscnnConvolutionDescriptor  aCoder =
withObjCPtr aCoder $ \raw_aCoder ->
    sendMsg mpscnnConvolutionDescriptor (mkSelector "encodeWithCoder:") retVoid [argPtr (castPtr raw_aCoder :: Ptr ())]

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor, IsNSCoder aDecoder) => mpscnnConvolutionDescriptor -> aDecoder -> IO (Id MPSCNNConvolutionDescriptor)
initWithCoder mpscnnConvolutionDescriptor  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnConvolutionDescriptor (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr neuronFilter $ \raw_neuronFilter ->
      sendClassMsg cls' (mkSelector "cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:neuronFilter:") (retPtr retVoid) [argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral inputFeatureChannels), argCULong (fromIntegral outputFeatureChannels), argPtr (castPtr raw_neuronFilter :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:") (retPtr retVoid) [argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral inputFeatureChannels), argCULong (fromIntegral outputFeatureChannels)] >>= retainedObject . castPtr

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
setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilon mpscnnConvolutionDescriptor  mean variance gamma beta epsilon =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setBatchNormalizationParametersForInferenceWithMean:variance:gamma:beta:epsilon:") retVoid [argPtr (unConst mean), argPtr (unConst variance), argPtr (unConst gamma), argPtr (unConst beta), argCFloat (fromIntegral (unConst epsilon))]

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
setNeuronType_parameterA_parameterB mpscnnConvolutionDescriptor  neuronType parameterA parameterB =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setNeuronType:parameterA:parameterB:") retVoid [argCInt (coerce neuronType), argCFloat (fromIntegral parameterA), argCFloat (fromIntegral parameterB)]

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO MPSCNNNeuronType
neuronType mpscnnConvolutionDescriptor  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpscnnConvolutionDescriptor (mkSelector "neuronType") retCInt []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CFloat
neuronParameterA mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "neuronParameterA") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CFloat
neuronParameterB mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "neuronParameterB") retCFloat []

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
setNeuronToPReLUWithParametersA mpscnnConvolutionDescriptor  a =
withObjCPtr a $ \raw_a ->
    sendMsg mpscnnConvolutionDescriptor (mkSelector "setNeuronToPReLUWithParametersA:") retVoid [argPtr (castPtr raw_a :: Ptr ())]

-- | kernelWidth
--
-- The width of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the left edge of the filter window is given              by offset.x - (kernelWidth>>1)
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
kernelWidth mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "kernelWidth") retCULong []

-- | kernelWidth
--
-- The width of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the left edge of the filter window is given              by offset.x - (kernelWidth>>1)
--
-- ObjC selector: @- setKernelWidth:@
setKernelWidth :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setKernelWidth mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setKernelWidth:") retVoid [argCULong (fromIntegral value)]

-- | kernelHeight
--
-- The height of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the top edge of the filter window is given              by offset.y - (kernelHeight>>1)
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
kernelHeight mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "kernelHeight") retCULong []

-- | kernelHeight
--
-- The height of the filter window.  The default value is 3.              Any positive non-zero value is valid, including even values.              The position of the top edge of the filter window is given              by offset.y - (kernelHeight>>1)
--
-- ObjC selector: @- setKernelHeight:@
setKernelHeight :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setKernelHeight mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setKernelHeight:") retVoid [argCULong (fromIntegral value)]

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
inputFeatureChannels mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "inputFeatureChannels") retCULong []

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- setInputFeatureChannels:@
setInputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setInputFeatureChannels mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setInputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
outputFeatureChannels mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "outputFeatureChannels") retCULong []

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- setOutputFeatureChannels:@
setOutputFeatureChannels :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setOutputFeatureChannels mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setOutputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | strideInPixelsX
--
-- The output stride (downsampling factor) in the x dimension. The default value is 1.
--
-- ObjC selector: @- strideInPixelsX@
strideInPixelsX :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
strideInPixelsX mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "strideInPixelsX") retCULong []

-- | strideInPixelsX
--
-- The output stride (downsampling factor) in the x dimension. The default value is 1.
--
-- ObjC selector: @- setStrideInPixelsX:@
setStrideInPixelsX :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setStrideInPixelsX mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setStrideInPixelsX:") retVoid [argCULong (fromIntegral value)]

-- | strideInPixelsY
--
-- The output stride (downsampling factor) in the y dimension. The default value is 1.
--
-- ObjC selector: @- strideInPixelsY@
strideInPixelsY :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
strideInPixelsY mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "strideInPixelsY") retCULong []

-- | strideInPixelsY
--
-- The output stride (downsampling factor) in the y dimension. The default value is 1.
--
-- ObjC selector: @- setStrideInPixelsY:@
setStrideInPixelsY :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setStrideInPixelsY mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setStrideInPixelsY:") retVoid [argCULong (fromIntegral value)]

-- | groups
--
-- Number of groups input and output channels are divided into. The default value is 1.              Groups lets you reduce the parameterization. If groups is set to n, input is divided into n              groups with inputFeatureChannels/n channels in each group. Similarly output is divided into              n groups with outputFeatureChannels/n channels in each group. ith group in input is only              connected to ith group in output so number of weights (parameters) needed is reduced by factor              of n. Both inputFeatureChannels and outputFeatureChannels must be divisible by n and number of              channels in each group must be multiple of 4.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO CULong
groups mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "groups") retCULong []

-- | groups
--
-- Number of groups input and output channels are divided into. The default value is 1.              Groups lets you reduce the parameterization. If groups is set to n, input is divided into n              groups with inputFeatureChannels/n channels in each group. Similarly output is divided into              n groups with outputFeatureChannels/n channels in each group. ith group in input is only              connected to ith group in output so number of weights (parameters) needed is reduced by factor              of n. Both inputFeatureChannels and outputFeatureChannels must be divisible by n and number of              channels in each group must be multiple of 4.
--
-- ObjC selector: @- setGroups:@
setGroups :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> CULong -> IO ()
setGroups mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setGroups:") retVoid [argCULong (fromIntegral value)]

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
dilationRateX mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "dilationRateX") retCULong []

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
setDilationRateX mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setDilationRateX:") retVoid [argCULong (fromIntegral value)]

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
dilationRateY mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "dilationRateY") retCULong []

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
setDilationRateY mpscnnConvolutionDescriptor  value =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "setDilationRateY:") retVoid [argCULong (fromIntegral value)]

-- | fusedNeuronDescriptor
--
-- This mathod can be used to add a neuron activation funtion of given type with              associated scalar parameters A and B that are shared across all output channels.              Neuron activation fucntion is applied to output of convolution. This is a per-pixel              operation that is fused with convolution kernel itself for best performance.              Note that this method can only be used to fuse neuron of kind for which parameters              A and B are shared across all channels of convoution output. It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions. Default is descriptor with neuronType MPSCNNNeuronTypeNone.
--
-- Note: in certain cases the neuron descriptor will be cached by the MPSNNGraph or the              MPSCNNConvolution. If the neuron type changes after either is made, behavior is undefined.
--
-- ObjC selector: @- fusedNeuronDescriptor@
fusedNeuronDescriptor :: IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor => mpscnnConvolutionDescriptor -> IO (Id MPSNNNeuronDescriptor)
fusedNeuronDescriptor mpscnnConvolutionDescriptor  =
  sendMsg mpscnnConvolutionDescriptor (mkSelector "fusedNeuronDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fusedNeuronDescriptor
--
-- This mathod can be used to add a neuron activation funtion of given type with              associated scalar parameters A and B that are shared across all output channels.              Neuron activation fucntion is applied to output of convolution. This is a per-pixel              operation that is fused with convolution kernel itself for best performance.              Note that this method can only be used to fuse neuron of kind for which parameters              A and B are shared across all channels of convoution output. It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions. Default is descriptor with neuronType MPSCNNNeuronTypeNone.
--
-- Note: in certain cases the neuron descriptor will be cached by the MPSNNGraph or the              MPSCNNConvolution. If the neuron type changes after either is made, behavior is undefined.
--
-- ObjC selector: @- setFusedNeuronDescriptor:@
setFusedNeuronDescriptor :: (IsMPSCNNConvolutionDescriptor mpscnnConvolutionDescriptor, IsMPSNNNeuronDescriptor value) => mpscnnConvolutionDescriptor -> value -> IO ()
setFusedNeuronDescriptor mpscnnConvolutionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpscnnConvolutionDescriptor (mkSelector "setFusedNeuronDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | <NSSecureCoding> support
--
-- ObjC selector: @+ supportsSecureCoding@
supportsSecureCoding :: IO Bool
supportsSecureCoding  =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionDescriptor"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsSecureCoding") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:neuronFilter:@
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilterSelector :: Selector
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannels_neuronFilterSelector = mkSelector "cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:neuronFilter:"

-- | @Selector@ for @cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:@
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannelsSelector :: Selector
cnnConvolutionDescriptorWithKernelWidth_kernelHeight_inputFeatureChannels_outputFeatureChannelsSelector = mkSelector "cnnConvolutionDescriptorWithKernelWidth:kernelHeight:inputFeatureChannels:outputFeatureChannels:"

-- | @Selector@ for @setBatchNormalizationParametersForInferenceWithMean:variance:gamma:beta:epsilon:@
setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilonSelector :: Selector
setBatchNormalizationParametersForInferenceWithMean_variance_gamma_beta_epsilonSelector = mkSelector "setBatchNormalizationParametersForInferenceWithMean:variance:gamma:beta:epsilon:"

-- | @Selector@ for @setNeuronType:parameterA:parameterB:@
setNeuronType_parameterA_parameterBSelector :: Selector
setNeuronType_parameterA_parameterBSelector = mkSelector "setNeuronType:parameterA:parameterB:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @setNeuronToPReLUWithParametersA:@
setNeuronToPReLUWithParametersASelector :: Selector
setNeuronToPReLUWithParametersASelector = mkSelector "setNeuronToPReLUWithParametersA:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector
setKernelHeightSelector = mkSelector "setKernelHeight:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @setInputFeatureChannels:@
setInputFeatureChannelsSelector :: Selector
setInputFeatureChannelsSelector = mkSelector "setInputFeatureChannels:"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @setOutputFeatureChannels:@
setOutputFeatureChannelsSelector :: Selector
setOutputFeatureChannelsSelector = mkSelector "setOutputFeatureChannels:"

-- | @Selector@ for @strideInPixelsX@
strideInPixelsXSelector :: Selector
strideInPixelsXSelector = mkSelector "strideInPixelsX"

-- | @Selector@ for @setStrideInPixelsX:@
setStrideInPixelsXSelector :: Selector
setStrideInPixelsXSelector = mkSelector "setStrideInPixelsX:"

-- | @Selector@ for @strideInPixelsY@
strideInPixelsYSelector :: Selector
strideInPixelsYSelector = mkSelector "strideInPixelsY"

-- | @Selector@ for @setStrideInPixelsY:@
setStrideInPixelsYSelector :: Selector
setStrideInPixelsYSelector = mkSelector "setStrideInPixelsY:"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @setGroups:@
setGroupsSelector :: Selector
setGroupsSelector = mkSelector "setGroups:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @setDilationRateX:@
setDilationRateXSelector :: Selector
setDilationRateXSelector = mkSelector "setDilationRateX:"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector
dilationRateYSelector = mkSelector "dilationRateY"

-- | @Selector@ for @setDilationRateY:@
setDilationRateYSelector :: Selector
setDilationRateYSelector = mkSelector "setDilationRateY:"

-- | @Selector@ for @fusedNeuronDescriptor@
fusedNeuronDescriptorSelector :: Selector
fusedNeuronDescriptorSelector = mkSelector "fusedNeuronDescriptor"

-- | @Selector@ for @setFusedNeuronDescriptor:@
setFusedNeuronDescriptorSelector :: Selector
setFusedNeuronDescriptorSelector = mkSelector "setFusedNeuronDescriptor:"

-- | @Selector@ for @supportsSecureCoding@
supportsSecureCodingSelector :: Selector
supportsSecureCodingSelector = mkSelector "supportsSecureCoding"

