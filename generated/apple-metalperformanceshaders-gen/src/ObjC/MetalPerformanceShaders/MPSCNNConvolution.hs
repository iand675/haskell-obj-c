{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolution
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolution specifies a convolution.              The MPSCNNConvolution convolves the input image with a set of filters, each producing one feature map in the output image.
--
-- Generated bindings for @MPSCNNConvolution@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolution
  ( MPSCNNConvolution
  , IsMPSCNNConvolution(..)
  , initWithDevice_weights
  , initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flags
  , initWithCoder_device
  , initWithDevice
  , resultStateForSourceImage_sourceStates_destinationImage
  , resultStateBatchForSourceImage_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage
  , reloadWeightsAndBiasesFromDataSource
  , reloadWeightsAndBiasesWithDataSource
  , reloadWeightsAndBiasesWithCommandBuffer_state
  , exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary
  , inputFeatureChannels
  , outputFeatureChannels
  , groups
  , dataSource
  , subPixelScaleFactor
  , neuron
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , fusedNeuronDescriptor
  , channelMultiplier
  , accumulatorPrecisionOption
  , setAccumulatorPrecisionOption
  , accumulatorPrecisionOptionSelector
  , channelMultiplierSelector
  , dataSourceSelector
  , exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector
  , fusedNeuronDescriptorSelector
  , groupsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector
  , initWithDevice_weightsSelector
  , inputFeatureChannelsSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , neuronSelector
  , neuronTypeSelector
  , outputFeatureChannelsSelector
  , reloadWeightsAndBiasesFromDataSourceSelector
  , reloadWeightsAndBiasesWithCommandBuffer_stateSelector
  , reloadWeightsAndBiasesWithDataSourceSelector
  , resultStateBatchForSourceImage_sourceStates_destinationImageSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , setAccumulatorPrecisionOptionSelector
  , subPixelScaleFactorSelector
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector

  -- * Enum types
  , MPSCNNConvolutionFlags(MPSCNNConvolutionFlags)
  , pattern MPSCNNConvolutionFlagsNone
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
  , MPSNNConvolutionAccumulatorPrecisionOption(MPSNNConvolutionAccumulatorPrecisionOption)
  , pattern MPSNNConvolutionAccumulatorPrecisionOptionHalf
  , pattern MPSNNConvolutionAccumulatorPrecisionOptionFloat

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

-- | Initializes a convolution kernel
--
-- @device@ — The MTLDevice on which this MPSCNNConvolution filter will be used
--
-- @weights@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource                                              protocol. The MPSCNNConvolutionDataSource protocol declares the methods that an                                              instance of MPSCNNConvolution uses to obtain the weights and bias terms                                               for the CNN convolution filter.
--
-- Returns: A valid MPSCNNConvolution object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:weights:@
initWithDevice_weights :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> RawId -> RawId -> IO (Id MPSCNNConvolution)
initWithDevice_weights mpscnnConvolution device weights =
  sendOwnedMessage mpscnnConvolution initWithDevice_weightsSelector device weights

-- | Initializes a convolution kernel              WARNING:                        This API is depreated and will be removed in the future. It cannot be used                                              when training. Also serialization/unserialization wont work for MPSCNNConvolution                                              objects created with this init. Please move onto using initWithDevice:weights:.
--
-- @device@ — The MTLDevice on which this MPSCNNConvolution filter will be used
--
-- @convolutionDescriptor@ — A pointer to a MPSCNNConvolutionDescriptor.
--
-- @kernelWeights@ — A pointer to a weights array.  Each entry is a float value. The number of entries is =                                              inputFeatureChannels * outputFeatureChannels * kernelHeight * kernelWidth                                              The layout of filter weight is so that it can be reinterpreted as 4D tensor (array)                                              weight[ outputChannels ][ kernelHeight ][ kernelWidth ][ inputChannels / groups ]                                              Weights are converted to half float (fp16) internally for best performance.
--
-- @biasTerms@ — A pointer to bias terms to be applied to the convolution output.  Each entry is a float value.                                              The number of entries is = numberOfOutputFeatureMaps
--
-- @flags@ — Currently unused. Pass MPSCNNConvolutionFlagsNone
--
-- Returns: A valid MPSCNNConvolution object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:@
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flags :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> RawId -> Const (Id MPSCNNConvolutionDescriptor) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNConvolutionFlags -> IO (Id MPSCNNConvolution)
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flags mpscnnConvolution device convolutionDescriptor kernelWeights biasTerms flags =
  sendOwnedMessage mpscnnConvolution initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector device convolutionDescriptor kernelWeights biasTerms flags

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNConvolution mpscnnConvolution, IsNSCoder aDecoder) => mpscnnConvolution -> aDecoder -> RawId -> IO (Id MPSCNNConvolution)
initWithCoder_device mpscnnConvolution aDecoder device =
  sendOwnedMessage mpscnnConvolution initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> RawId -> IO (Id MPSCNNConvolution)
initWithDevice mpscnnConvolution device =
  sendOwnedMessage mpscnnConvolution initWithDeviceSelector device

-- | Allocate a MPCNNConvolutionGradientSState to hold the results from a -encodeBatchToCommandBuffer... operation
--
-- @sourceImage@ — The MPSImage consumed by the associated -encode call.
--
-- @sourceStates@ — The list of MPSStates consumed by the associated -encode call,                                  for a batch size of 1.
--
-- Returns: The list of states produced by the -encode call for batch size of 1.              -isResultStateReusedAcrossBatch returns YES for MPSCNNConvolution so same              state is used across entire batch. State object is not reusasable across batches.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolution mpscnnConvolution, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnConvolution -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNConvolutionGradientState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnConvolution sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolution resultStateForSourceImage_sourceStates_destinationImageSelector (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolution mpscnnConvolution, IsNSArray sourceStates) => mpscnnConvolution -> RawId -> sourceStates -> RawId -> IO RawId
resultStateBatchForSourceImage_sourceStates_destinationImage mpscnnConvolution sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolution resultStateBatchForSourceImage_sourceStates_destinationImageSelector sourceImage (toNSArray sourceStates) destinationImage

-- | @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolution mpscnnConvolution, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnConvolution -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNConvolutionGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolution commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolution temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolution mpscnnConvolution, IsNSArray sourceStates) => mpscnnConvolution -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolution commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolution temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer sourceImage (toNSArray sourceStates) destinationImage

-- | CPU side reload. Reload the updated weights and biases from data provider into internal weights and bias buffers. Weights and biases              gradients needed for update are obtained from MPSCNNConvolutionGradientState object. Data provider passed in init call is used for this purpose.
--
-- ObjC selector: @- reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSource :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO ()
reloadWeightsAndBiasesFromDataSource mpscnnConvolution =
  sendMessage mpscnnConvolution reloadWeightsAndBiasesFromDataSourceSelector

-- | Deprecated. dataSource will be ignored.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithDataSource:@
reloadWeightsAndBiasesWithDataSource :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> RawId -> IO ()
reloadWeightsAndBiasesWithDataSource mpscnnConvolution dataSource =
  sendMessage mpscnnConvolution reloadWeightsAndBiasesWithDataSourceSelector dataSource

-- | GPU side reload. Reload the updated weights and biases from update buffer produced by application enqueued metal kernel into internal weights              and biases buffer. Weights and biases gradients needed for update are obtained from MPSCNNConvolutionGradientState object's gradientForWeights and gradientForBiases metal buffer.
--
-- @commandBuffer@ — Metal command buffer on which application update kernel was enqueued consuming MPSCNNConvolutionGradientState's gradientForWeights and gradientForBiases buffers                                 and producing updateBuffer metal buffer.
--
-- @state@ — MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffers which have updated weights produced by application's update kernel.                                 The state readcount will be decremented.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_state :: (IsMPSCNNConvolution mpscnnConvolution, IsMPSCNNConvolutionWeightsAndBiasesState state) => mpscnnConvolution -> RawId -> state -> IO ()
reloadWeightsAndBiasesWithCommandBuffer_state mpscnnConvolution commandBuffer state =
  sendMessage mpscnnConvolution reloadWeightsAndBiasesWithCommandBuffer_stateSelector commandBuffer (toMPSCNNConvolutionWeightsAndBiasesState state)

-- | GPU side export. Enqueue a kernel to export current weights and biases stored in MPSCNNConvoltion's internal buffers into weights and biases MTLBuffer              returned in MPSCNNConvolutionWeightsAndBiasesState.
--
-- @commandBuffer@ — Metal command buffer on which export kernel is enqueued.
--
-- @resultStateCanBeTemporary@ — If FALSE, state returned will be non-temporary. If TRUE, returned state may or may not be temporary.
--
-- Returns: MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffer to which weights got exported. This state and be                temporary or non-temporary depending on the flag resultStateCanBeTemporary
--
-- ObjC selector: @- exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:@
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> RawId -> Bool -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary mpscnnConvolution commandBuffer resultStateCanBeTemporary =
  sendMessage mpscnnConvolution exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector commandBuffer resultStateCanBeTemporary

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
inputFeatureChannels mpscnnConvolution =
  sendMessage mpscnnConvolution inputFeatureChannelsSelector

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
outputFeatureChannels mpscnnConvolution =
  sendMessage mpscnnConvolution outputFeatureChannelsSelector

-- | groups
--
-- Number of groups input and output channels are divided into.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
groups mpscnnConvolution =
  sendMessage mpscnnConvolution groupsSelector

-- | dataSource
--
-- dataSource with which convolution object was created
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO RawId
dataSource mpscnnConvolution =
  sendMessage mpscnnConvolution dataSourceSelector

-- | subPixelScaleFactor
--
-- Sub pixel scale factor which was passed in as part of MPSCNNConvolutionDescriptor when creating this MPSCNNConvolution object.
--
-- ObjC selector: @- subPixelScaleFactor@
subPixelScaleFactor :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
subPixelScaleFactor mpscnnConvolution =
  sendMessage mpscnnConvolution subPixelScaleFactorSelector

-- | neuron
--
-- MPSCNNNeuron filter to be applied as part of convolution.              Can be nil in wich case no neuron activation fuction is applied.
--
-- ObjC selector: @- neuron@
neuron :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO RawId
neuron mpscnnConvolution =
  sendMessage mpscnnConvolution neuronSelector

-- | The type of neuron to append to the convolution
--
-- Please see class description for a full list. Default is MPSCNNNeuronTypeNone.
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO MPSCNNNeuronType
neuronType mpscnnConvolution =
  sendMessage mpscnnConvolution neuronTypeSelector

-- | Parameter "a" for the neuron.  Default: 1.0f
--
-- Please see class description for interpretation of a.
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CFloat
neuronParameterA mpscnnConvolution =
  sendMessage mpscnnConvolution neuronParameterASelector

-- | Parameter "b" for the neuron.  Default: 1.0f
--
-- Please see class description for interpretation of b.
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CFloat
neuronParameterB mpscnnConvolution =
  sendMessage mpscnnConvolution neuronParameterBSelector

-- | Parameter "c" for the neuron.  Default: 1.0f
--
-- Please see class description for interpretation of c.
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CFloat
neuronParameterC mpscnnConvolution =
  sendMessage mpscnnConvolution neuronParameterCSelector

-- | Fused neuron descritor passed in convolution descriptor for fusion with convolution.
--
-- Please see class description for interpretation of c.
--
-- ObjC selector: @- fusedNeuronDescriptor@
fusedNeuronDescriptor :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO (Id MPSNNNeuronDescriptor)
fusedNeuronDescriptor mpscnnConvolution =
  sendMessage mpscnnConvolution fusedNeuronDescriptorSelector

-- | Channel multiplier.
--
-- For convolution created with MPSCNNDepthWiseConvolutionDescriptor, it is the number of              output feature channels for each input channel. See MPSCNNDepthWiseConvolutionDescriptor for more details.              Default is 0 which means regular CNN convolution.
--
-- ObjC selector: @- channelMultiplier@
channelMultiplier :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
channelMultiplier mpscnnConvolution =
  sendMessage mpscnnConvolution channelMultiplierSelector

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- accumulatorPrecisionOption@
accumulatorPrecisionOption :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecisionOption mpscnnConvolution =
  sendMessage mpscnnConvolution accumulatorPrecisionOptionSelector

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOption :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> MPSNNConvolutionAccumulatorPrecisionOption -> IO ()
setAccumulatorPrecisionOption mpscnnConvolution value =
  sendMessage mpscnnConvolution setAccumulatorPrecisionOptionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector '[RawId, RawId] (Id MPSCNNConvolution)
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:@
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector :: Selector '[RawId, Const (Id MPSCNNConvolutionDescriptor), Const (Ptr CFloat), Const (Ptr CFloat), MPSCNNConvolutionFlags] (Id MPSCNNConvolution)
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector = mkSelector "initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNConvolution)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNConvolution)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNConvolutionGradientState)
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id NSArray, RawId] RawId
resultStateBatchForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNConvolutionGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, RawId, Id NSArray, RawId] RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSourceSelector :: Selector '[] ()
reloadWeightsAndBiasesFromDataSourceSelector = mkSelector "reloadWeightsAndBiasesFromDataSource"

-- | @Selector@ for @reloadWeightsAndBiasesWithDataSource:@
reloadWeightsAndBiasesWithDataSourceSelector :: Selector '[RawId] ()
reloadWeightsAndBiasesWithDataSourceSelector = mkSelector "reloadWeightsAndBiasesWithDataSource:"

-- | @Selector@ for @reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_stateSelector :: Selector '[RawId, Id MPSCNNConvolutionWeightsAndBiasesState] ()
reloadWeightsAndBiasesWithCommandBuffer_stateSelector = mkSelector "reloadWeightsAndBiasesWithCommandBuffer:state:"

-- | @Selector@ for @exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:@
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector :: Selector '[RawId, Bool] (Id MPSCNNConvolutionWeightsAndBiasesState)
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector = mkSelector "exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector '[] CULong
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector '[] CULong
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @groups@
groupsSelector :: Selector '[] CULong
groupsSelector = mkSelector "groups"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @subPixelScaleFactor@
subPixelScaleFactorSelector :: Selector '[] CULong
subPixelScaleFactorSelector = mkSelector "subPixelScaleFactor"

-- | @Selector@ for @neuron@
neuronSelector :: Selector '[] RawId
neuronSelector = mkSelector "neuron"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector '[] MPSCNNNeuronType
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector '[] CFloat
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector '[] CFloat
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @neuronParameterC@
neuronParameterCSelector :: Selector '[] CFloat
neuronParameterCSelector = mkSelector "neuronParameterC"

-- | @Selector@ for @fusedNeuronDescriptor@
fusedNeuronDescriptorSelector :: Selector '[] (Id MPSNNNeuronDescriptor)
fusedNeuronDescriptorSelector = mkSelector "fusedNeuronDescriptor"

-- | @Selector@ for @channelMultiplier@
channelMultiplierSelector :: Selector '[] CULong
channelMultiplierSelector = mkSelector "channelMultiplier"

-- | @Selector@ for @accumulatorPrecisionOption@
accumulatorPrecisionOptionSelector :: Selector '[] MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecisionOptionSelector = mkSelector "accumulatorPrecisionOption"

-- | @Selector@ for @setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOptionSelector :: Selector '[MPSNNConvolutionAccumulatorPrecisionOption] ()
setAccumulatorPrecisionOptionSelector = mkSelector "setAccumulatorPrecisionOption:"

