{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDevice_weightsSelector
  , initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , resultStateBatchForSourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , reloadWeightsAndBiasesFromDataSourceSelector
  , reloadWeightsAndBiasesWithDataSourceSelector
  , reloadWeightsAndBiasesWithCommandBuffer_stateSelector
  , exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector
  , inputFeatureChannelsSelector
  , outputFeatureChannelsSelector
  , groupsSelector
  , dataSourceSelector
  , subPixelScaleFactorSelector
  , neuronSelector
  , neuronTypeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , fusedNeuronDescriptorSelector
  , channelMultiplierSelector
  , accumulatorPrecisionOptionSelector
  , setAccumulatorPrecisionOptionSelector

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
initWithDevice_weights mpscnnConvolution  device weights =
    sendMsg mpscnnConvolution (mkSelector "initWithDevice:weights:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flags mpscnnConvolution  device convolutionDescriptor kernelWeights biasTerms flags =
  withObjCPtr convolutionDescriptor $ \raw_convolutionDescriptor ->
      sendMsg mpscnnConvolution (mkSelector "initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_convolutionDescriptor :: Ptr ()), argPtr (unConst kernelWeights), argPtr (unConst biasTerms), argCULong (coerce flags)] >>= ownedObject . castPtr

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
initWithCoder_device mpscnnConvolution  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnConvolution (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> RawId -> IO (Id MPSCNNConvolution)
initWithDevice mpscnnConvolution  device =
    sendMsg mpscnnConvolution (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
resultStateForSourceImage_sourceStates_destinationImage mpscnnConvolution  sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnConvolution (mkSelector "resultStateForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolution mpscnnConvolution, IsNSArray sourceStates) => mpscnnConvolution -> RawId -> sourceStates -> RawId -> IO RawId
resultStateBatchForSourceImage_sourceStates_destinationImage mpscnnConvolution  sourceImage sourceStates destinationImage =
  withObjCPtr sourceStates $ \raw_sourceStates ->
      fmap (RawId . castPtr) $ sendMsg mpscnnConvolution (mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolution mpscnnConvolution, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnConvolution -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNConvolutionGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolution  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnConvolution (mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolution mpscnnConvolution, IsNSArray sourceStates) => mpscnnConvolution -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolution  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceStates $ \raw_sourceStates ->
      fmap (RawId . castPtr) $ sendMsg mpscnnConvolution (mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | CPU side reload. Reload the updated weights and biases from data provider into internal weights and bias buffers. Weights and biases              gradients needed for update are obtained from MPSCNNConvolutionGradientState object. Data provider passed in init call is used for this purpose.
--
-- ObjC selector: @- reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSource :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO ()
reloadWeightsAndBiasesFromDataSource mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "reloadWeightsAndBiasesFromDataSource") retVoid []

-- | Deprecated. dataSource will be ignored.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithDataSource:@
reloadWeightsAndBiasesWithDataSource :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> RawId -> IO ()
reloadWeightsAndBiasesWithDataSource mpscnnConvolution  dataSource =
    sendMsg mpscnnConvolution (mkSelector "reloadWeightsAndBiasesWithDataSource:") retVoid [argPtr (castPtr (unRawId dataSource) :: Ptr ())]

-- | GPU side reload. Reload the updated weights and biases from update buffer produced by application enqueued metal kernel into internal weights              and biases buffer. Weights and biases gradients needed for update are obtained from MPSCNNConvolutionGradientState object's gradientForWeights and gradientForBiases metal buffer.
--
-- @commandBuffer@ — Metal command buffer on which application update kernel was enqueued consuming MPSCNNConvolutionGradientState's gradientForWeights and gradientForBiases buffers                                 and producing updateBuffer metal buffer.
--
-- @state@ — MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffers which have updated weights produced by application's update kernel.                                 The state readcount will be decremented.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_state :: (IsMPSCNNConvolution mpscnnConvolution, IsMPSCNNConvolutionWeightsAndBiasesState state) => mpscnnConvolution -> RawId -> state -> IO ()
reloadWeightsAndBiasesWithCommandBuffer_state mpscnnConvolution  commandBuffer state =
  withObjCPtr state $ \raw_state ->
      sendMsg mpscnnConvolution (mkSelector "reloadWeightsAndBiasesWithCommandBuffer:state:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_state :: Ptr ())]

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
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary mpscnnConvolution  commandBuffer resultStateCanBeTemporary =
    sendMsg mpscnnConvolution (mkSelector "exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (if resultStateCanBeTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
inputFeatureChannels mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "inputFeatureChannels") retCULong []

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
outputFeatureChannels mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "outputFeatureChannels") retCULong []

-- | groups
--
-- Number of groups input and output channels are divided into.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
groups mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "groups") retCULong []

-- | dataSource
--
-- dataSource with which convolution object was created
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO RawId
dataSource mpscnnConvolution  =
    fmap (RawId . castPtr) $ sendMsg mpscnnConvolution (mkSelector "dataSource") (retPtr retVoid) []

-- | subPixelScaleFactor
--
-- Sub pixel scale factor which was passed in as part of MPSCNNConvolutionDescriptor when creating this MPSCNNConvolution object.
--
-- ObjC selector: @- subPixelScaleFactor@
subPixelScaleFactor :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
subPixelScaleFactor mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "subPixelScaleFactor") retCULong []

-- | neuron
--
-- MPSCNNNeuron filter to be applied as part of convolution.              Can be nil in wich case no neuron activation fuction is applied.
--
-- ObjC selector: @- neuron@
neuron :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO RawId
neuron mpscnnConvolution  =
    fmap (RawId . castPtr) $ sendMsg mpscnnConvolution (mkSelector "neuron") (retPtr retVoid) []

-- | The type of neuron to append to the convolution
--
-- Please see class description for a full list. Default is MPSCNNNeuronTypeNone.
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO MPSCNNNeuronType
neuronType mpscnnConvolution  =
    fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpscnnConvolution (mkSelector "neuronType") retCInt []

-- | Parameter "a" for the neuron.  Default: 1.0f
--
-- Please see class description for interpretation of a.
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CFloat
neuronParameterA mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "neuronParameterA") retCFloat []

-- | Parameter "b" for the neuron.  Default: 1.0f
--
-- Please see class description for interpretation of b.
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CFloat
neuronParameterB mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "neuronParameterB") retCFloat []

-- | Parameter "c" for the neuron.  Default: 1.0f
--
-- Please see class description for interpretation of c.
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CFloat
neuronParameterC mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "neuronParameterC") retCFloat []

-- | Fused neuron descritor passed in convolution descriptor for fusion with convolution.
--
-- Please see class description for interpretation of c.
--
-- ObjC selector: @- fusedNeuronDescriptor@
fusedNeuronDescriptor :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO (Id MPSNNNeuronDescriptor)
fusedNeuronDescriptor mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "fusedNeuronDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Channel multiplier.
--
-- For convolution created with MPSCNNDepthWiseConvolutionDescriptor, it is the number of              output feature channels for each input channel. See MPSCNNDepthWiseConvolutionDescriptor for more details.              Default is 0 which means regular CNN convolution.
--
-- ObjC selector: @- channelMultiplier@
channelMultiplier :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO CULong
channelMultiplier mpscnnConvolution  =
    sendMsg mpscnnConvolution (mkSelector "channelMultiplier") retCULong []

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- accumulatorPrecisionOption@
accumulatorPrecisionOption :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> IO MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecisionOption mpscnnConvolution  =
    fmap (coerce :: CULong -> MPSNNConvolutionAccumulatorPrecisionOption) $ sendMsg mpscnnConvolution (mkSelector "accumulatorPrecisionOption") retCULong []

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOption :: IsMPSCNNConvolution mpscnnConvolution => mpscnnConvolution -> MPSNNConvolutionAccumulatorPrecisionOption -> IO ()
setAccumulatorPrecisionOption mpscnnConvolution  value =
    sendMsg mpscnnConvolution (mkSelector "setAccumulatorPrecisionOption:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:@
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector :: Selector
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector = mkSelector "initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImageSelector :: Selector
resultStateBatchForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSourceSelector :: Selector
reloadWeightsAndBiasesFromDataSourceSelector = mkSelector "reloadWeightsAndBiasesFromDataSource"

-- | @Selector@ for @reloadWeightsAndBiasesWithDataSource:@
reloadWeightsAndBiasesWithDataSourceSelector :: Selector
reloadWeightsAndBiasesWithDataSourceSelector = mkSelector "reloadWeightsAndBiasesWithDataSource:"

-- | @Selector@ for @reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_stateSelector :: Selector
reloadWeightsAndBiasesWithCommandBuffer_stateSelector = mkSelector "reloadWeightsAndBiasesWithCommandBuffer:state:"

-- | @Selector@ for @exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:@
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector :: Selector
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector = mkSelector "exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @subPixelScaleFactor@
subPixelScaleFactorSelector :: Selector
subPixelScaleFactorSelector = mkSelector "subPixelScaleFactor"

-- | @Selector@ for @neuron@
neuronSelector :: Selector
neuronSelector = mkSelector "neuron"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @neuronParameterC@
neuronParameterCSelector :: Selector
neuronParameterCSelector = mkSelector "neuronParameterC"

-- | @Selector@ for @fusedNeuronDescriptor@
fusedNeuronDescriptorSelector :: Selector
fusedNeuronDescriptorSelector = mkSelector "fusedNeuronDescriptor"

-- | @Selector@ for @channelMultiplier@
channelMultiplierSelector :: Selector
channelMultiplierSelector = mkSelector "channelMultiplier"

-- | @Selector@ for @accumulatorPrecisionOption@
accumulatorPrecisionOptionSelector :: Selector
accumulatorPrecisionOptionSelector = mkSelector "accumulatorPrecisionOption"

-- | @Selector@ for @setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOptionSelector :: Selector
setAccumulatorPrecisionOptionSelector = mkSelector "setAccumulatorPrecisionOption:"

