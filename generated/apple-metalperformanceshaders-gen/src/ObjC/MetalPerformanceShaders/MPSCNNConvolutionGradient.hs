{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolutionGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionGradient implementents backward propagation of gradient i.e. it computes the gradient of loss function              with respect input data of corresonding forward convolution and gradient of loss function with respect to weights and bias              of corresponding convolution in forward pass.
--
-- Gradient with respect to data              ==============================              Gradient with respect to input data of corresponding forward convolution will be written in destination image passed to              encode call of MPSCNNConvolutionGradient.              This step is similar to convolution transpose in that the strided convolution in forward pass become zero filled convolution in              backward propagation of gradients. The difference between MPSCNNConvolutionTranspose and gradient wrt data is how the              weights, that are provided by data source, are interpreted. MPSCNNConvolution and MPSCNNConvolutionTranspose interpret weights              provided by data source as                                          weights[outputFeatureChannels][kernelWidth][kernelHeight][inputFeatureChannels]              whereas convoution gradient with respect to data interpret the weights as                                          weights[inputFeatureChannels][kernelWidth][kernelHeight][outputFeatureChannels]              i.e. weights are transposed in inputFeatureChannels/outputFeatureChannels dimension and also rotated 180 degress in spatial dimension
--
-- User should use the same data source provider to initialize MPSCNNConvolutionGradient as is used to initialize corresponding              forward MPSCNNConvolution. Implementation will do the transposition/shuffling needed.              Thus, while the forward MPSCNNConvolution takes sourceImage of inputFeatureChannels and convolves it with              Wt[outputFeatureChannels][kernelHeight][kernelWidth][inputFeatureChannels] to produce destinationImage of outputFeatureChannels,              MPSConvolutionGradient takes sourceGradient of outputFeatureChannels which is out of previous layer (nomally neuron backward layer),              convolves it with transposed and rotated weights and produces destinationGradient of inputFeatureChannels.              If the user decide to double buffer data source provider i.e. different data source providers are passed to forward MPSCNNConvolution object and              corresponding MPSCNNConvolutionGradient object, it is user responsibility to make sure both data source providers provide same weights/bias data              and have same properties in convolution descriptor else behavior is undefined.
--
-- Gradient with respect to weights and bias              =========================================              Gradient with respect to weights and bias are returned in MPSCNNConvolutionGradientState object to be used in weights update functions.              If I denotes the input image to corresponding MPSCNNConvolution in forward pass and E denoates the loss gradient from previous layer              (normally neuron backward layer) in backward pass, gradient of E with respect to weights is
--
-- delta_E/delta_Wkpqc = sum_i sum_j [ E(i, j, k) * I( secondaryStrideInPixelX*i + secondaryOffset.x + secondaryDilationRateX*p,                                                                  secondaryStrideinPixelY*i + secondaryOffset.y + secondaryDilationRateY*q, c) ]
--
-- where i goes over 0..W-1 and j goes over 0..H-1, (W,H) being width and height of E.              p in [0, secondaryKernelWidth-1]              q in [0, secondaryKernelHeight-1]              c in [0, inputeFeatureChannels/groups - 1]              k in [0, outputFeatureChannels]
--
-- and gradient with respect to bias
--
-- delta_E/delta_bk = sum_i sum_j [ E(i, j, k) ]
--
-- These gradients with respect to weights and bias are returned as buffers in MPSCNNConvolutionGradientState object passed in the encode call.              These are consumed by MPSCNNConvolution object's -updateWeightsAndBias:MPSCNNConvolutionGradientState* method for CPU side update and              encodeWeightsAndBiasUpdate:commandBuffer:MPSCNNConvolutionGradientState* method of MPSCNNConvolution object for GPU side update.              UPdated weights and biases are computed as
--
-- Wkpqc_new = Wkpqc_old + delta_E/delta_Wkpqc                         bk_new = bk_old + delta_E/delta_bk
--
-- Note that MPSCNNConvolutionGradientState objects's buffers that contain gradients, for CPU side update, will only contain              valid data after command buffer is complete so              its only makes sense to call -updateWeightsAndBias method on MPSCNNConvolution objects after command bufer is              complete. One can achieve this by enqueueing a command buffer completion handler block that make this call.              Since MPSCNNConvolutionGradientState is used across command buffers i.e. its created in forward pass, consumed by  MPSCNNConvolutionGradient in backward pass in same command buffer and passed onto MPSCNNConvolution updateWeightsAndBias method              after completion of command buffer, it cannot be a temporary state.
--
-- In order to gaurantee consistency between forward pass (MPSCNNConvolution) and weights gradient computation in this filter, certain requirements              must be met.              1) Dimensions of loss gradient E from previous layer in backward pass must be equal to clipRect.size of corresponding MPSCNNConvolution in forward pass.                 This is to gaurantee that only those pixels for which weights/bias contributed in destination of forward pass end up contributing to weights/bias gradient update.                 If the dimension of loss gradient E from previous layer is not equal to clipRect.size of corresponding forward MPSCNNConvolution,                    i) one can insert a slice operation to extract out the region of size clipRect.size from appropriate offset in E and set primaryOffset = 0 Or                   ii) set primatryOffset to offset in E at which valid data starts and make sure data outside is zeroed.              2) secondaryOffset should be set to what offset property of MPSCNNConvolution was set to in forward pass.
--
-- Currently back propagation for gradients is only supported for regualar convolution and depthwise convolution. Back propagation              sub-pixel convolution are not supported. So channelMultiplier and subPixelScaleFactor must be one.
--
-- Note on setting correct offsets  ===============================              If the forward convolution is called with                              offset = _offset; kernelWidth = kW; kernelHeight = kH; strideInPixelsX = sX; strideInPixelsY = sY;                              dilationRateX = dX; dilationRateY = dY;              thus dilated filter parameters are                              kW_Dilated = (kW - 1)*dX + 1; kH_Dilated = (kH - 1)*dY + 1;              Then the correct offset can be computed as follows.              Convoluton Gradient with Data              =============================                Convolution gradient with data of forward convolution with stride > 1 is essentially normal convoution with unit stride,                on an image that is formed by inserting strideInPixelsX-1 zeros in between each column and strideInPixelsY-1 zeros in between each                row of input gradient (output gradient of last layer) with kernel weights that are rotated by 180 degrees in spatial dimension (MPSCNNConvolutionGradient                does this rotation internally). primaryOffset property defines offset in original input gradient coordinate system. In order to                translate it in zero filled intermediate image coordinate system, kernelOffsetX and kernelOffsetY properties can be used as follows                       offsetInZeroFilledImageX = primaryOffset.x * primaryStrideInPixelsX + kernelOffsetX;                       offsetInZeroFilledImageY = primaryOffset.y * primaryStrideInPixelsY + kernelOffsetY;                This is what internally MPSCNNConvolutionGradient do. In order to correctly match forward convolution offset setting (so that padding policy is                consistent), application should set                       primaryOffset.x = 0; primaryOffset.y = 0;                       kernelOffset.x = -_offset.x + (~(NSInteger) kW_Dilated & 1L);                       kernelOffset.y = -_offset.y + (~(NSInteger) kH_Dilated & 1L);                Convolution gradient with data does not use secondaryOffset.
--
-- Convolution Gradient with Weights and Biases                ============================================                 For consistent padding policy with respect to forward convolution,                       secondaryOffset.x = _offset.x - kW_Dilated/2                       secondaryOffset.y = _offset.y - kH_Dilated/2                 Convolution gradient with weights and biases does not use primaryOffset (or it is assumed to be zero) as summation is over entire                 gradient image and only gradient image without any padding is currently accepted. If previous layer produces gradient image with                 padding, slice operation should be used to extract out the gradient which will be input to MPSCNNConvolutionGradient.
--
-- Note that if application uses encode method that return destination gradient on left hand side and consumes MPSCNNConvolutionGradientState             object produced by forward MPSCNNConvolution, all these parameters are set automatically for the application i.e. applicaiton does not             need to worry about setting these.
--
-- Generated bindings for @MPSCNNConvolutionGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionGradient
  ( MPSCNNConvolutionGradient
  , IsMPSCNNConvolutionGradient(..)
  , initWithDevice_weights
  , initWithCoder_device
  , initWithDevice
  , reloadWeightsAndBiasesFromDataSource
  , reloadWeightsAndBiasesWithCommandBuffer_state
  , sourceGradientFeatureChannels
  , sourceImageFeatureChannels
  , groups
  , channelMultiplier
  , dataSource
  , gradientOption
  , setGradientOption
  , serializeWeightsAndBiases
  , setSerializeWeightsAndBiases
  , channelMultiplierSelector
  , dataSourceSelector
  , gradientOptionSelector
  , groupsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_weightsSelector
  , reloadWeightsAndBiasesFromDataSourceSelector
  , reloadWeightsAndBiasesWithCommandBuffer_stateSelector
  , serializeWeightsAndBiasesSelector
  , setGradientOptionSelector
  , setSerializeWeightsAndBiasesSelector
  , sourceGradientFeatureChannelsSelector
  , sourceImageFeatureChannelsSelector

  -- * Enum types
  , MPSCNNConvolutionGradientOption(MPSCNNConvolutionGradientOption)
  , pattern MPSCNNConvolutionGradientOptionGradientWithData
  , pattern MPSCNNConvolutionGradientOptionGradientWithWeightsAndBias
  , pattern MPSCNNConvolutionGradientOptionAll

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

-- | Initializes a convolution gradient (with respect to weights and bias) object.
--
-- @device@ — The MTLDevice on which this MPSCNNConvolutionGradient filter will be used
--
-- @weights@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource                                              protocol. Note that same data source as provided to forward convolution should be used.
--
-- Returns: A valid MPSCNNConvolutionGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:weights:@
initWithDevice_weights :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> RawId -> RawId -> IO (Id MPSCNNConvolutionGradient)
initWithDevice_weights mpscnnConvolutionGradient device weights =
  sendOwnedMessage mpscnnConvolutionGradient initWithDevice_weightsSelector device weights

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
initWithCoder_device :: (IsMPSCNNConvolutionGradient mpscnnConvolutionGradient, IsNSCoder aDecoder) => mpscnnConvolutionGradient -> aDecoder -> RawId -> IO (Id MPSCNNConvolutionGradient)
initWithCoder_device mpscnnConvolutionGradient aDecoder device =
  sendOwnedMessage mpscnnConvolutionGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> RawId -> IO (Id MPSCNNConvolutionGradient)
initWithDevice mpscnnConvolutionGradient device =
  sendOwnedMessage mpscnnConvolutionGradient initWithDeviceSelector device

-- | CPU side reload. Reload the updated weights and biases from data provider into internal weights and bias buffers. Weights and biases              gradients needed for update are obtained from MPSCNNConvolutionGradientState object. Data provider passed in init call is used for this purpose.
--
-- ObjC selector: @- reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSource :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO ()
reloadWeightsAndBiasesFromDataSource mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient reloadWeightsAndBiasesFromDataSourceSelector

-- | GPU side reload. Reload the updated weights and biases from update buffer produced by application enqueued metal kernel into internal weights              and biases buffer. Weights and biases gradients needed for update are obtained from MPSCNNConvolutionGradientState object's gradientForWeights and gradientForBiases metal buffer.
--
-- @commandBuffer@ — Metal command buffer on which application update kernel was enqueued consuming MPSCNNConvolutionGradientState's gradientForWeights and gradientForBiases buffer                                 and producing updateBuffer metal buffer.
--
-- @state@ — MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffers which have updated weights produced by application's update kernel.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_state :: (IsMPSCNNConvolutionGradient mpscnnConvolutionGradient, IsMPSCNNConvolutionWeightsAndBiasesState state) => mpscnnConvolutionGradient -> RawId -> state -> IO ()
reloadWeightsAndBiasesWithCommandBuffer_state mpscnnConvolutionGradient commandBuffer state =
  sendMessage mpscnnConvolutionGradient reloadWeightsAndBiasesWithCommandBuffer_stateSelector commandBuffer (toMPSCNNConvolutionWeightsAndBiasesState state)

-- | sourceGradientFeatureChannels
--
-- The number of feature channels per pixel in the gradient image (primarySource) of encode call. This is same is outputFeatureChannels              or the feature channels of destination image in forward convolution i.e. dataSource.descriptor.outputFeatureChannels
--
-- ObjC selector: @- sourceGradientFeatureChannels@
sourceGradientFeatureChannels :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO CULong
sourceGradientFeatureChannels mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient sourceGradientFeatureChannelsSelector

-- | sourceImageFeatureChannels
--
-- The number of feature channels per pixel in the input image to forward convolution which is used here as secondarySource.              This is same as dataSource.descriptor.inputFeatureChannels. This is also the number of feature channels in destinatin image              here i.e. gradient with respect to data.
--
-- ObjC selector: @- sourceImageFeatureChannels@
sourceImageFeatureChannels :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO CULong
sourceImageFeatureChannels mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient sourceImageFeatureChannelsSelector

-- | groups
--
-- Number of groups input and output channels are divided into.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO CULong
groups mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient groupsSelector

-- | Channel multiplier.
--
-- For convolution created with MPSCNNDepthWiseConvolutionDescriptor, it is the number of              output feature channels for each input channel. See MPSCNNDepthWiseConvolutionDescriptor for more details.              Default is 0 which means regular CNN convolution. Currently only channelMultiplier of 1 is supported i.e. inputChannels == outputChannels
--
-- ObjC selector: @- channelMultiplier@
channelMultiplier :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO CULong
channelMultiplier mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient channelMultiplierSelector

-- | dataSource
--
-- dataSource with which gradient object was created
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO RawId
dataSource mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient dataSourceSelector

-- | gradientOption
--
-- Option to control which gradient to compute. Default is MPSCNNConvolutionGradientOptionAll              which means both gradient with respect to data and gradient with respect to weight and bias are computed.
--
-- ObjC selector: @- gradientOption@
gradientOption :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO MPSCNNConvolutionGradientOption
gradientOption mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient gradientOptionSelector

-- | gradientOption
--
-- Option to control which gradient to compute. Default is MPSCNNConvolutionGradientOptionAll              which means both gradient with respect to data and gradient with respect to weight and bias are computed.
--
-- ObjC selector: @- setGradientOption:@
setGradientOption :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> MPSCNNConvolutionGradientOption -> IO ()
setGradientOption mpscnnConvolutionGradient value =
  sendMessage mpscnnConvolutionGradient setGradientOptionSelector value

-- | Property to control serialization of weights and bias.
--
-- During serialization of convolution object in -encodeWithCoder call, weights and biases are saved so that convolution               object can be properly unserialized/restored in -initWithCoder call. If data source provied is NSSecureCoding compliant,               data source is serialized else weights and biases are serialized.               As weights/biases data may be several MB and these are same for both gradient and forward convolution object,               application may already have weights/biases on disk through convolution, it can               save disk space by setting this property false so convolution gradient object does not end up storing another copy of weights/biases.               Default is NO. When application decides to set it to NO, it MUST call                              -(void) reloadWeightsAndBiasesFromDataSource               after initWithCoder has initialized convolution object.
--
-- ObjC selector: @- serializeWeightsAndBiases@
serializeWeightsAndBiases :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> IO Bool
serializeWeightsAndBiases mpscnnConvolutionGradient =
  sendMessage mpscnnConvolutionGradient serializeWeightsAndBiasesSelector

-- | Property to control serialization of weights and bias.
--
-- During serialization of convolution object in -encodeWithCoder call, weights and biases are saved so that convolution               object can be properly unserialized/restored in -initWithCoder call. If data source provied is NSSecureCoding compliant,               data source is serialized else weights and biases are serialized.               As weights/biases data may be several MB and these are same for both gradient and forward convolution object,               application may already have weights/biases on disk through convolution, it can               save disk space by setting this property false so convolution gradient object does not end up storing another copy of weights/biases.               Default is NO. When application decides to set it to NO, it MUST call                              -(void) reloadWeightsAndBiasesFromDataSource               after initWithCoder has initialized convolution object.
--
-- ObjC selector: @- setSerializeWeightsAndBiases:@
setSerializeWeightsAndBiases :: IsMPSCNNConvolutionGradient mpscnnConvolutionGradient => mpscnnConvolutionGradient -> Bool -> IO ()
setSerializeWeightsAndBiases mpscnnConvolutionGradient value =
  sendMessage mpscnnConvolutionGradient setSerializeWeightsAndBiasesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector '[RawId, RawId] (Id MPSCNNConvolutionGradient)
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNConvolutionGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNConvolutionGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSourceSelector :: Selector '[] ()
reloadWeightsAndBiasesFromDataSourceSelector = mkSelector "reloadWeightsAndBiasesFromDataSource"

-- | @Selector@ for @reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_stateSelector :: Selector '[RawId, Id MPSCNNConvolutionWeightsAndBiasesState] ()
reloadWeightsAndBiasesWithCommandBuffer_stateSelector = mkSelector "reloadWeightsAndBiasesWithCommandBuffer:state:"

-- | @Selector@ for @sourceGradientFeatureChannels@
sourceGradientFeatureChannelsSelector :: Selector '[] CULong
sourceGradientFeatureChannelsSelector = mkSelector "sourceGradientFeatureChannels"

-- | @Selector@ for @sourceImageFeatureChannels@
sourceImageFeatureChannelsSelector :: Selector '[] CULong
sourceImageFeatureChannelsSelector = mkSelector "sourceImageFeatureChannels"

-- | @Selector@ for @groups@
groupsSelector :: Selector '[] CULong
groupsSelector = mkSelector "groups"

-- | @Selector@ for @channelMultiplier@
channelMultiplierSelector :: Selector '[] CULong
channelMultiplierSelector = mkSelector "channelMultiplier"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @gradientOption@
gradientOptionSelector :: Selector '[] MPSCNNConvolutionGradientOption
gradientOptionSelector = mkSelector "gradientOption"

-- | @Selector@ for @setGradientOption:@
setGradientOptionSelector :: Selector '[MPSCNNConvolutionGradientOption] ()
setGradientOptionSelector = mkSelector "setGradientOption:"

-- | @Selector@ for @serializeWeightsAndBiases@
serializeWeightsAndBiasesSelector :: Selector '[] Bool
serializeWeightsAndBiasesSelector = mkSelector "serializeWeightsAndBiases"

-- | @Selector@ for @setSerializeWeightsAndBiases:@
setSerializeWeightsAndBiasesSelector :: Selector '[Bool] ()
setSerializeWeightsAndBiasesSelector = mkSelector "setSerializeWeightsAndBiases:"

