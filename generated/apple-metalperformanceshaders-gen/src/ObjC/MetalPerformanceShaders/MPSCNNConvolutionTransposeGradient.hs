{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolutionTransposeGradient
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionTransposeGradient implementents backward propagation of gradient for MPSCNNConvolutionTranspose forward filter
--
-- Generated bindings for @MPSCNNConvolutionTransposeGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionTransposeGradient
  ( MPSCNNConvolutionTransposeGradient
  , IsMPSCNNConvolutionTransposeGradient(..)
  , initWithDevice_weights
  , initWithCoder_device
  , initWithDevice
  , reloadWeightsAndBiasesFromDataSource
  , reloadWeightsAndBiasesWithCommandBuffer_state
  , sourceGradientFeatureChannels
  , sourceImageFeatureChannels
  , groups
  , dataSource
  , gradientOption
  , setGradientOption
  , dataSourceSelector
  , gradientOptionSelector
  , groupsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_weightsSelector
  , reloadWeightsAndBiasesFromDataSourceSelector
  , reloadWeightsAndBiasesWithCommandBuffer_stateSelector
  , setGradientOptionSelector
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

-- | Initializes a convolution transpose gradient (with respect to weights and bias) object.
--
-- @device@ — The MTLDevice on which this MPSCNNConvolutionGradient filter will be used
--
-- @weights@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource                                              protocol. Note that same data source as provided to forward convolution should be used.
--
-- Returns: A valid MPSCNNConvolutionTransposeGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:weights:@
initWithDevice_weights :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> RawId -> RawId -> IO (Id MPSCNNConvolutionTransposeGradient)
initWithDevice_weights mpscnnConvolutionTransposeGradient device weights =
  sendOwnedMessage mpscnnConvolutionTransposeGradient initWithDevice_weightsSelector device weights

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
initWithCoder_device :: (IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient, IsNSCoder aDecoder) => mpscnnConvolutionTransposeGradient -> aDecoder -> RawId -> IO (Id MPSCNNConvolutionTransposeGradient)
initWithCoder_device mpscnnConvolutionTransposeGradient aDecoder device =
  sendOwnedMessage mpscnnConvolutionTransposeGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> RawId -> IO (Id MPSCNNConvolutionTransposeGradient)
initWithDevice mpscnnConvolutionTransposeGradient device =
  sendOwnedMessage mpscnnConvolutionTransposeGradient initWithDeviceSelector device

-- | CPU side reload. Reload the updated weights and biases from data provider into internal weights and bias buffers. Weights and biases              gradients needed for update are obtained from MPSCNNConvolutionGradientState object. Data provider passed in init call is used for this purpose.
--
-- ObjC selector: @- reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSource :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> IO ()
reloadWeightsAndBiasesFromDataSource mpscnnConvolutionTransposeGradient =
  sendMessage mpscnnConvolutionTransposeGradient reloadWeightsAndBiasesFromDataSourceSelector

-- | GPU side reload. Reload the updated weights and biases from update buffer produced by application enqueued metal kernel into internal weights              and biases buffer. Weights and biases gradients needed for update are obtained from MPSCNNConvolutionGradientState object's gradientForWeights and gradientForBiases metal buffer.
--
-- @commandBuffer@ — Metal command buffer on which application update kernel was enqueued consuming MPSCNNConvolutionGradientState's gradientForWeights and gradientForBiases buffer                                 and producing updateBuffer metal buffer.
--
-- @state@ — MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffers which have updated weights produced by application's update kernel.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_state :: (IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient, IsMPSCNNConvolutionWeightsAndBiasesState state) => mpscnnConvolutionTransposeGradient -> RawId -> state -> IO ()
reloadWeightsAndBiasesWithCommandBuffer_state mpscnnConvolutionTransposeGradient commandBuffer state =
  sendMessage mpscnnConvolutionTransposeGradient reloadWeightsAndBiasesWithCommandBuffer_stateSelector commandBuffer (toMPSCNNConvolutionWeightsAndBiasesState state)

-- | sourceGradientFeatureChannels
--
-- The number of feature channels per pixel in the gradient image (primarySource) of encode call. This is same is outputFeatureChannels              or the feature channels of destination image in forward convolution i.e. dataSource.descriptor.outputFeatureChannels
--
-- ObjC selector: @- sourceGradientFeatureChannels@
sourceGradientFeatureChannels :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> IO CULong
sourceGradientFeatureChannels mpscnnConvolutionTransposeGradient =
  sendMessage mpscnnConvolutionTransposeGradient sourceGradientFeatureChannelsSelector

-- | sourceImageFeatureChannels
--
-- The number of feature channels per pixel in the input image to forward convolution which is used here as secondarySource.              This is same as dataSource.descriptor.inputFeatureChannels. This is also the number of feature channels in destinatin image              here i.e. gradient with respect to data.
--
-- ObjC selector: @- sourceImageFeatureChannels@
sourceImageFeatureChannels :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> IO CULong
sourceImageFeatureChannels mpscnnConvolutionTransposeGradient =
  sendMessage mpscnnConvolutionTransposeGradient sourceImageFeatureChannelsSelector

-- | groups
--
-- Number of groups input and output channels are divided into.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> IO CULong
groups mpscnnConvolutionTransposeGradient =
  sendMessage mpscnnConvolutionTransposeGradient groupsSelector

-- | dataSource
--
-- dataSource with which gradient object was created
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> IO RawId
dataSource mpscnnConvolutionTransposeGradient =
  sendMessage mpscnnConvolutionTransposeGradient dataSourceSelector

-- | gradientOption
--
-- Option to control which gradient to compute. Default is MPSCNNConvolutionGradientOptionAll              which means both gradient with respect to data and gradient with respect to weight and bias are computed.
--
-- ObjC selector: @- gradientOption@
gradientOption :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> IO MPSCNNConvolutionGradientOption
gradientOption mpscnnConvolutionTransposeGradient =
  sendMessage mpscnnConvolutionTransposeGradient gradientOptionSelector

-- | gradientOption
--
-- Option to control which gradient to compute. Default is MPSCNNConvolutionGradientOptionAll              which means both gradient with respect to data and gradient with respect to weight and bias are computed.
--
-- ObjC selector: @- setGradientOption:@
setGradientOption :: IsMPSCNNConvolutionTransposeGradient mpscnnConvolutionTransposeGradient => mpscnnConvolutionTransposeGradient -> MPSCNNConvolutionGradientOption -> IO ()
setGradientOption mpscnnConvolutionTransposeGradient value =
  sendMessage mpscnnConvolutionTransposeGradient setGradientOptionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector '[RawId, RawId] (Id MPSCNNConvolutionTransposeGradient)
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNConvolutionTransposeGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNConvolutionTransposeGradient)
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

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @gradientOption@
gradientOptionSelector :: Selector '[] MPSCNNConvolutionGradientOption
gradientOptionSelector = mkSelector "gradientOption"

-- | @Selector@ for @setGradientOption:@
setGradientOptionSelector :: Selector '[MPSCNNConvolutionGradientOption] ()
setGradientOptionSelector = mkSelector "setGradientOption:"

