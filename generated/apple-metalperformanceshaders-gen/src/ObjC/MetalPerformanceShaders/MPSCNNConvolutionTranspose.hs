{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolutionTranspose
--
-- This depends on Metal.framework
--
-- The MPSCNNConvolutionTranspose specifies a transposed convolution.              The MPSCNNConvolutionTranspose convolves the input image with a set of filters, each producing one feature map in the output image.
--
-- Some third-party frameworks may rotate the weights spatially by 180 degrees for Convolution Transpose. MPS uses the weights              specified by the developer as-is and does not perform any rotation. The developer may need to rotate the weights appropriately              in case this rotation is needed before the convolution transpose is applied.
--
-- When the stride in any dimension is greater than 1, the convolution transpose puts (stride - 1) zeroes in-between the source               image pixels to create an expanded image. Then a convolution is done over the expanded image to generate the output of the               convolution transpose.
--
-- Intermediate image size = (srcSize - 1) * Stride + 1
--
-- Examples:
--
-- So in case of sride == 2 (this behaves same in both dimensions)
--
-- Source image:
-- _______________
-- |   |   |   |   |
-- | 1 | 2 | 3 | 4 |
-- |   |   |   |   |
-- ---------------
--
-- Intermediate Image:
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   |   |   |   |   |   |   |
-- ---------------------------
--
-- NOTE on Offset:
-- There are 2 types of offsets defined:
-- 1) The Offset defined in MPSCNNKernel from which MPSCNNConvolutionTranspose inherits. This offset is applied to from where
-- the kernel will be applied on the source.
-- 2) The kernelOffsetX and kernelOffsetY which is the offset applied to the kernel when it is finally applied on the intermediate
-- image.
--
-- So totalOffset = Offset * stride + kernelOffset
--
-- The offset defined by user refers to the coordinate frame of the expanded image
-- (we are showing only 1 dimension X it can be extended to Y dimension as well) :
--
-- X indicates where the convolution transpose begins:
--
-- Intermediate Image:  Offset = 0, kernelOffset = 0
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- | X |   |   |   |   |   |   |
-- ---------------------------
--
-- X indicates where the convolution transpose begins:
--
-- Intermediate Image:  Offset = 0, kernelOffset = 1
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   | X |   |   |   |   |   |
-- ---------------------------
--
-- X indicates where the convolution transpose begins:
--
-- Intermediate Image:  Offset = 0, kernelOffset = -1
-- ___________________________
-- |   |   |   |   |   |   |   |
-- X | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   |   |   |   |   |   |   |
-- ---------------------------
--
-- So if the user wanted to apply an offset of 2 on the source image of convolution transpose:
--
-- Source image:
-- _______________
-- |   |   |   |   |
-- | 1 | 2 | 3 | 4 |
-- |   |   | X |   |
-- ---------------
--
-- offset = 2, kernelOffset = 0
--
-- Intermediate Image:
-- ___________________________
-- |   |   |   |   |   |   |   |
-- | 1 | 0 | 2 | 0 | 3 | 0 | 4 |
-- |   |   |   |   | X |   |   |
-- ---------------------------
--
-- Note that if your application is not using MPSCNNConvolutionGradientState to configure the convolution transpose with respect to convolution,      your application may do this using padding policy. In such case if convolution uses valid padding policy, than convolution transpose should use      full padding policy and vice vera. Full padding remains full.
--
-- Generated bindings for @MPSCNNConvolutionTranspose@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionTranspose
  ( MPSCNNConvolutionTranspose
  , IsMPSCNNConvolutionTranspose(..)
  , initWithDevice_weights
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImage_convolutionGradientState
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates
  , encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImage
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImages
  , resultStateForSourceImage_sourceStates_destinationImage
  , resultStateBatchForSourceImage_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage
  , reloadWeightsAndBiasesFromDataSource
  , reloadWeightsAndBiasesWithCommandBuffer_state
  , exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary
  , encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporary
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporary
  , inputFeatureChannels
  , outputFeatureChannels
  , kernelOffsetX
  , setKernelOffsetX
  , kernelOffsetY
  , setKernelOffsetY
  , groups
  , accumulatorPrecisionOption
  , setAccumulatorPrecisionOption
  , dataSource
  , accumulatorPrecisionOptionSelector
  , dataSourceSelector
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStatesSelector
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporarySelector
  , encodeToCommandBuffer_sourceImage_convolutionGradientStateSelector
  , encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporarySelector
  , exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector
  , groupsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_weightsSelector
  , inputFeatureChannelsSelector
  , kernelOffsetXSelector
  , kernelOffsetYSelector
  , outputFeatureChannelsSelector
  , reloadWeightsAndBiasesFromDataSourceSelector
  , reloadWeightsAndBiasesWithCommandBuffer_stateSelector
  , resultStateBatchForSourceImage_sourceStates_destinationImageSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , setAccumulatorPrecisionOptionSelector
  , setKernelOffsetXSelector
  , setKernelOffsetYSelector
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector

  -- * Enum types
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

-- | Initializes a convolution transpose kernel
--
-- @device@ — The MTLDevice on which this MPSCNNConvolutionTranspose filter will be used
--
-- @weights@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource                                              protocol. The MPSCNNConvolutionDataSource protocol declares the methods that an                                              instance of MPSCNNConvolutionTranspose uses to obtain the weights and bias terms                                              for the CNN convolutionTranspose filter. Currently we support only Float32 weights.
--
-- Returns: A valid MPSCNNConvolutionTranspose object.
--
-- ObjC selector: @- initWithDevice:weights:@
initWithDevice_weights :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> RawId -> IO (Id MPSCNNConvolutionTranspose)
initWithDevice_weights mpscnnConvolutionTranspose device weights =
  sendOwnedMessage mpscnnConvolutionTranspose initWithDevice_weightsSelector device weights

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> IO (Id MPSCNNConvolutionTranspose)
initWithDevice mpscnnConvolutionTranspose device =
  sendOwnedMessage mpscnnConvolutionTranspose initWithDeviceSelector device

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsNSCoder aDecoder) => mpscnnConvolutionTranspose -> aDecoder -> RawId -> IO (Id MPSCNNConvolutionTranspose)
initWithCoder_device mpscnnConvolutionTranspose aDecoder device =
  sendOwnedMessage mpscnnConvolutionTranspose initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture to hold the result and return it.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.
--
-- Note: the regular encodeToCommandBuffer:sourceImage: method may be used when no state is needed,                  such as when the convolution transpose operation is not balanced by a matching convolution object upstream.                  These encode methods are for auto encoders where each convolution in inference pass is coupled with convolution                  transpose. In order for convolution transpose to correctly undo the convolution downsampling, MPSCNNConvolutionGradientState                  produced by convolution is needed by convolution transpose to correctly size destination image.                  These methods are only useful for inference only network. For training, use encode methods that take MPSCNNConvolutionTransposeGradientState below.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImage@ — A MPSImage to use as the source images for the filter.
--
-- @convolutionGradientState@ — A valid MPSCNNConvolutionGradientState from the MPSCNNConvoluton counterpart to this MPSCNNConvolutionTranspose.                                      If there is no forward convolution counterpart, pass NULL here. This state affects the sizing                                      the result.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The offset property will be adjusted to reflect the offset used during the encode.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:convolutionGradientState:@
encodeToCommandBuffer_sourceImage_convolutionGradientState :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsMPSCNNConvolutionGradientState convolutionGradientState) => mpscnnConvolutionTranspose -> RawId -> sourceImage -> convolutionGradientState -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_convolutionGradientState mpscnnConvolutionTranspose commandBuffer sourceImage convolutionGradientState =
  sendMessage mpscnnConvolutionTranspose encodeToCommandBuffer_sourceImage_convolutionGradientStateSelector commandBuffer (toMPSImage sourceImage) (toMPSCNNConvolutionGradientState convolutionGradientState)

-- | @- encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates mpscnnConvolutionTranspose commandBuffer sourceImage convolutionGradientState =
  sendMessage mpscnnConvolutionTranspose encodeBatchToCommandBuffer_sourceImages_convolutionGradientStatesSelector commandBuffer sourceImage convolutionGradientState

-- | @- encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationImage:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSImage destinationImage) => mpscnnConvolutionTranspose -> RawId -> sourceImage -> convolutionGradientState -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImage mpscnnConvolutionTranspose commandBuffer sourceImage convolutionGradientState destinationImage =
  sendMessage mpscnnConvolutionTranspose encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toMPSCNNConvolutionGradientState convolutionGradientState) (toMPSImage destinationImage)

-- | @- encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImages :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImages mpscnnConvolutionTranspose commandBuffer sourceImage convolutionGradientState destinationImage =
  sendMessage mpscnnConvolutionTranspose encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImagesSelector commandBuffer sourceImage convolutionGradientState destinationImage

-- | Allocate a MPCNNConvolutionTransposeGradientState to hold the results from a -encodeBatchToCommandBuffer... operation
--
-- @sourceImage@ — The MPSImage consumed by the associated -encode call.
--
-- @sourceStates@ — The list of MPSCNNConvolutionGradientState consumed by the associated -encode call,                                  for a batch size of 1. In auto encoders, this state is produced by corresponding MPSCNNConvolution.
--
-- Returns: The list of states produced by the -encode call for batch size of 1.              -isResultStateReusedAcrossBatch returns YES for MPSCNNConvolutionTranspose so same              state is used across entire batch. State object is not reusasable across batches.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnConvolutionTranspose -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNConvolutionTransposeGradientState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolutionTranspose resultStateForSourceImage_sourceStates_destinationImageSelector (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsNSArray sourceStates) => mpscnnConvolutionTranspose -> RawId -> sourceStates -> RawId -> IO RawId
resultStateBatchForSourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolutionTranspose resultStateBatchForSourceImage_sourceStates_destinationImageSelector sourceImage (toNSArray sourceStates) destinationImage

-- | @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnConvolutionTranspose -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNConvolutionTransposeGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolutionTranspose temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsNSArray sourceStates) => mpscnnConvolutionTranspose -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose commandBuffer sourceImage sourceStates destinationImage =
  sendMessage mpscnnConvolutionTranspose temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector commandBuffer sourceImage (toNSArray sourceStates) destinationImage

-- | CPU side reload. Reload the updated weights and biases from data provider into internal weights and bias buffers. Weights and biases              gradients needed for update are obtained from MPSCNNConvolutionTransposeGradientState object. Data provider passed in init call is used for this purpose.
--
-- ObjC selector: @- reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSource :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO ()
reloadWeightsAndBiasesFromDataSource mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose reloadWeightsAndBiasesFromDataSourceSelector

-- | GPU side reload. Reload the updated weights and biases from update buffer produced by application enqueued metal kernel into internal weights              and biases buffer. Weights and biases gradients needed for update are obtained from MPSCNNConvolutionTransposeGradientState object's gradientForWeights and gradientForBiases metal buffer.
--
-- @commandBuffer@ — Metal command buffer on which application update kernel was enqueued consuming MPSCNNConvolutionGradientState's gradientForWeights and gradientForBiases buffers                                 and producing updateBuffer metal buffer.
--
-- @state@ — MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffers which have updated weights produced by application's update kernel.                                 The state readcount will be decremented.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_state :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSCNNConvolutionWeightsAndBiasesState state) => mpscnnConvolutionTranspose -> RawId -> state -> IO ()
reloadWeightsAndBiasesWithCommandBuffer_state mpscnnConvolutionTranspose commandBuffer state =
  sendMessage mpscnnConvolutionTranspose reloadWeightsAndBiasesWithCommandBuffer_stateSelector commandBuffer (toMPSCNNConvolutionWeightsAndBiasesState state)

-- | GPU side export. Enqueue a kernel to export current weights and biases stored in MPSCNNConvoltionTranspose's internal buffers into weights and biases MTLBuffer              returned in MPSCNNConvolutionWeightsAndBiasesState.
--
-- @commandBuffer@ — Metal command buffer on which export kernel is enqueued.
--
-- @resultStateCanBeTemporary@ — If FALSE, state returned will be non-temporary. If TRUE, returned state may or may not be temporary.
--
-- Returns: MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffer to which weights got exported. This state and be temporary or non-temporary depending on the flag resultStateCanBeTemporary
--
-- ObjC selector: @- exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:@
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> Bool -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary mpscnnConvolutionTranspose commandBuffer resultStateCanBeTemporary =
  sendMessage mpscnnConvolutionTranspose exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector commandBuffer resultStateCanBeTemporary

-- | These low level encode functions should be used during training. The first two encode functions, which return                   destination image on left hand side, takes in MPSCNNConvolutionGradientState that was produced by corresponding                   MPSCNNConvolution when there is one e.g. auto encoders. This state is used to correctly size destination being returned.                   These encode methods return MPSCNNConvoltionTransposeGradientState object on auto release pool to be consumed by MPSCNNConvolutionTransposeGradient.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporary :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSCNNConvolutionTransposeGradientState outState) => mpscnnConvolutionTranspose -> RawId -> sourceImage -> convolutionGradientState -> outState -> Bool -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporary mpscnnConvolutionTranspose commandBuffer sourceImage convolutionGradientState outState isTemporary =
  sendMessage mpscnnConvolutionTranspose encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporarySelector commandBuffer (toMPSImage sourceImage) (toMPSCNNConvolutionGradientState convolutionGradientState) (toMPSCNNConvolutionTransposeGradientState outState) isTemporary

-- | @- encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporary :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> RawId -> RawId -> RawId -> Bool -> IO RawId
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporary mpscnnConvolutionTranspose commandBuffer sourceImages convolutionGradientStates outStates isTemporary =
  sendMessage mpscnnConvolutionTranspose encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporarySelector commandBuffer sourceImages convolutionGradientStates outStates isTemporary

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CULong
inputFeatureChannels mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose inputFeatureChannelsSelector

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CULong
outputFeatureChannels mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose outputFeatureChannelsSelector

-- | kernelOffsetX
--
-- Offset in X from which the kernel starts sliding
--
-- ObjC selector: @- kernelOffsetX@
kernelOffsetX :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CLong
kernelOffsetX mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose kernelOffsetXSelector

-- | kernelOffsetX
--
-- Offset in X from which the kernel starts sliding
--
-- ObjC selector: @- setKernelOffsetX:@
setKernelOffsetX :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> CLong -> IO ()
setKernelOffsetX mpscnnConvolutionTranspose value =
  sendMessage mpscnnConvolutionTranspose setKernelOffsetXSelector value

-- | kernelOffsetY
--
-- Offset in Y from which the kernel starts sliding
--
-- ObjC selector: @- kernelOffsetY@
kernelOffsetY :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CLong
kernelOffsetY mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose kernelOffsetYSelector

-- | kernelOffsetY
--
-- Offset in Y from which the kernel starts sliding
--
-- ObjC selector: @- setKernelOffsetY:@
setKernelOffsetY :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> CLong -> IO ()
setKernelOffsetY mpscnnConvolutionTranspose value =
  sendMessage mpscnnConvolutionTranspose setKernelOffsetYSelector value

-- | groups
--
-- Number of groups input and output channels are divided into.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CULong
groups mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose groupsSelector

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- accumulatorPrecisionOption@
accumulatorPrecisionOption :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecisionOption mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose accumulatorPrecisionOptionSelector

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOption :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> MPSNNConvolutionAccumulatorPrecisionOption -> IO ()
setAccumulatorPrecisionOption mpscnnConvolutionTranspose value =
  sendMessage mpscnnConvolutionTranspose setAccumulatorPrecisionOptionSelector value

-- | dataSource
--
-- dataSource with which convolution transpose object was created
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO RawId
dataSource mpscnnConvolutionTranspose =
  sendMessage mpscnnConvolutionTranspose dataSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector '[RawId, RawId] (Id MPSCNNConvolutionTranspose)
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNConvolutionTranspose)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNConvolutionTranspose)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:convolutionGradientState:@
encodeToCommandBuffer_sourceImage_convolutionGradientStateSelector :: Selector '[RawId, Id MPSImage, Id MPSCNNConvolutionGradientState] (Id MPSImage)
encodeToCommandBuffer_sourceImage_convolutionGradientStateSelector = mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStatesSelector :: Selector '[RawId, RawId, RawId] RawId
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStatesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationImage:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSCNNConvolutionGradientState, Id MPSImage] ()
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImagesSelector :: Selector '[RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationImages:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNConvolutionTransposeGradientState)
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id NSArray, RawId] RawId
resultStateBatchForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSCNNConvolutionTransposeGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector '[RawId, RawId, Id NSArray, RawId] RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSourceSelector :: Selector '[] ()
reloadWeightsAndBiasesFromDataSourceSelector = mkSelector "reloadWeightsAndBiasesFromDataSource"

-- | @Selector@ for @reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_stateSelector :: Selector '[RawId, Id MPSCNNConvolutionWeightsAndBiasesState] ()
reloadWeightsAndBiasesWithCommandBuffer_stateSelector = mkSelector "reloadWeightsAndBiasesWithCommandBuffer:state:"

-- | @Selector@ for @exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:@
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector :: Selector '[RawId, Bool] (Id MPSCNNConvolutionWeightsAndBiasesState)
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector = mkSelector "exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporarySelector :: Selector '[RawId, Id MPSImage, Id MPSCNNConvolutionGradientState, Id MPSCNNConvolutionTransposeGradientState, Bool] (Id MPSImage)
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationState:destinationStateIsTemporary:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporarySelector :: Selector '[RawId, RawId, RawId, RawId, Bool] RawId
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporarySelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationStates:destinationStateIsTemporary:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector '[] CULong
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector '[] CULong
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @kernelOffsetX@
kernelOffsetXSelector :: Selector '[] CLong
kernelOffsetXSelector = mkSelector "kernelOffsetX"

-- | @Selector@ for @setKernelOffsetX:@
setKernelOffsetXSelector :: Selector '[CLong] ()
setKernelOffsetXSelector = mkSelector "setKernelOffsetX:"

-- | @Selector@ for @kernelOffsetY@
kernelOffsetYSelector :: Selector '[] CLong
kernelOffsetYSelector = mkSelector "kernelOffsetY"

-- | @Selector@ for @setKernelOffsetY:@
setKernelOffsetYSelector :: Selector '[CLong] ()
setKernelOffsetYSelector = mkSelector "setKernelOffsetY:"

-- | @Selector@ for @groups@
groupsSelector :: Selector '[] CULong
groupsSelector = mkSelector "groups"

-- | @Selector@ for @accumulatorPrecisionOption@
accumulatorPrecisionOptionSelector :: Selector '[] MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecisionOptionSelector = mkSelector "accumulatorPrecisionOption"

-- | @Selector@ for @setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOptionSelector :: Selector '[MPSNNConvolutionAccumulatorPrecisionOption] ()
setAccumulatorPrecisionOptionSelector = mkSelector "setAccumulatorPrecisionOption:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

