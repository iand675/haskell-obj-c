{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDevice_weightsSelector
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImage_convolutionGradientStateSelector
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStatesSelector
  , encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImageSelector
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImagesSelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , resultStateBatchForSourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , reloadWeightsAndBiasesFromDataSourceSelector
  , reloadWeightsAndBiasesWithCommandBuffer_stateSelector
  , exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector
  , encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporarySelector
  , encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporarySelector
  , inputFeatureChannelsSelector
  , outputFeatureChannelsSelector
  , kernelOffsetXSelector
  , setKernelOffsetXSelector
  , kernelOffsetYSelector
  , setKernelOffsetYSelector
  , groupsSelector
  , accumulatorPrecisionOptionSelector
  , setAccumulatorPrecisionOptionSelector
  , dataSourceSelector

  -- * Enum types
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
initWithDevice_weights mpscnnConvolutionTranspose  device weights =
    sendMsg mpscnnConvolutionTranspose (mkSelector "initWithDevice:weights:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr (unRawId weights) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> IO (Id MPSCNNConvolutionTranspose)
initWithDevice mpscnnConvolutionTranspose  device =
    sendMsg mpscnnConvolutionTranspose (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsNSCoder aDecoder) => mpscnnConvolutionTranspose -> aDecoder -> RawId -> IO (Id MPSCNNConvolutionTranspose)
initWithCoder_device mpscnnConvolutionTranspose  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnConvolutionTranspose (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_sourceImage_convolutionGradientState mpscnnConvolutionTranspose  commandBuffer sourceImage convolutionGradientState =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
        sendMsg mpscnnConvolutionTranspose (mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ())] >>= retainedObject . castPtr

-- | @- encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates mpscnnConvolutionTranspose  commandBuffer sourceImage convolutionGradientState =
    fmap (RawId . castPtr) $ sendMsg mpscnnConvolutionTranspose (mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr (unRawId convolutionGradientState) :: Ptr ())]

-- | @- encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationImage:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSImage destinationImage) => mpscnnConvolutionTranspose -> RawId -> sourceImage -> convolutionGradientState -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImage mpscnnConvolutionTranspose  commandBuffer sourceImage convolutionGradientState destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnConvolutionTranspose (mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | @- encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImages :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImages mpscnnConvolutionTranspose  commandBuffer sourceImage convolutionGradientState destinationImage =
    sendMsg mpscnnConvolutionTranspose (mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationImages:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr (unRawId convolutionGradientState) :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

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
resultStateForSourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose  sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnConvolutionTranspose (mkSelector "resultStateForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsNSArray sourceStates) => mpscnnConvolutionTranspose -> RawId -> sourceStates -> RawId -> IO RawId
resultStateBatchForSourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose  sourceImage sourceStates destinationImage =
  withObjCPtr sourceStates $ \raw_sourceStates ->
      fmap (RawId . castPtr) $ sendMsg mpscnnConvolutionTranspose (mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnConvolutionTranspose -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSCNNConvolutionTransposeGradientState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnConvolutionTranspose (mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsNSArray sourceStates) => mpscnnConvolutionTranspose -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnConvolutionTranspose  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceStates $ \raw_sourceStates ->
      fmap (RawId . castPtr) $ sendMsg mpscnnConvolutionTranspose (mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | CPU side reload. Reload the updated weights and biases from data provider into internal weights and bias buffers. Weights and biases              gradients needed for update are obtained from MPSCNNConvolutionTransposeGradientState object. Data provider passed in init call is used for this purpose.
--
-- ObjC selector: @- reloadWeightsAndBiasesFromDataSource@
reloadWeightsAndBiasesFromDataSource :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO ()
reloadWeightsAndBiasesFromDataSource mpscnnConvolutionTranspose  =
    sendMsg mpscnnConvolutionTranspose (mkSelector "reloadWeightsAndBiasesFromDataSource") retVoid []

-- | GPU side reload. Reload the updated weights and biases from update buffer produced by application enqueued metal kernel into internal weights              and biases buffer. Weights and biases gradients needed for update are obtained from MPSCNNConvolutionTransposeGradientState object's gradientForWeights and gradientForBiases metal buffer.
--
-- @commandBuffer@ — Metal command buffer on which application update kernel was enqueued consuming MPSCNNConvolutionGradientState's gradientForWeights and gradientForBiases buffers                                 and producing updateBuffer metal buffer.
--
-- @state@ — MPSCNNConvolutionWeightsAndBiasesState containing weights and biases buffers which have updated weights produced by application's update kernel.                                 The state readcount will be decremented.
--
-- ObjC selector: @- reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_state :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSCNNConvolutionWeightsAndBiasesState state) => mpscnnConvolutionTranspose -> RawId -> state -> IO ()
reloadWeightsAndBiasesWithCommandBuffer_state mpscnnConvolutionTranspose  commandBuffer state =
  withObjCPtr state $ \raw_state ->
      sendMsg mpscnnConvolutionTranspose (mkSelector "reloadWeightsAndBiasesWithCommandBuffer:state:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_state :: Ptr ())]

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
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporary mpscnnConvolutionTranspose  commandBuffer resultStateCanBeTemporary =
    sendMsg mpscnnConvolutionTranspose (mkSelector "exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argCULong (if resultStateCanBeTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | These low level encode functions should be used during training. The first two encode functions, which return                   destination image on left hand side, takes in MPSCNNConvolutionGradientState that was produced by corresponding                   MPSCNNConvolution when there is one e.g. auto encoders. This state is used to correctly size destination being returned.                   These encode methods return MPSCNNConvoltionTransposeGradientState object on auto release pool to be consumed by MPSCNNConvolutionTransposeGradient.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporary :: (IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose, IsMPSImage sourceImage, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSCNNConvolutionTransposeGradientState outState) => mpscnnConvolutionTranspose -> RawId -> sourceImage -> convolutionGradientState -> outState -> Bool -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporary mpscnnConvolutionTranspose  commandBuffer sourceImage convolutionGradientState outState isTemporary =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
      withObjCPtr outState $ \raw_outState ->
          sendMsg mpscnnConvolutionTranspose (mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationState:destinationStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr raw_outState :: Ptr ()), argCULong (if isTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | @- encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporary :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> RawId -> RawId -> RawId -> RawId -> Bool -> IO RawId
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporary mpscnnConvolutionTranspose  commandBuffer sourceImages convolutionGradientStates outStates isTemporary =
    fmap (RawId . castPtr) $ sendMsg mpscnnConvolutionTranspose (mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationStates:destinationStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId convolutionGradientStates) :: Ptr ()), argPtr (castPtr (unRawId outStates) :: Ptr ()), argCULong (if isTemporary then 1 else 0)]

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CULong
inputFeatureChannels mpscnnConvolutionTranspose  =
    sendMsg mpscnnConvolutionTranspose (mkSelector "inputFeatureChannels") retCULong []

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CULong
outputFeatureChannels mpscnnConvolutionTranspose  =
    sendMsg mpscnnConvolutionTranspose (mkSelector "outputFeatureChannels") retCULong []

-- | kernelOffsetX
--
-- Offset in X from which the kernel starts sliding
--
-- ObjC selector: @- kernelOffsetX@
kernelOffsetX :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CLong
kernelOffsetX mpscnnConvolutionTranspose  =
    sendMsg mpscnnConvolutionTranspose (mkSelector "kernelOffsetX") retCLong []

-- | kernelOffsetX
--
-- Offset in X from which the kernel starts sliding
--
-- ObjC selector: @- setKernelOffsetX:@
setKernelOffsetX :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> CLong -> IO ()
setKernelOffsetX mpscnnConvolutionTranspose  value =
    sendMsg mpscnnConvolutionTranspose (mkSelector "setKernelOffsetX:") retVoid [argCLong value]

-- | kernelOffsetY
--
-- Offset in Y from which the kernel starts sliding
--
-- ObjC selector: @- kernelOffsetY@
kernelOffsetY :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CLong
kernelOffsetY mpscnnConvolutionTranspose  =
    sendMsg mpscnnConvolutionTranspose (mkSelector "kernelOffsetY") retCLong []

-- | kernelOffsetY
--
-- Offset in Y from which the kernel starts sliding
--
-- ObjC selector: @- setKernelOffsetY:@
setKernelOffsetY :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> CLong -> IO ()
setKernelOffsetY mpscnnConvolutionTranspose  value =
    sendMsg mpscnnConvolutionTranspose (mkSelector "setKernelOffsetY:") retVoid [argCLong value]

-- | groups
--
-- Number of groups input and output channels are divided into.
--
-- ObjC selector: @- groups@
groups :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO CULong
groups mpscnnConvolutionTranspose  =
    sendMsg mpscnnConvolutionTranspose (mkSelector "groups") retCULong []

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- accumulatorPrecisionOption@
accumulatorPrecisionOption :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO MPSNNConvolutionAccumulatorPrecisionOption
accumulatorPrecisionOption mpscnnConvolutionTranspose  =
    fmap (coerce :: CULong -> MPSNNConvolutionAccumulatorPrecisionOption) $ sendMsg mpscnnConvolutionTranspose (mkSelector "accumulatorPrecisionOption") retCULong []

-- | Precision of accumulator used in convolution.
--
-- See MPSNeuralNetworkTypes.h for discussion. Default is MPSNNConvolutionAccumulatorPrecisionOptionFloat.
--
-- ObjC selector: @- setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOption :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> MPSNNConvolutionAccumulatorPrecisionOption -> IO ()
setAccumulatorPrecisionOption mpscnnConvolutionTranspose  value =
    sendMsg mpscnnConvolutionTranspose (mkSelector "setAccumulatorPrecisionOption:") retVoid [argCULong (coerce value)]

-- | dataSource
--
-- dataSource with which convolution transpose object was created
--
-- ObjC selector: @- dataSource@
dataSource :: IsMPSCNNConvolutionTranspose mpscnnConvolutionTranspose => mpscnnConvolutionTranspose -> IO RawId
dataSource mpscnnConvolutionTranspose  =
    fmap (RawId . castPtr) $ sendMsg mpscnnConvolutionTranspose (mkSelector "dataSource") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:convolutionGradientState:@
encodeToCommandBuffer_sourceImage_convolutionGradientStateSelector :: Selector
encodeToCommandBuffer_sourceImage_convolutionGradientStateSelector = mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStatesSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStatesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationImage:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationImages:"

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

-- | @Selector@ for @reloadWeightsAndBiasesWithCommandBuffer:state:@
reloadWeightsAndBiasesWithCommandBuffer_stateSelector :: Selector
reloadWeightsAndBiasesWithCommandBuffer_stateSelector = mkSelector "reloadWeightsAndBiasesWithCommandBuffer:state:"

-- | @Selector@ for @exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:@
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector :: Selector
exportWeightsAndBiasesWithCommandBuffer_resultStateCanBeTemporarySelector = mkSelector "exportWeightsAndBiasesWithCommandBuffer:resultStateCanBeTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporarySelector :: Selector
encodeToCommandBuffer_sourceImage_convolutionGradientState_destinationState_destinationStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceImage:convolutionGradientState:destinationState:destinationStateIsTemporary:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporarySelector :: Selector
encodeBatchToCommandBuffer_sourceImages_convolutionGradientStates_destinationStates_destinationStateIsTemporarySelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:convolutionGradientStates:destinationStates:destinationStateIsTemporary:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @kernelOffsetX@
kernelOffsetXSelector :: Selector
kernelOffsetXSelector = mkSelector "kernelOffsetX"

-- | @Selector@ for @setKernelOffsetX:@
setKernelOffsetXSelector :: Selector
setKernelOffsetXSelector = mkSelector "setKernelOffsetX:"

-- | @Selector@ for @kernelOffsetY@
kernelOffsetYSelector :: Selector
kernelOffsetYSelector = mkSelector "kernelOffsetY"

-- | @Selector@ for @setKernelOffsetY:@
setKernelOffsetYSelector :: Selector
setKernelOffsetYSelector = mkSelector "setKernelOffsetY:"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @accumulatorPrecisionOption@
accumulatorPrecisionOptionSelector :: Selector
accumulatorPrecisionOptionSelector = mkSelector "accumulatorPrecisionOption"

-- | @Selector@ for @setAccumulatorPrecisionOption:@
setAccumulatorPrecisionOptionSelector :: Selector
setAccumulatorPrecisionOptionSelector = mkSelector "setAccumulatorPrecisionOption:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

