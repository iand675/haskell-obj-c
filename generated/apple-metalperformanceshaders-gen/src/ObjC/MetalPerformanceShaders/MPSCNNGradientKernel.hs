{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNGradientKernel
--
-- Gradient kernels are the backwards pass of a MPSCNNKernel              used during training to calculate gradient back propagation.              These take as arguments the gradient result from the next filter              and the source image for the forward version of the filter.              There is also a MPSNNGradientState passed from MPSCNNKernel              to MPSCNNGradientKernel that contains information about the              MPSCNNKernel parameters at the time it encoded and possibly              also additional MTLResources to enable it to do its job.
--
-- Training graph (partial):
--
-- ---> input image ---------> MPSCNNKernel ------>  resultImage ------>-->-->-->.
-- \                  |                                           |
-- '------.    MPSNNGradientState                         loss estimation
-- \         |                                           |
-- V        V                                           V
-- <--- result gradient <- MPSCNNGradientKernel <---  input gradient <--<--<--<---'
--
-- In general operation, starting with the input image, the sequence of events is:
-- 1a)  Invoke padding policy to find result size for MPSCNNKernel.  This
-- also configures some MPSCNNKernel parameters such as offset.
-- 1b)  Use the MPSImageDescriptor from 1a to make resultImage.
-- 1c)  Call MPSCNNKernel -encode...
-- 2) stages 1a-c are repeated for other forward passes in the inference portion of the graph
-- 3) We estimate the loss resulting from the whole inference computation so far (see MPSCNNLoss.h>
-- 4) stages 5a-c are repeated for corresponding backward gradient passes in the graph
-- 5a) Invoke padding policy on the MPSCNNGradientKernel shown above. This sets the
-- MPSCNNGradientKernel parameters to correspond with those in the forward pass
-- 5b) The result gradient for the MPSCNNGradientKernel is created from the MPSImageDescriptor from 5a
-- 5c) Call MPSCNNGradientKernel -encode with the input image, input gradient, result gradient and MPSNNGradientState
-- 6) pass the result gradient on to leftward gradient passes.
--
-- For MPSCNNKernels that are trained, there may be other accompanying training kernels that              need to be called in addition to the gradient kernel to update convolution weights or batch              normalization parameters, for example. Steps 1a-c and 5a-c can be combined in a single -encode              call. These return the result image or gradient out the left hand side.
--
-- For purposes of inheritance the gradient image is the MPSCNNBinaryKernel primary image              and the source image is the MPSCNNBinaryKernel secondary image. Various secondary properties              such as kernel size are copies of the forward inference pass parameters of similar name              are set automatically when -[MPSCNNGradientKernel destinationImageDescriptorForSourceImages:sourceStates:]              is called.
--
-- Generated bindings for @MPSCNNGradientKernel@.
module ObjC.MetalPerformanceShaders.MPSCNNGradientKernel
  ( MPSCNNGradientKernel
  , IsMPSCNNGradientKernel(..)
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientState
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradient
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradients
  , kernelOffsetX
  , setKernelOffsetX
  , kernelOffsetY
  , setKernelOffsetY
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientStateSelector
  , encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradientSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStatesSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradientsSelector
  , kernelOffsetXSelector
  , setKernelOffsetXSelector
  , kernelOffsetYSelector
  , setKernelOffsetYSelector


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
import ObjC.Foundation.Internal.Classes

-- | Standard init with default properties per filter type
--
-- @device@ — The device that the filter will be used on. May not be NULL.
--
-- Returns: A pointer to the newly initialized object. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNGradientKernel mpscnnGradientKernel => mpscnnGradientKernel -> RawId -> IO (Id MPSCNNGradientKernel)
initWithDevice mpscnnGradientKernel  device =
    sendMsg mpscnnGradientKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNGradientKernel mpscnnGradientKernel, IsNSCoder aDecoder) => mpscnnGradientKernel -> aDecoder -> RawId -> IO (Id MPSCNNGradientKernel)
initWithCoder_device mpscnnGradientKernel  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnGradientKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a gradient filter and return a gradient
--
-- During training, gradient filters are used to calculate the gradient              associated with the loss for each feature channel in the forward pass              source image. For those nodes that are trainable, these are then used              to refine the value used in the trainable parameter. They consume              a source gradient image which contains the gradients corresponding              with the forward pass destination image, and calculate the gradients              corresponding to the forward pass source image.
--
-- A gradient filter consumes a MPSNNGradientState object which captured              various forward pass properties such as offset and edgeMode at the time              the forward pass was encoded. These are transferred to the MPSCNNBinaryKernel              secondary image properties automatically when this method creates its              destination image.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode
--
-- @sourceGradient@ — The gradient image from the "next" filter in the graph (in the inference direction)
--
-- @sourceImage@ — The image used as source image by the forward inference pass
--
-- @gradientState@ — The MPSNNGradientState or MPSNNBinaryGradientState subclass produced by the forward                              inference pass
--
-- Returns: The result gradient from the gradient filter
--
-- ObjC selector: @- encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState :: (IsMPSCNNGradientKernel mpscnnGradientKernel, IsMPSImage sourceGradient, IsMPSImage sourceImage, IsMPSState gradientState) => mpscnnGradientKernel -> RawId -> sourceGradient -> sourceImage -> gradientState -> IO (Id MPSImage)
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState mpscnnGradientKernel  commandBuffer sourceGradient sourceImage gradientState =
  withObjCPtr sourceGradient $ \raw_sourceGradient ->
    withObjCPtr sourceImage $ \raw_sourceImage ->
      withObjCPtr gradientState $ \raw_gradientState ->
          sendMsg mpscnnGradientKernel (mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ())] >>= retainedObject . castPtr

-- | Encode a gradient filter and return a gradient
--
-- During training, gradient filters are used to calculate the gradient              associated with the loss for each feature channel in the forward pass              source image. For those nodes that are trainable, these are then used              to refine the value used in the trainable parameter. They consume              a source gradient image which contains the gradients corresponding              with the forward pass destination image, and calculate the gradients              corresponding to the forward pass source image.
--
-- A gradient filter consumes a MPSNNGradientState object which captured              various forward pass properties such as offset and edgeMode at the time              the forward pass was encoded. These are transferred to the MPSCNNBinaryKernel              secondary image properties automatically when you use -[MPSCNNGradientKernel              destinationImageDescriptorForSourceImages:sourceStates:]. If you do not call              this method, then you are responsible for configuring all of the primary and              secondary image properties in MPSCNNBinaryKernel. Please see class description              for expected ordering of operations.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode
--
-- @sourceGradient@ — The gradient image from the "next" filter in the graph
--
-- @sourceImage@ — The image used as source image from the forward pass
--
-- @gradientState@ — The MPSNNGradientState and MPSNNBinaryGradientState subclass produced by the                              forward pass
--
-- @destinationGradient@ — The MPSImage into which to write the filter result
--
-- ObjC selector: @- encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:destinationGradient:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradient :: (IsMPSCNNGradientKernel mpscnnGradientKernel, IsMPSImage sourceGradient, IsMPSImage sourceImage, IsMPSState gradientState, IsMPSImage destinationGradient) => mpscnnGradientKernel -> RawId -> sourceGradient -> sourceImage -> gradientState -> destinationGradient -> IO ()
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradient mpscnnGradientKernel  commandBuffer sourceGradient sourceImage gradientState destinationGradient =
  withObjCPtr sourceGradient $ \raw_sourceGradient ->
    withObjCPtr sourceImage $ \raw_sourceImage ->
      withObjCPtr gradientState $ \raw_gradientState ->
        withObjCPtr destinationGradient $ \raw_destinationGradient ->
            sendMsg mpscnnGradientKernel (mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:destinationGradient:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_destinationGradient :: Ptr ())]

-- | Encode a gradient filter and return a gradient
--
-- During training, gradient filters are used to calculate the gradient              associated with the loss for each feature channel in the forward pass              source image. For those nodes that are trainable, these are then used              to refine the value used in the trainable parameter. They consume              a source gradient image which contains the gradients corresponding              with the forward pass destination image, and calculate the gradients              corresponding to the forward pass source image.
--
-- A gradient filter consumes a MPSNNGradientState object which captured              various forward pass properties such as offset and edgeMode at the time              the forward pass was encoded. These are transferred to the MPSCNNBinaryKernel              secondary image properties automatically when this method creates its              destination image.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode
--
-- @sourceGradients@ — The gradient images from the "next" filter in the graph
--
-- @sourceImages@ — The images used as source image from the forward pass
--
-- @gradientStates@ — The MPSNNGradientState or MPSNNBinaryGradientState subclass produced by the                               forward pass
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates :: IsMPSCNNGradientKernel mpscnnGradientKernel => mpscnnGradientKernel -> RawId -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates mpscnnGradientKernel  commandBuffer sourceGradients sourceImages gradientStates =
    fmap (RawId . castPtr) $ sendMsg mpscnnGradientKernel (mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceGradients) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId gradientStates) :: Ptr ())]

-- | Encode a gradient filter and return a gradient
--
-- During training, gradient filters are used to calculate the gradient              associated with the loss for each feature channel in the forward pass              source image. For those nodes that are trainable, these are then used              to refine the value used in the trainable parameter. They consume              a source gradient image which contains the gradients corresponding              with the forward pass destination image, and calculate the gradients              corresponding to the forward pass source image.
--
-- A gradient filter consumes a MPSNNGradientState object which captured              various forward pass properties such as offset and edgeMode at the time              the forward pass was encoded. These are transferred to the MPSCNNBinaryKernel              secondary image properties automatically when you use -[MPSCNNGradientKernel              destinationImageDescriptorForSourceImages:sourceStates:]. If you do not call              this method, then you are responsible for configuring all of the primary and              secondary image properties in MPSCNNBinaryKernel. Please see class description              for expected ordering of operations.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode
--
-- @sourceGradients@ — The gradient images from the "next" filter in the graph
--
-- @sourceImages@ — The image used as source images from the forward pass
--
-- @gradientStates@ — An array of the MPSNNGradientState or MPSNNBinaryGradientState subclass                               produced by the forward pass
--
-- @destinationGradients@ — The MPSImages into which to write the filter result
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradients :: IsMPSCNNGradientKernel mpscnnGradientKernel => mpscnnGradientKernel -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradients mpscnnGradientKernel  commandBuffer sourceGradients sourceImages gradientStates destinationGradients =
    sendMsg mpscnnGradientKernel (mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:destinationGradients:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceGradients) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId gradientStates) :: Ptr ()), argPtr (castPtr (unRawId destinationGradients) :: Ptr ())]

-- | kernelOffsetX
--
-- Offset in the kernel reference frame to position the kernel in the X dimension
--
-- In some cases, the input gradient must be upsampled with zero insertion              to account for things like strides in the forward MPSCNNKernel pass.              As such, the offset, which describes a X,Y offset in the source coordinate              space is insufficient to fully describe the offset applied to a kernel.              The kernel offset is the offset after upsampling. Both the source offset              and kernel offset are additive:  effective offset = source offset * stride + kernel offset.              The offset is applied to the (upsampled) source gradient
--
-- ObjC selector: @- kernelOffsetX@
kernelOffsetX :: IsMPSCNNGradientKernel mpscnnGradientKernel => mpscnnGradientKernel -> IO CLong
kernelOffsetX mpscnnGradientKernel  =
    sendMsg mpscnnGradientKernel (mkSelector "kernelOffsetX") retCLong []

-- | kernelOffsetX
--
-- Offset in the kernel reference frame to position the kernel in the X dimension
--
-- In some cases, the input gradient must be upsampled with zero insertion              to account for things like strides in the forward MPSCNNKernel pass.              As such, the offset, which describes a X,Y offset in the source coordinate              space is insufficient to fully describe the offset applied to a kernel.              The kernel offset is the offset after upsampling. Both the source offset              and kernel offset are additive:  effective offset = source offset * stride + kernel offset.              The offset is applied to the (upsampled) source gradient
--
-- ObjC selector: @- setKernelOffsetX:@
setKernelOffsetX :: IsMPSCNNGradientKernel mpscnnGradientKernel => mpscnnGradientKernel -> CLong -> IO ()
setKernelOffsetX mpscnnGradientKernel  value =
    sendMsg mpscnnGradientKernel (mkSelector "setKernelOffsetX:") retVoid [argCLong value]

-- | kernelOffsetY
--
-- Offset in the kernel reference frame to position the kernel in the Y dimension
--
-- In some cases, the input gradient must be upsampled with zero insertion              to account for things like strides in the forward MPSCNNKernel pass.              As such, the offset, which describes a X,Y offset in the source coordinate              space is insufficient to fully describe the offset applied to a kernel.              The kernel offset is the offset after upsampling. Both the source offset              and kernel offset are additive:  effective offset = source offset * stride + kernel offset.               The offset is applied to the (upsampled) source gradient
--
-- ObjC selector: @- kernelOffsetY@
kernelOffsetY :: IsMPSCNNGradientKernel mpscnnGradientKernel => mpscnnGradientKernel -> IO CLong
kernelOffsetY mpscnnGradientKernel  =
    sendMsg mpscnnGradientKernel (mkSelector "kernelOffsetY") retCLong []

-- | kernelOffsetY
--
-- Offset in the kernel reference frame to position the kernel in the Y dimension
--
-- In some cases, the input gradient must be upsampled with zero insertion              to account for things like strides in the forward MPSCNNKernel pass.              As such, the offset, which describes a X,Y offset in the source coordinate              space is insufficient to fully describe the offset applied to a kernel.              The kernel offset is the offset after upsampling. Both the source offset              and kernel offset are additive:  effective offset = source offset * stride + kernel offset.               The offset is applied to the (upsampled) source gradient
--
-- ObjC selector: @- setKernelOffsetY:@
setKernelOffsetY :: IsMPSCNNGradientKernel mpscnnGradientKernel => mpscnnGradientKernel -> CLong -> IO ()
setKernelOffsetY mpscnnGradientKernel  value =
    sendMsg mpscnnGradientKernel (mkSelector "setKernelOffsetY:") retVoid [argCLong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientStateSelector :: Selector
encodeToCommandBuffer_sourceGradient_sourceImage_gradientStateSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:destinationGradient:@
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradientSelector :: Selector
encodeToCommandBuffer_sourceGradient_sourceImage_gradientState_destinationGradientSelector = mkSelector "encodeToCommandBuffer:sourceGradient:sourceImage:gradientState:destinationGradient:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStatesSelector :: Selector
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStatesSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradientsSelector :: Selector
encodeBatchToCommandBuffer_sourceGradients_sourceImages_gradientStates_destinationGradientsSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:gradientStates:destinationGradients:"

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

