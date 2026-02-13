{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSKernel
--
-- This depends on Metal.framework
--
-- The MPSKernel class is the base class for all MPS objects.  It defines a standard interface for              MPS kernels.   You should not use the MPSKernel class directly. Instead, a  number of MPSKernel               subclasses are available in MetalPerformanceShaders.framework that define specific high-performance              image processing operations.
--
-- The basic sequence for applying a MPSKernel to an image is as follows:
--
-- 1.  Create a MPSKernel corresponding to the operation you wish to perform:
--
-- MPSImageSobel *sobel = [[MPSImageSobel alloc] initWithDevice: mtlDevice];
--
-- 2.  Encode the filter into a command buffer:
--
-- sobel.offset = ...;
-- sobel.clipRect = ...;
-- sobel.options = ...;
-- [sobel encodeToCommandBuffer: commandBuffer
-- sourceTexture: inputImage
-- destinationTexture: resultImage ];
--
-- Encoding the kernel merely encodes the operation into a MTLCommandBuffer. It does not modify any pixels, yet.                  All MPSKernel state has been copied to the command buffer. MPSKernels may be reused.  If the texture was previously                  operated on by another command encoder (e.g. MTLRenderCommandEncoder), you should call -endEncoding on the other                  encoder before encoding the filter.
--
-- Some MPS filters work in place (inputImage = resultImage) even in situations where Metal might not                  normally allow in place operation on textures. If in-place operation is desired, you may attempt to call                  [MPSKernel encodeKernelInPlace...]. If the operation can not be completed in place, then                  NO will be returned and you will have to create a new result texture and try again. To make an in-place                  image filter reliable, pass a fallback MPSCopyAllocator to the method to create a new texture to write                  to in the event that a filter can not operate in place.
--
-- (Repeat steps 2 for more filters, as desired.)
--
-- It should be self evident that step 2 may not be thread safe. That is, you can not have                      multiple threads manipulating the same properties on the same MPSKernel object at the                      same time and achieve coherent output. In common usage, the MPSKernel properties don't                      often need to be changed from their default values, but if you need to apply the same                      filter to multiple images on multiple threads with cropping / tiling, make additional                      MPSKernel objects per thread. They are cheap. You can use multiple MPSKernel objects on                      multiple threads, as long as only one thread is operating on any particular MPSKernel                      object at a time.
--
-- 3.  After encoding any additional work to the command buffer using other encoders, submit the MTLCommandBuffer                  to your MTLCommandQueue, using:
--
-- [mtlCommandBuffer commit];
--
-- A MPSKernel can be saved to disk / network using NSCoders such as NSKeyedArchiver.              When decoding, the system default MTLDevice will be chosen unless the NSCoder adopts              the <MPSDeviceProvider> protocol.  To accomplish this, subclass or extend your unarchiver               to add this method.
--
-- Generated bindings for @MPSKernel@.
module ObjC.MetalPerformanceShaders.MPSKernel
  ( MPSKernel
  , IsMPSKernel(..)
  , initWithDevice
  , copyWithZone_device
  , initWithCoder
  , initWithCoder_device
  , options
  , setOptions
  , device
  , label
  , setLabel
  , copyWithZone_deviceSelector
  , deviceSelector
  , initWithCoderSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , labelSelector
  , optionsSelector
  , setLabelSelector
  , setOptionsSelector

  -- * Enum types
  , MPSKernelOptions(MPSKernelOptions)
  , pattern MPSKernelOptionsNone
  , pattern MPSKernelOptionsSkipAPIValidation
  , pattern MPSKernelOptionsAllowReducedPrecision
  , pattern MPSKernelOptionsDisableInternalTiling
  , pattern MPSKernelOptionsInsertDebugGroups
  , pattern MPSKernelOptionsVerbose

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

-- | Standard init with default properties per filter type
--
-- @device@ — The device that the filter will be used on. May not be NULL.
--
-- Returns: a pointer to the newly initialized object. This will fail, returning              nil if the device is not supported. Devices must be               MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSKernel mpsKernel => mpsKernel -> RawId -> IO (Id MPSKernel)
initWithDevice mpsKernel device =
  sendOwnedMessage mpsKernel initWithDeviceSelector device

-- | Make a copy of this MPSKernel for a new device
--
-- -copyWithZone: will call this API to make a copy of the              MPSKernel on the same device.  This interface may also be              called directly to make a copy of the MPSKernel on a new              device. Typically, the same MPSKernels should not be used              to encode kernels on multiple command buffers from multiple              threads. Many MPSKernels have mutable properties that might               be changed by the other thread while this one is trying to               encode. If you need to use a MPSKernel from multiple threads              make a copy of it for each additional thread using -copyWithZone:              or -copyWithZone:device:
--
-- @zone@ — The NSZone in which to allocate the object
--
-- @device@ — The device for the new MPSKernel. If nil, then use                          self.device.
--
-- Returns: a pointer to a copy of this MPSKernel. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSKernel mpsKernel => mpsKernel -> Ptr () -> RawId -> IO (Id MPSKernel)
copyWithZone_device mpsKernel zone device =
  sendOwnedMessage mpsKernel copyWithZone_deviceSelector zone device

-- | Called by NSCoder to decode MPSKernels
--
-- This isn't the right interface to decode a MPSKernel, but              it is the one that NSCoder uses. To enable your NSCoder              (e.g. NSKeyedUnarchiver) to set which device to use              extend the object to adopt the MPSDeviceProvider               protocol. Otherwise, the Metal system default device              will be used.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsMPSKernel mpsKernel, IsNSCoder aDecoder) => mpsKernel -> aDecoder -> IO (Id MPSKernel)
initWithCoder mpsKernel aDecoder =
  sendOwnedMessage mpsKernel initWithCoderSelector (toNSCoder aDecoder)

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't               know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid               that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSKernel mpsKernel, IsNSCoder aDecoder) => mpsKernel -> aDecoder -> RawId -> IO (Id MPSKernel)
initWithCoder_device mpsKernel aDecoder device =
  sendOwnedMessage mpsKernel initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | options
--
-- The set of options used to run the kernel.  subsubsection_options
--
-- ObjC selector: @- options@
options :: IsMPSKernel mpsKernel => mpsKernel -> IO MPSKernelOptions
options mpsKernel =
  sendMessage mpsKernel optionsSelector

-- | options
--
-- The set of options used to run the kernel.  subsubsection_options
--
-- ObjC selector: @- setOptions:@
setOptions :: IsMPSKernel mpsKernel => mpsKernel -> MPSKernelOptions -> IO ()
setOptions mpsKernel value =
  sendMessage mpsKernel setOptionsSelector value

-- | device
--
-- The device on which the kernel will be used
--
-- ObjC selector: @- device@
device :: IsMPSKernel mpsKernel => mpsKernel -> IO RawId
device mpsKernel =
  sendMessage mpsKernel deviceSelector

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMPSKernel mpsKernel => mpsKernel -> IO (Id NSString)
label mpsKernel =
  sendMessage mpsKernel labelSelector

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMPSKernel mpsKernel, IsNSString value) => mpsKernel -> value -> IO ()
setLabel mpsKernel value =
  sendMessage mpsKernel setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSKernel)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id MPSKernel)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] MPSKernelOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[MPSKernelOptions] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

