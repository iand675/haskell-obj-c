{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSUnaryImageKernel
--
-- This depends on Metal.framework
--
-- A MPSUnaryImageKernel consumes one MTLTexture and produces one MTLTexture.
--
-- Generated bindings for @MPSUnaryImageKernel@.
module ObjC.MetalPerformanceShaders.MPSUnaryImageKernel
  ( MPSUnaryImageKernel
  , IsMPSUnaryImageKernel(..)
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_inPlaceTexture_fallbackCopyAllocator
  , encodeToCommandBuffer_sourceTexture_destinationTexture
  , encodeToCommandBuffer_sourceImage_destinationImage
  , edgeMode
  , setEdgeMode
  , edgeModeSelector
  , encodeToCommandBuffer_inPlaceTexture_fallbackCopyAllocatorSelector
  , encodeToCommandBuffer_sourceImage_destinationImageSelector
  , encodeToCommandBuffer_sourceTexture_destinationTextureSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , setEdgeModeSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Standard init with default properties per filter type
--
-- @device@ — The device that the filter will be used on. May not be NULL.
--
-- Returns: a pointer to the newly initialized object. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSUnaryImageKernel mpsUnaryImageKernel => mpsUnaryImageKernel -> RawId -> IO (Id MPSUnaryImageKernel)
initWithDevice mpsUnaryImageKernel device =
  sendOwnedMessage mpsUnaryImageKernel initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSUnaryImageKernel mpsUnaryImageKernel, IsNSCoder aDecoder) => mpsUnaryImageKernel -> aDecoder -> RawId -> IO (Id MPSUnaryImageKernel)
initWithCoder_device mpsUnaryImageKernel aDecoder device =
  sendOwnedMessage mpsUnaryImageKernel initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | This method attempts to apply the MPSKernel in place on a texture.
--
-- In-place operation means that the same texture is used both to hold the input          image and the results. Operating in-place can be an excellent way to reduce          resource utilization, and save time and energy. While simple Metal kernels can          not operate in place because textures can not be readable and writable at the          same time, some MPSKernels can operate in place because they use          multi-pass algorithms. Whether a MPSKernel can operate in-place can          depend on current hardware, operating system revision and the parameters          and properties passed to it. You should never assume that a MPSKernel will          continue to work in place, even if you have observed it doing so before.
--
-- If the operation succeeds in-place, YES is returned.  If the in-place operation  fails and no copyAllocator is provided, then NO is returned. Without a fallback  MPSCopyAllocator, in neither case is the pointer held at *texture modified.
--
-- Failure during in-place operation is very common and will occur inconsistently across  different hardware platforms and OS releases. Without a fallback MPSCopyAllocator,  operating in place may require significant error handling code to accompany each  call to -encodeToCommandBuffer:..., complicating your code.
--
-- You may find it simplifies your code to provide a fallback MPSCopyAllocator so  that the operation can proceed reliably even when it can not proceed in-place.  When an in-place filter fails, the MPSCopyAllocator (if any) will be  invoked to create a new texture in which to write the results, allowing the  filter to proceed reliably out-of-place. The original texture will be released,  replaced with a pointer to the new texture and YES will be returned. If the  allocator returns an invalid texture, it is released, *texture remains unmodified  and NO is returned.  Please see the MPSCopyAllocator definition for a sample allocator  implementation.
--
-- Sample usage with a copy allocator:
--
-- id <MTLTexture> inPlaceTex = ...;
-- MPSImageSobel *sobelFiler = [[MPSImageSobel alloc] initWithDevice: myDevice];
--
-- // With a fallback MPSCopyAllocator, failure should only occur in exceptional
-- // conditions such as MTLTexture allocation failure or programmer error.
-- // That is, the operation is roughly as robust as the MPSCopyAllocator.
-- // Depending on the quality of that, we might decide we are justified here
-- // in not checking the return value.
-- [sobelFilter encodeToCommandBuffer: myCommandBuffer
-- inPlaceTexture: &inPlaceTex  // may be replaced!
-- fallbackCopyAllocator: myAllocator];
--
-- // If myAllocator was not called:
-- //
-- //      inPlaceTex holds the original texture with the result pixels in it
-- //
-- // else,
-- //
-- //      1) myAllocator creates a new texture.
-- //      2) The new texture pixel data is overwritten by MPSUnaryImageKernel.
-- //      3) The old texture passed in *inPlaceTex is released once.
-- //      4) *inPlaceTex = the new texture
-- //
-- // In either case, the caller should now hold one reference to the texture now held in
-- // inPlaceTex, whether it was replaced or not. Most of the time that means that nothing
-- // further needs to be done here, and you can proceed to the next image encoding operation.
-- // However, if other agents held references to the original texture, they still hold them
-- // and may need to be alerted that the texture has been replaced so that they can retain
-- // the new texture and release the old one.
--
-- [sobelFilter release];  // if not ARC, clean up the MPSImageSobel object
--
-- Note: Image filters that look at neighboring pixel values may actually consume more        memory when operating in place than out of place. Many such operations are        tiled internally to save intermediate texture storage, but can not tile when        operating in place. The memory savings for tiling is however very short term,        typically the lifetime of the MTLCommandBuffer.
--
-- Attempt to apply a MPSKernel to a texture in place.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @texture@ — A pointer to a valid MTLTexture containing source image.                                  On success, the image contents and possibly texture itself                                  will be replaced with the result image.
--
-- @copyAllocator@ — An optional block to allocate a new texture to hold the                                  results, in case in-place operation is not possible. The                                  allocator may use a different MTLPixelFormat or size than                                  the original texture. You may enqueue operations on the                                  provided MTLCommandBuffer using the provided                                  MTLComputeCommandEncoder to initialize the texture contents.
--
-- Returns: On success, YES is returned. The texture may have been replaced with a new              texture if a copyAllocator was provided.  On failure, NO is returned. The              texture is unmodified.
--
-- ObjC selector: @- encodeToCommandBuffer:inPlaceTexture:fallbackCopyAllocator:@
encodeToCommandBuffer_inPlaceTexture_fallbackCopyAllocator :: IsMPSUnaryImageKernel mpsUnaryImageKernel => mpsUnaryImageKernel -> RawId -> RawId -> Ptr () -> IO Bool
encodeToCommandBuffer_inPlaceTexture_fallbackCopyAllocator mpsUnaryImageKernel commandBuffer texture copyAllocator =
  sendMessage mpsUnaryImageKernel encodeToCommandBuffer_inPlaceTexture_fallbackCopyAllocatorSelector commandBuffer texture copyAllocator

-- | Encode a MPSKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceTexture@ — A valid MTLTexture containing the source image.
--
-- @destinationTexture@ — A valid MTLTexture to be overwritten by result image. DestinationTexture may not alias sourceTexture.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:destinationTexture:@
encodeToCommandBuffer_sourceTexture_destinationTexture :: IsMPSUnaryImageKernel mpsUnaryImageKernel => mpsUnaryImageKernel -> RawId -> RawId -> RawId -> IO ()
encodeToCommandBuffer_sourceTexture_destinationTexture mpsUnaryImageKernel commandBuffer sourceTexture destinationTexture =
  sendMessage mpsUnaryImageKernel encodeToCommandBuffer_sourceTexture_destinationTextureSelector commandBuffer sourceTexture destinationTexture

-- | Encode a MPSKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImage@ — A valid MPSImage containing the source image.
--
-- @destinationImage@ — A valid MPSImage to be overwritten by result image. DestinationImage may not alias sourceImage.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImage :: (IsMPSUnaryImageKernel mpsUnaryImageKernel, IsMPSImage sourceImage, IsMPSImage destinationImage) => mpsUnaryImageKernel -> RawId -> sourceImage -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_destinationImage mpsUnaryImageKernel commandBuffer sourceImage destinationImage =
  sendMessage mpsUnaryImageKernel encodeToCommandBuffer_sourceImage_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toMPSImage destinationImage)

-- | edgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of an image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen because of a              negative offset property, because the offset + clipRect.size is larger than the              source image or because the filter looks at neighboring pixels, such as a Convolution              or morphology filter.   Default: usually MPSImageEdgeModeZero. (Some MPSKernel types default              to MPSImageEdgeModeClamp, because MPSImageEdgeModeZero is either not supported or              would produce unexpected results.)
--
-- See Also: MetalPerformanceShaders.h subsubsection_edgemode
--
-- ObjC selector: @- edgeMode@
edgeMode :: IsMPSUnaryImageKernel mpsUnaryImageKernel => mpsUnaryImageKernel -> IO MPSImageEdgeMode
edgeMode mpsUnaryImageKernel =
  sendMessage mpsUnaryImageKernel edgeModeSelector

-- | edgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of an image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen because of a              negative offset property, because the offset + clipRect.size is larger than the              source image or because the filter looks at neighboring pixels, such as a Convolution              or morphology filter.   Default: usually MPSImageEdgeModeZero. (Some MPSKernel types default              to MPSImageEdgeModeClamp, because MPSImageEdgeModeZero is either not supported or              would produce unexpected results.)
--
-- See Also: MetalPerformanceShaders.h subsubsection_edgemode
--
-- ObjC selector: @- setEdgeMode:@
setEdgeMode :: IsMPSUnaryImageKernel mpsUnaryImageKernel => mpsUnaryImageKernel -> MPSImageEdgeMode -> IO ()
setEdgeMode mpsUnaryImageKernel value =
  sendMessage mpsUnaryImageKernel setEdgeModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSUnaryImageKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSUnaryImageKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:inPlaceTexture:fallbackCopyAllocator:@
encodeToCommandBuffer_inPlaceTexture_fallbackCopyAllocatorSelector :: Selector '[RawId, RawId, Ptr ()] Bool
encodeToCommandBuffer_inPlaceTexture_fallbackCopyAllocatorSelector = mkSelector "encodeToCommandBuffer:inPlaceTexture:fallbackCopyAllocator:"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:destinationTexture:@
encodeToCommandBuffer_sourceTexture_destinationTextureSelector :: Selector '[RawId, RawId, RawId] ()
encodeToCommandBuffer_sourceTexture_destinationTextureSelector = mkSelector "encodeToCommandBuffer:sourceTexture:destinationTexture:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage] ()
encodeToCommandBuffer_sourceImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationImage:"

-- | @Selector@ for @edgeMode@
edgeModeSelector :: Selector '[] MPSImageEdgeMode
edgeModeSelector = mkSelector "edgeMode"

-- | @Selector@ for @setEdgeMode:@
setEdgeModeSelector :: Selector '[MPSImageEdgeMode] ()
setEdgeModeSelector = mkSelector "setEdgeMode:"

