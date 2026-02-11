{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSBinaryImageKernel
--
-- This depends on Metal.framework
--
-- A MPSBinaryImageKernel consumes two MTLTextures and produces one MTLTexture.
--
-- Generated bindings for @MPSBinaryImageKernel@.
module ObjC.MetalPerformanceShaders.MPSBinaryImageKernel
  ( MPSBinaryImageKernel
  , IsMPSBinaryImageKernel(..)
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_primaryTexture_inPlaceSecondaryTexture_fallbackCopyAllocator
  , encodeToCommandBuffer_inPlacePrimaryTexture_secondaryTexture_fallbackCopyAllocator
  , encodeToCommandBuffer_primaryTexture_secondaryTexture_destinationTexture
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage
  , primaryEdgeMode
  , setPrimaryEdgeMode
  , secondaryEdgeMode
  , setSecondaryEdgeMode
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_primaryTexture_inPlaceSecondaryTexture_fallbackCopyAllocatorSelector
  , encodeToCommandBuffer_inPlacePrimaryTexture_secondaryTexture_fallbackCopyAllocatorSelector
  , encodeToCommandBuffer_primaryTexture_secondaryTexture_destinationTextureSelector
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector
  , primaryEdgeModeSelector
  , setPrimaryEdgeModeSelector
  , secondaryEdgeModeSelector
  , setSecondaryEdgeModeSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
initWithDevice :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> RawId -> IO (Id MPSBinaryImageKernel)
initWithDevice mpsBinaryImageKernel  device =
  sendMsg mpsBinaryImageKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSBinaryImageKernel mpsBinaryImageKernel, IsNSCoder aDecoder) => mpsBinaryImageKernel -> aDecoder -> RawId -> IO (Id MPSBinaryImageKernel)
initWithCoder_device mpsBinaryImageKernel  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsBinaryImageKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | This method attempts to apply the MPSKernel in place on a texture.
--
-- In-place operation means that the same texture is used both to hold the input          image and the results. Operating in-place can be an excellent way to reduce          resource utilization, and save time and energy. While simple Metal kernels can          not operate in place because textures can not be readable and writable at the          same time, some MPSKernels can operate in place because they use          multi-pass algorithms. Whether a MPSKernel can operate in-place can          depend on current hardware, operating system revision and the parameters          and properties passed to it. You should never assume that a MPSKernel will          continue to work in place, even if you have observed it doing so before.
--
-- If the operation succeeds in-place, YES is returned.  If the in-place operation  fails and no copyAllocator is provided, then NO is returned. In neither  case is the pointer held at *texture modified.
--
-- Failure during in-place operation is common. You may find it simplifies your  code to provide a copyAllocator. When an in-place filter fails, your  copyAllocator will be invoked to create a new texture in which to write  the results, allowing the filter to proceed reliably out-of-place. The  original texture will be released, replaced with a pointer to the new texture  and YES will be returned. If the allocator returns an invalid texture, it is  released, *texture remains unmodified and NO is returned.  Please see the  MPSCopyAllocator definition for a sample allocator implementation.
--
-- Note: Image filters that look at neighboring pixel values may actually consume more        memory when operating in place than out of place. Many such operations are        tiled internally to save intermediate texture storage, but can not tile when        operating in place. The memory savings for tiling is however very short term,        typically the lifetime of the MTLCommandBuffer.
--
-- Attempt to apply a MPSKernel to a texture in place.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @primaryTexture@ — A pointer to a valid MTLTexture containing the                                      primary source image. It will not be overwritten.
--
-- @inPlaceSecondaryTexture@ — A pointer to a valid MTLTexture containing secondary image.                                      On success, the image contents and possibly texture itself                                      will be replaced with the result image.
--
-- @copyAllocator@ — An optional block to allocate a new texture to hold the                                      results, in case in-place operation is not possible. The                                      allocator may use a different MTLPixelFormat or size than                                      the original texture. You may enqueue operations on the                                      provided MTLCommandBuffer using the provided                                      MTLComputeCommandEncoder to initialize the texture contents.
--
-- Returns: On success, YES is returned. The texture may have been replaced with a new              texture if a copyAllocator was provided.  On failure, NO is returned. The              texture is unmodified.
--
-- ObjC selector: @- encodeToCommandBuffer:primaryTexture:inPlaceSecondaryTexture:fallbackCopyAllocator:@
encodeToCommandBuffer_primaryTexture_inPlaceSecondaryTexture_fallbackCopyAllocator :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> RawId -> RawId -> RawId -> Ptr () -> IO Bool
encodeToCommandBuffer_primaryTexture_inPlaceSecondaryTexture_fallbackCopyAllocator mpsBinaryImageKernel  commandBuffer primaryTexture inPlaceSecondaryTexture copyAllocator =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsBinaryImageKernel (mkSelector "encodeToCommandBuffer:primaryTexture:inPlaceSecondaryTexture:fallbackCopyAllocator:") retCULong [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId primaryTexture) :: Ptr ()), argPtr (castPtr (unRawId inPlaceSecondaryTexture) :: Ptr ()), argPtr (castPtr copyAllocator :: Ptr ())]

-- | Attempt to apply a MPSKernel to a texture in place.
--
-- This method attempts to apply the MPSKernel in place on a texture.
--
-- In-place operation means that the same texture is used both to hold the input
-- image and the results. Operating in-place can be an excellent way to reduce
-- resource utilization, and save time and energy. While simple Metal kernels can
-- not operate in place because textures can not be readable and writable at the
-- same time, some MPSKernels can operate in place because they use
-- multi-pass algorithms. Whether a MPSKernel can operate in-place can
-- depend on current hardware, operating system revision and the parameters
-- and properties passed to it. You should never assume that a MPSKernel will
-- continue to work in place, even if you have observed it doing so before.
--
-- If the operation succeeds in-place, YES is returned.  If the in-place operation  fails and no copyAllocator is provided, then NO is returned. In neither  case is the pointer held at *texture modified.
--
-- Failure during in-place operation is common. You may find it simplifies your  code to provide a copyAllocator. When an in-place filter fails, your  copyAllocator will be invoked to create a new texture in which to write  the results, allowing the filter to proceed reliably out-of-place. The  original texture will be released, replaced with a pointer to the new texture  and YES will be returned. If the allocator returns an invalid texture, it is  released, *texture remains unmodified and NO is returned.  Please see the  MPSCopyAllocator definition for a sample allocator implementation.
--
-- Note: Image filters that look at neighboring pixel values may actually consume more        memory when operating in place than out of place. Many such operations are        tiled internally to save intermediate texture storage, but can not tile when        operating in place. The memory savings for tiling is however very short term,        typically the lifetime of the MTLCommandBuffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @inPlacePrimaryTexture@ — A pointer to a valid MTLTexture containing secondary image.                                      On success, the image contents and possibly texture itself                                      will be replaced with the result image.
--
-- @secondaryTexture@ — A pointer to a valid MTLTexture containing the                                      primary source image. It will not be overwritten.
--
-- @copyAllocator@ — An optional block to allocate a new texture to hold the                                      results, in case in-place operation is not possible. The                                      allocator may use a different MTLPixelFormat or size than                                      the original texture. You may enqueue operations on the                                      provided MTLCommandBuffer using the provided                                      MTLComputeCommandEncoder to initialize the texture contents.
--
-- Returns: On success, YES is returned. The texture may have been replaced with a new              texture if a copyAllocator was provided.  On failure, NO is returned. The              texture is unmodified.
--
-- ObjC selector: @- encodeToCommandBuffer:inPlacePrimaryTexture:secondaryTexture:fallbackCopyAllocator:@
encodeToCommandBuffer_inPlacePrimaryTexture_secondaryTexture_fallbackCopyAllocator :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> RawId -> RawId -> RawId -> Ptr () -> IO Bool
encodeToCommandBuffer_inPlacePrimaryTexture_secondaryTexture_fallbackCopyAllocator mpsBinaryImageKernel  commandBuffer inPlacePrimaryTexture secondaryTexture copyAllocator =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsBinaryImageKernel (mkSelector "encodeToCommandBuffer:inPlacePrimaryTexture:secondaryTexture:fallbackCopyAllocator:") retCULong [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId inPlacePrimaryTexture) :: Ptr ()), argPtr (castPtr (unRawId secondaryTexture) :: Ptr ()), argPtr (castPtr copyAllocator :: Ptr ())]

-- | Encode a MPSKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @primaryTexture@ — A valid MTLTexture containing the primary source image.
--
-- @secondaryTexture@ — A valid MTLTexture containing the secondary source image.
--
-- @destinationTexture@ — A valid MTLTexture to be overwritten by result image. destinationTexture may not alias the source textures.
--
-- ObjC selector: @- encodeToCommandBuffer:primaryTexture:secondaryTexture:destinationTexture:@
encodeToCommandBuffer_primaryTexture_secondaryTexture_destinationTexture :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeToCommandBuffer_primaryTexture_secondaryTexture_destinationTexture mpsBinaryImageKernel  commandBuffer primaryTexture secondaryTexture destinationTexture =
  sendMsg mpsBinaryImageKernel (mkSelector "encodeToCommandBuffer:primaryTexture:secondaryTexture:destinationTexture:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId primaryTexture) :: Ptr ()), argPtr (castPtr (unRawId secondaryTexture) :: Ptr ()), argPtr (castPtr (unRawId destinationTexture) :: Ptr ())]

-- | Encode a MPSKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @primaryImage@ — A valid MPSImage containing the primary source image.
--
-- @secondaryImage@ — A valid MPSImage containing the secondary source image.
--
-- @destinationImage@ — A valid MPSImage to be overwritten by result image. destinationImage may not alias the source images.
--
-- ObjC selector: @- encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage :: (IsMPSBinaryImageKernel mpsBinaryImageKernel, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsMPSImage destinationImage) => mpsBinaryImageKernel -> RawId -> primaryImage -> secondaryImage -> destinationImage -> IO ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage mpsBinaryImageKernel  commandBuffer primaryImage secondaryImage destinationImage =
withObjCPtr primaryImage $ \raw_primaryImage ->
  withObjCPtr secondaryImage $ \raw_secondaryImage ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpsBinaryImageKernel (mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_primaryImage :: Ptr ()), argPtr (castPtr raw_secondaryImage :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | primaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of a source image. This can happen because of a              negative offset property, because the offset + clipRect.size is larger than the              source image or because the filter looks at neighboring pixels, such as a Convolution              or morphology filter.   Default: usually MPSImageEdgeModeZero. (Some MPSKernel types default              to MPSImageEdgeModeClamp, because MPSImageEdgeModeZero is either not supported or              would produce unexpected results.)
--
-- See Also: MetalPerformanceShaders.h  subsubsection_edgemode
--
-- ObjC selector: @- primaryEdgeMode@
primaryEdgeMode :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> IO MPSImageEdgeMode
primaryEdgeMode mpsBinaryImageKernel  =
  fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpsBinaryImageKernel (mkSelector "primaryEdgeMode") retCULong []

-- | primaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of a source image. This can happen because of a              negative offset property, because the offset + clipRect.size is larger than the              source image or because the filter looks at neighboring pixels, such as a Convolution              or morphology filter.   Default: usually MPSImageEdgeModeZero. (Some MPSKernel types default              to MPSImageEdgeModeClamp, because MPSImageEdgeModeZero is either not supported or              would produce unexpected results.)
--
-- See Also: MetalPerformanceShaders.h  subsubsection_edgemode
--
-- ObjC selector: @- setPrimaryEdgeMode:@
setPrimaryEdgeMode :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> MPSImageEdgeMode -> IO ()
setPrimaryEdgeMode mpsBinaryImageKernel  value =
  sendMsg mpsBinaryImageKernel (mkSelector "setPrimaryEdgeMode:") retVoid [argCULong (coerce value)]

-- | secondaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the secondary source image
--
-- Most MPSKernel objects can read off the edge of a source image. This can happen because of a              negative offset property, because the offset + clipRect.size is larger than the              source image or because the filter looks at neighboring pixels, such as a Convolution              or morphology filter.   Default: usually MPSImageEdgeModeZero. (Some MPSKernel types default              to MPSImageEdgeModeClamp, because MPSImageEdgeModeZero is either not supported or              would produce unexpected results.)
--
-- See Also: MetalPerformanceShaders.h  subsubsection_edgemode
--
-- ObjC selector: @- secondaryEdgeMode@
secondaryEdgeMode :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> IO MPSImageEdgeMode
secondaryEdgeMode mpsBinaryImageKernel  =
  fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpsBinaryImageKernel (mkSelector "secondaryEdgeMode") retCULong []

-- | secondaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the secondary source image
--
-- Most MPSKernel objects can read off the edge of a source image. This can happen because of a              negative offset property, because the offset + clipRect.size is larger than the              source image or because the filter looks at neighboring pixels, such as a Convolution              or morphology filter.   Default: usually MPSImageEdgeModeZero. (Some MPSKernel types default              to MPSImageEdgeModeClamp, because MPSImageEdgeModeZero is either not supported or              would produce unexpected results.)
--
-- See Also: MetalPerformanceShaders.h  subsubsection_edgemode
--
-- ObjC selector: @- setSecondaryEdgeMode:@
setSecondaryEdgeMode :: IsMPSBinaryImageKernel mpsBinaryImageKernel => mpsBinaryImageKernel -> MPSImageEdgeMode -> IO ()
setSecondaryEdgeMode mpsBinaryImageKernel  value =
  sendMsg mpsBinaryImageKernel (mkSelector "setSecondaryEdgeMode:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:primaryTexture:inPlaceSecondaryTexture:fallbackCopyAllocator:@
encodeToCommandBuffer_primaryTexture_inPlaceSecondaryTexture_fallbackCopyAllocatorSelector :: Selector
encodeToCommandBuffer_primaryTexture_inPlaceSecondaryTexture_fallbackCopyAllocatorSelector = mkSelector "encodeToCommandBuffer:primaryTexture:inPlaceSecondaryTexture:fallbackCopyAllocator:"

-- | @Selector@ for @encodeToCommandBuffer:inPlacePrimaryTexture:secondaryTexture:fallbackCopyAllocator:@
encodeToCommandBuffer_inPlacePrimaryTexture_secondaryTexture_fallbackCopyAllocatorSelector :: Selector
encodeToCommandBuffer_inPlacePrimaryTexture_secondaryTexture_fallbackCopyAllocatorSelector = mkSelector "encodeToCommandBuffer:inPlacePrimaryTexture:secondaryTexture:fallbackCopyAllocator:"

-- | @Selector@ for @encodeToCommandBuffer:primaryTexture:secondaryTexture:destinationTexture:@
encodeToCommandBuffer_primaryTexture_secondaryTexture_destinationTextureSelector :: Selector
encodeToCommandBuffer_primaryTexture_secondaryTexture_destinationTextureSelector = mkSelector "encodeToCommandBuffer:primaryTexture:secondaryTexture:destinationTexture:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector :: Selector
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:"

-- | @Selector@ for @primaryEdgeMode@
primaryEdgeModeSelector :: Selector
primaryEdgeModeSelector = mkSelector "primaryEdgeMode"

-- | @Selector@ for @setPrimaryEdgeMode:@
setPrimaryEdgeModeSelector :: Selector
setPrimaryEdgeModeSelector = mkSelector "setPrimaryEdgeMode:"

-- | @Selector@ for @secondaryEdgeMode@
secondaryEdgeModeSelector :: Selector
secondaryEdgeModeSelector = mkSelector "secondaryEdgeMode"

-- | @Selector@ for @setSecondaryEdgeMode:@
setSecondaryEdgeModeSelector :: Selector
setSecondaryEdgeModeSelector = mkSelector "setSecondaryEdgeMode:"

