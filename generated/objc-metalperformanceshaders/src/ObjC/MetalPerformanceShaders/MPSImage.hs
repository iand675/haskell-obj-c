{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImage
--
-- This depends on Metal.framework
--
-- A MPSImage object describes a MTLTexture that may have more than 4 channels.
--
-- Some image types, such as those found in convolutional neural networks (CNN)               differ from a standard texture in that they may have more than 4 channels              per image. While the channels could hold RGBA data, they will more commonly               hold a number of structural permutations upon a multi-channel image as the neural              network progresses. It is not uncommon for each pixel to have 32 or 64 channels               in it.
--
-- A standard MTLTexture may have no more than 4 channels. The additional              channels are stored in slices of 2d texture array (i.e. texture type is MTLTextureType2DArray)               such that 4 consecutive channels are stored in each slice of this array.              If the number of feature channels is N, number of array slices needed is (N+3)/4.              E.g. a CNN image with width 3 and height 2 with 9 channels will be stored as
--
-- slice 0   RGBA   RGBA  RGBA
-- RGBA   RGBA  RGBA
--
-- slice 1      RGBA   RGBA   RGBA
-- RGBA   RGBA   RGBA         (ASCII art /diagonal offset/ intended to show a Z dimension)
--
-- slice 2         R???   R???   R???
-- R???   R???   R???
--
-- The width and height of underlying 2d texture array is the same as the width and height of the MPSImage.              The array length is equal to (featureChannels + 3) / 4. Channels marked with ? are just              for padding and should not contain NaNs or Infs.
--
-- A MPSImage can be container of multiple CNN images for batch processing. In order to create a              MPSImage that contains N images, create MPSImageDescriptor with numberOfImages set to N.
--
-- Although a MPSImage can contain numberOfImages > 1, the actual number of images among these processed by MPSCNNKernel              is controlled by z-dimension of the clipRect. A MPSCNNKernel processes n=clipRect.size.depth images from this collection.              The starting source image index to process is given by offset.z. The starting index of the destination image is given by               clipRect.origin.z. The MPSCNNKernel takes n=clipRect.size.depth images from tje source at indices [offset.z, offset.z+n],               processes each independently and stores the result in the destination at indices [clipRect.origin.z, clipRect.origin.z+n]               respectively. Offset.z+n should be <= [src numberOfImage] and clipRect.origin.z+n should be <= [dest numberOfImages] and               offset.z must be >= 0.
--
-- Example: Suppose MPSCNNConvolution takes an input image with 8 channels and outputs an image with 16 channels. The number of              slices needed in the source 2d texture array is 2 and the number of slices needed in the destination 2d array is 4. Suppose               the source batch size is 5 and destination batch size is 4. (Multiple N-channel images can be processed concurrently in a               batch.) The number of source slices will be 2*5=10 and number of destination slices will be 4*4=16. If you want to process              just images 2 and 3 of the source and store the result at index 1 and 2 in the destination, you may achieve this by setting              offset.z=2, clipRect.origin.z=1 and clipRect.size.depth=2. MPSCNNConvolution will take, in this case, slice 4 and 5 of source and              produce slices 4 to 7 of destination. Similarly, slices 6 and 7 will be used to produce slices 8 to 11 of destination.
--
-- All MPSCNNKernels process images within each batch independently. That is, calling a MPSCNNKernel on an              batch is formally the same as calling it on each image in the batch one at a time. However, quite a lot of CPU and GPU overhead               will be avoided if batch processing is used. This is especially important for better performance on small images.
--
-- If the number of feature channels is <= 4 and numberOfImages = 1 i.e. only one slice is needed to represent a MPSImage, the underlying              metal texture type will be MTLTextureType2D rather than MTLTextureType2DArray.
--
-- There are also MPSTemporaryImages, intended for use for very short-lived image data that are produced and consumed              immediately in the same MTLCommandBuffer. They are a useful way to minimize CPU-side texture allocation costs and               greatly reduce the amount of memory used by your image pipeline.
--
-- Creation of the underlying texture may in some cases occur lazily.  You should              in general avoid calling MPSImage.texture except when unavoidable to avoid              materializing memory for longer than necessary. When possible, use the other MPSImage              properties to get information about the MPSImage instead.
--
-- Most MPSImages of 4 or fewer feature channels can generate quicklooks output in              Xcode for easy visualization of image data in the object. MPSTemporaryImages can not.
--
-- Generated bindings for @MPSImage@.
module ObjC.MetalPerformanceShaders.MPSImage
  ( MPSImage
  , IsMPSImage(..)
  , defaultAllocator
  , initWithDevice_imageDescriptor
  , initWithParentImage_sliceRange_featureChannels
  , initWithTexture_featureChannels
  , init_
  , subImageWithFeatureChannelRange
  , resourceSize
  , setPurgeableState
  , readBytes_dataLayout_imageIndex
  , writeBytes_dataLayout_imageIndex
  , synchronizeOnCommandBuffer
  , width
  , height
  , featureChannels
  , numberOfImages
  , textureType
  , pixelFormat
  , precision
  , usage
  , featureChannelFormat
  , pixelSize
  , label
  , setLabel
  , parent
  , defaultAllocatorSelector
  , initWithDevice_imageDescriptorSelector
  , initWithParentImage_sliceRange_featureChannelsSelector
  , initWithTexture_featureChannelsSelector
  , initSelector
  , subImageWithFeatureChannelRangeSelector
  , resourceSizeSelector
  , setPurgeableStateSelector
  , readBytes_dataLayout_imageIndexSelector
  , writeBytes_dataLayout_imageIndexSelector
  , synchronizeOnCommandBufferSelector
  , widthSelector
  , heightSelector
  , featureChannelsSelector
  , numberOfImagesSelector
  , textureTypeSelector
  , pixelFormatSelector
  , precisionSelector
  , usageSelector
  , featureChannelFormatSelector
  , pixelSizeSelector
  , labelSelector
  , setLabelSelector
  , parentSelector

  -- * Enum types
  , MPSDataLayout(MPSDataLayout)
  , pattern MPSDataLayoutHeightxWidthxFeatureChannels
  , pattern MPSDataLayoutFeatureChannelsxHeightxWidth
  , MPSImageFeatureChannelFormat(MPSImageFeatureChannelFormat)
  , pattern MPSImageFeatureChannelFormatNone
  , pattern MPSImageFeatureChannelFormatUnorm8
  , pattern MPSImageFeatureChannelFormatUnorm16
  , pattern MPSImageFeatureChannelFormatFloat16
  , pattern MPSImageFeatureChannelFormatFloat32
  , pattern MPSImageFeatureChannelFormat_reserved0
  , pattern MPSImageFeatureChannelFormatCount
  , MPSPurgeableState(MPSPurgeableState)
  , pattern MPSPurgeableStateAllocationDeferred
  , pattern MPSPurgeableStateKeepCurrent
  , pattern MPSPurgeableStateNonVolatile
  , pattern MPSPurgeableStateVolatile
  , pattern MPSPurgeableStateEmpty
  , MTLPixelFormat(MTLPixelFormat)
  , pattern MTLPixelFormatInvalid
  , pattern MTLPixelFormatA8Unorm
  , pattern MTLPixelFormatR8Unorm
  , pattern MTLPixelFormatR8Unorm_sRGB
  , pattern MTLPixelFormatR8Snorm
  , pattern MTLPixelFormatR8Uint
  , pattern MTLPixelFormatR8Sint
  , pattern MTLPixelFormatR16Unorm
  , pattern MTLPixelFormatR16Snorm
  , pattern MTLPixelFormatR16Uint
  , pattern MTLPixelFormatR16Sint
  , pattern MTLPixelFormatR16Float
  , pattern MTLPixelFormatRG8Unorm
  , pattern MTLPixelFormatRG8Unorm_sRGB
  , pattern MTLPixelFormatRG8Snorm
  , pattern MTLPixelFormatRG8Uint
  , pattern MTLPixelFormatRG8Sint
  , pattern MTLPixelFormatB5G6R5Unorm
  , pattern MTLPixelFormatA1BGR5Unorm
  , pattern MTLPixelFormatABGR4Unorm
  , pattern MTLPixelFormatBGR5A1Unorm
  , pattern MTLPixelFormatR32Uint
  , pattern MTLPixelFormatR32Sint
  , pattern MTLPixelFormatR32Float
  , pattern MTLPixelFormatRG16Unorm
  , pattern MTLPixelFormatRG16Snorm
  , pattern MTLPixelFormatRG16Uint
  , pattern MTLPixelFormatRG16Sint
  , pattern MTLPixelFormatRG16Float
  , pattern MTLPixelFormatRGBA8Unorm
  , pattern MTLPixelFormatRGBA8Unorm_sRGB
  , pattern MTLPixelFormatRGBA8Snorm
  , pattern MTLPixelFormatRGBA8Uint
  , pattern MTLPixelFormatRGBA8Sint
  , pattern MTLPixelFormatBGRA8Unorm
  , pattern MTLPixelFormatBGRA8Unorm_sRGB
  , pattern MTLPixelFormatRGB10A2Unorm
  , pattern MTLPixelFormatRGB10A2Uint
  , pattern MTLPixelFormatRG11B10Float
  , pattern MTLPixelFormatRGB9E5Float
  , pattern MTLPixelFormatBGR10A2Unorm
  , pattern MTLPixelFormatBGR10_XR
  , pattern MTLPixelFormatBGR10_XR_sRGB
  , pattern MTLPixelFormatRG32Uint
  , pattern MTLPixelFormatRG32Sint
  , pattern MTLPixelFormatRG32Float
  , pattern MTLPixelFormatRGBA16Unorm
  , pattern MTLPixelFormatRGBA16Snorm
  , pattern MTLPixelFormatRGBA16Uint
  , pattern MTLPixelFormatRGBA16Sint
  , pattern MTLPixelFormatRGBA16Float
  , pattern MTLPixelFormatBGRA10_XR
  , pattern MTLPixelFormatBGRA10_XR_sRGB
  , pattern MTLPixelFormatRGBA32Uint
  , pattern MTLPixelFormatRGBA32Sint
  , pattern MTLPixelFormatRGBA32Float
  , pattern MTLPixelFormatBC1_RGBA
  , pattern MTLPixelFormatBC1_RGBA_sRGB
  , pattern MTLPixelFormatBC2_RGBA
  , pattern MTLPixelFormatBC2_RGBA_sRGB
  , pattern MTLPixelFormatBC3_RGBA
  , pattern MTLPixelFormatBC3_RGBA_sRGB
  , pattern MTLPixelFormatBC4_RUnorm
  , pattern MTLPixelFormatBC4_RSnorm
  , pattern MTLPixelFormatBC5_RGUnorm
  , pattern MTLPixelFormatBC5_RGSnorm
  , pattern MTLPixelFormatBC6H_RGBFloat
  , pattern MTLPixelFormatBC6H_RGBUfloat
  , pattern MTLPixelFormatBC7_RGBAUnorm
  , pattern MTLPixelFormatBC7_RGBAUnorm_sRGB
  , pattern MTLPixelFormatPVRTC_RGB_2BPP
  , pattern MTLPixelFormatPVRTC_RGB_2BPP_sRGB
  , pattern MTLPixelFormatPVRTC_RGB_4BPP
  , pattern MTLPixelFormatPVRTC_RGB_4BPP_sRGB
  , pattern MTLPixelFormatPVRTC_RGBA_2BPP
  , pattern MTLPixelFormatPVRTC_RGBA_2BPP_sRGB
  , pattern MTLPixelFormatPVRTC_RGBA_4BPP
  , pattern MTLPixelFormatPVRTC_RGBA_4BPP_sRGB
  , pattern MTLPixelFormatEAC_R11Unorm
  , pattern MTLPixelFormatEAC_R11Snorm
  , pattern MTLPixelFormatEAC_RG11Unorm
  , pattern MTLPixelFormatEAC_RG11Snorm
  , pattern MTLPixelFormatEAC_RGBA8
  , pattern MTLPixelFormatEAC_RGBA8_sRGB
  , pattern MTLPixelFormatETC2_RGB8
  , pattern MTLPixelFormatETC2_RGB8_sRGB
  , pattern MTLPixelFormatETC2_RGB8A1
  , pattern MTLPixelFormatETC2_RGB8A1_sRGB
  , pattern MTLPixelFormatASTC_4x4_sRGB
  , pattern MTLPixelFormatASTC_5x4_sRGB
  , pattern MTLPixelFormatASTC_5x5_sRGB
  , pattern MTLPixelFormatASTC_6x5_sRGB
  , pattern MTLPixelFormatASTC_6x6_sRGB
  , pattern MTLPixelFormatASTC_8x5_sRGB
  , pattern MTLPixelFormatASTC_8x6_sRGB
  , pattern MTLPixelFormatASTC_8x8_sRGB
  , pattern MTLPixelFormatASTC_10x5_sRGB
  , pattern MTLPixelFormatASTC_10x6_sRGB
  , pattern MTLPixelFormatASTC_10x8_sRGB
  , pattern MTLPixelFormatASTC_10x10_sRGB
  , pattern MTLPixelFormatASTC_12x10_sRGB
  , pattern MTLPixelFormatASTC_12x12_sRGB
  , pattern MTLPixelFormatASTC_4x4_LDR
  , pattern MTLPixelFormatASTC_5x4_LDR
  , pattern MTLPixelFormatASTC_5x5_LDR
  , pattern MTLPixelFormatASTC_6x5_LDR
  , pattern MTLPixelFormatASTC_6x6_LDR
  , pattern MTLPixelFormatASTC_8x5_LDR
  , pattern MTLPixelFormatASTC_8x6_LDR
  , pattern MTLPixelFormatASTC_8x8_LDR
  , pattern MTLPixelFormatASTC_10x5_LDR
  , pattern MTLPixelFormatASTC_10x6_LDR
  , pattern MTLPixelFormatASTC_10x8_LDR
  , pattern MTLPixelFormatASTC_10x10_LDR
  , pattern MTLPixelFormatASTC_12x10_LDR
  , pattern MTLPixelFormatASTC_12x12_LDR
  , pattern MTLPixelFormatASTC_4x4_HDR
  , pattern MTLPixelFormatASTC_5x4_HDR
  , pattern MTLPixelFormatASTC_5x5_HDR
  , pattern MTLPixelFormatASTC_6x5_HDR
  , pattern MTLPixelFormatASTC_6x6_HDR
  , pattern MTLPixelFormatASTC_8x5_HDR
  , pattern MTLPixelFormatASTC_8x6_HDR
  , pattern MTLPixelFormatASTC_8x8_HDR
  , pattern MTLPixelFormatASTC_10x5_HDR
  , pattern MTLPixelFormatASTC_10x6_HDR
  , pattern MTLPixelFormatASTC_10x8_HDR
  , pattern MTLPixelFormatASTC_10x10_HDR
  , pattern MTLPixelFormatASTC_12x10_HDR
  , pattern MTLPixelFormatASTC_12x12_HDR
  , pattern MTLPixelFormatGBGR422
  , pattern MTLPixelFormatBGRG422
  , pattern MTLPixelFormatDepth16Unorm
  , pattern MTLPixelFormatDepth32Float
  , pattern MTLPixelFormatStencil8
  , pattern MTLPixelFormatDepth24Unorm_Stencil8
  , pattern MTLPixelFormatDepth32Float_Stencil8
  , pattern MTLPixelFormatX32_Stencil8
  , pattern MTLPixelFormatX24_Stencil8
  , pattern MTLPixelFormatUnspecialized
  , MTLTextureType(MTLTextureType)
  , pattern MTLTextureType1D
  , pattern MTLTextureType1DArray
  , pattern MTLTextureType2D
  , pattern MTLTextureType2DArray
  , pattern MTLTextureType2DMultisample
  , pattern MTLTextureTypeCube
  , pattern MTLTextureTypeCubeArray
  , pattern MTLTextureType3D
  , pattern MTLTextureType2DMultisampleArray
  , pattern MTLTextureTypeTextureBuffer
  , MTLTextureUsage(MTLTextureUsage)
  , pattern MTLTextureUsageUnknown
  , pattern MTLTextureUsageShaderRead
  , pattern MTLTextureUsageShaderWrite
  , pattern MTLTextureUsageRenderTarget
  , pattern MTLTextureUsagePixelFormatView
  , pattern MTLTextureUsageShaderAtomic

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
import ObjC.Foundation.Internal.Structs
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Get a well known MPSImageAllocator that makes MPSImages
--
-- ObjC selector: @+ defaultAllocator@
defaultAllocator :: IO RawId
defaultAllocator  =
  do
    cls' <- getRequiredClass "MPSImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "defaultAllocator") (retPtr retVoid) []

-- | Initialize an empty image object
--
-- @device@ — The device that the image will be used. May not be NULL.
--
-- @imageDescriptor@ — The MPSImageDescriptor. May not be NULL.
--
-- Returns: A valid MPSImage object or nil, if failure.
--
-- Storage to store data needed is allocated lazily on first use of MPSImage or              when application calls MPSImage.texture
--
-- ObjC selector: @- initWithDevice:imageDescriptor:@
initWithDevice_imageDescriptor :: IsMPSImage mpsImage => mpsImage -> RawId -> Const (Id MPSImageDescriptor) -> IO (Id MPSImage)
initWithDevice_imageDescriptor mpsImage  device imageDescriptor =
withObjCPtr imageDescriptor $ \raw_imageDescriptor ->
    sendMsg mpsImage (mkSelector "initWithDevice:imageDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_imageDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Use -batchRepresentation or -subImageWithFeatureChannelRange instead
--
-- Generally, you should call -batchRepresentation or -subImageWithFeatureChannelRange              instead because they are safer. This is provided so that these interfaces will work              with your MPSImage subclass.
--
-- @parent@ — The parent image that owns the texture. It may be a sub-image.
--
-- @sliceRange@ — The range of MTLTexture2dArray slices to be included in the sub-image
--
-- @featureChannels@ — The number of feature channels in the new image. The number of images                          is inferred.
--
-- Returns: A MPSImage that references a subregion of the texel storage in parent instead of              using its own storage.
--
-- ObjC selector: @- initWithParentImage:sliceRange:featureChannels:@
initWithParentImage_sliceRange_featureChannels :: (IsMPSImage mpsImage, IsMPSImage parent) => mpsImage -> parent -> NSRange -> CULong -> IO (Id MPSImage)
initWithParentImage_sliceRange_featureChannels mpsImage  parent sliceRange featureChannels =
withObjCPtr parent $ \raw_parent ->
    sendMsg mpsImage (mkSelector "initWithParentImage:sliceRange:featureChannels:") (retPtr retVoid) [argPtr (castPtr raw_parent :: Ptr ()), argNSRange sliceRange, argCULong (fromIntegral featureChannels)] >>= ownedObject . castPtr

-- | Initialize an MPSImage object using Metal texture. Metal texture has been created by              user for specific number of feature channels and number of images.
--
-- @texture@ — The MTLTexture allocated by the user to be used as backing for MPSImage.
--
-- @featureChannels@ — Number of feature channels this texture contains.
--
-- Returns: A valid MPSImage object or nil, if failure.
--
-- Application can let MPS framework allocate texture with properties specified in imageDescriptor               using initWithDevice:MPSImageDescriptor API above. However in memory intensive application,               you can save memory (and allocation/deallocation time) by using MPSTemporaryImage where MPS               framework aggressively reuse memory underlying textures on same command buffer. See MPSTemporaryImage              class for details below. However, in certain cases, application developer may want more control              on allocation, placement, reusing/recycling of memory backing textures used in application using              Metal Heaps API. In this case, application can create MPSImage from pre-allocated texture using               initWithTexture:featureChannels.
--
-- MTLTextureType of texture can be MTLTextureType2D ONLY if featureChannels <= 4 in which case              MPSImage.numberOfImages is set to 1. Else it should be MTLTextureType2DArray with              arrayLength == numberOfImage * ((featureChannels + 3)/4). MPSImage.numberOfImages is set to              texture.arrayLength / ((featureChannels + 3)/4).
--
-- For MTLTextures containing typical image data which application may obtain from MetalKit or               other libraries such as that drawn from a JPEG or PNG, featureChannels should              be set to number of valid color channel e.g. for RGB data, even thought MTLPixelFormat will be              MTLPixelFormatRGBA, featureChannels should be set to 3.
--
-- ObjC selector: @- initWithTexture:featureChannels:@
initWithTexture_featureChannels :: IsMPSImage mpsImage => mpsImage -> RawId -> CULong -> IO (Id MPSImage)
initWithTexture_featureChannels mpsImage  texture featureChannels =
  sendMsg mpsImage (mkSelector "initWithTexture:featureChannels:") (retPtr retVoid) [argPtr (castPtr (unRawId texture) :: Ptr ()), argCULong (fromIntegral featureChannels)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPSImage mpsImage => mpsImage -> IO (Id MPSImage)
init_ mpsImage  =
  sendMsg mpsImage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- subImageWithFeatureChannelRange:@
subImageWithFeatureChannelRange :: IsMPSImage mpsImage => mpsImage -> NSRange -> IO (Id MPSImage)
subImageWithFeatureChannelRange mpsImage  range =
  sendMsg mpsImage (mkSelector "subImageWithFeatureChannelRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | Get the number of bytes used to allocate underyling MTLResources
--
-- This is the size of the backing store of underlying MTLResources.                  It does not include all storage used by the object, for example                  the storage used to hold the MPSImage instantiation and MTLTexture                  is not included. It only measures the size of the allocation                  used to hold the texels in the image. This value is subject to                  change between different devices and operating systems.
--
-- Except when -initWithTexture:featureChannels: is used, most                  MPSImages (including MPSTemporaryImages) are allocated without                  a backing store. The backing store is allocated lazily when                  it is needed, typically when the .texture property is called.                  Consequently, in most cases, it should be inexpensive to make                  a MPSImage to see how much memory it will need, and release it                  if it is too large.
--
-- This method may fail in certain circumstances, such as when the                  MPSImage is created with -initWithTexture:featureChannels:, in                  which case 0 will be returned. 0 will also be returned if                  it is a sub-image or sub-batch (.parent is not nil).
--
-- ObjC selector: @- resourceSize@
resourceSize :: IsMPSImage mpsImage => mpsImage -> IO CULong
resourceSize mpsImage  =
  sendMsg mpsImage (mkSelector "resourceSize") retCULong []

-- | Set (or query) the purgeability state of a MPSImage
--
-- Usage is per [MTLResource setPurgeableState:], except that the MTLTexture might be                  MPSPurgeableStateAllocationDeferred, which means there is no texture to mark volatile / nonvolatile.                  Attempts to set purgeability on MTLTextures that have not been allocated will be ignored.
--
-- ObjC selector: @- setPurgeableState:@
setPurgeableState :: IsMPSImage mpsImage => mpsImage -> MPSPurgeableState -> IO MPSPurgeableState
setPurgeableState mpsImage  state =
  fmap (coerce :: CULong -> MPSPurgeableState) $ sendMsg mpsImage (mkSelector "setPurgeableState:") retCULong [argCULong (coerce state)]

-- | readBytes
--
-- Get the values inside MPSImage and put them in the Buffer passed in.
--
-- @dataBytes@ — The array allocated by the user to be used to put data from MPSImage, the length should be                                               imageWidth * imageHeight * numberOfFeatureChannels and dataType should be inferred from pixelFormat                                               defined in the Image Descriptor.
--
-- @dataLayout@ — The enum tells how to layout MPS data in the buffer.
--
-- @imageIndex@ — Image index in MPSImage to read from.
--
-- Use the enum to set data is coming in with what order. The data type will be determined by the pixelFormat                  defined in the Image Descriptor. Region is full image, buffer width and height is same as MPSImage width and height.
--
-- ObjC selector: @- readBytes:dataLayout:imageIndex:@
readBytes_dataLayout_imageIndex :: IsMPSImage mpsImage => mpsImage -> Ptr () -> MPSDataLayout -> CULong -> IO ()
readBytes_dataLayout_imageIndex mpsImage  dataBytes dataLayout imageIndex =
  sendMsg mpsImage (mkSelector "readBytes:dataLayout:imageIndex:") retVoid [argPtr dataBytes, argCULong (coerce dataLayout), argCULong (fromIntegral imageIndex)]

-- | writeBytes
--
-- Set the values inside MPSImage with the Buffer passed in.
--
-- @dataBytes@ — The array allocated by the user to be used to put data from MPSImage, the length should be                                               imageWidth * imageHeight * numberOfFeatureChannels and dataType should be inferred from pixelFormat                                               defined in the Image Descriptor.
--
-- @dataLayout@ — The enum tells how to layout MPS data in the buffer.
--
-- @imageIndex@ — Image index in MPSImage to write to.
--
-- Use the enum to set data is coming in with what order. The data type will be determined by the pixelFormat                  defined in the Image Descriptor. Region is full image, buffer width and height is same as MPSImage width and height.
--
-- ObjC selector: @- writeBytes:dataLayout:imageIndex:@
writeBytes_dataLayout_imageIndex :: IsMPSImage mpsImage => mpsImage -> Const (Ptr ()) -> MPSDataLayout -> CULong -> IO ()
writeBytes_dataLayout_imageIndex mpsImage  dataBytes dataLayout imageIndex =
  sendMsg mpsImage (mkSelector "writeBytes:dataLayout:imageIndex:") retVoid [argPtr (unConst dataBytes), argCULong (coerce dataLayout), argCULong (fromIntegral imageIndex)]

-- | Flush the underlying MTLTexture from the device's caches, and invalidate any CPU caches if needed.
--
-- This will call [id <MTLBlitEncoder> synchronizeResource: ] on the image's MTLTexture, if any.              This is necessary for all MTLStorageModeManaged resources. For other resources, including temporary              resources (these are all MTLStorageModePrivate), and textures that have not yet been allocated, nothing is done.              It is more efficient to use this method than to attempt to do this yourself with the texture property.
--
-- @commandBuffer@ — The commandbuffer on which to synchronize
--
-- ObjC selector: @- synchronizeOnCommandBuffer:@
synchronizeOnCommandBuffer :: IsMPSImage mpsImage => mpsImage -> RawId -> IO ()
synchronizeOnCommandBuffer mpsImage  commandBuffer =
  sendMsg mpsImage (mkSelector "synchronizeOnCommandBuffer:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ())]

-- | width
--
-- The formal width of the image in pixels.
--
-- ObjC selector: @- width@
width :: IsMPSImage mpsImage => mpsImage -> IO CULong
width mpsImage  =
  sendMsg mpsImage (mkSelector "width") retCULong []

-- | height
--
-- The formal height of the image in pixels.
--
-- ObjC selector: @- height@
height :: IsMPSImage mpsImage => mpsImage -> IO CULong
height mpsImage  =
  sendMsg mpsImage (mkSelector "height") retCULong []

-- | featureChannels
--
-- The number of feature channels per pixel.
--
-- ObjC selector: @- featureChannels@
featureChannels :: IsMPSImage mpsImage => mpsImage -> IO CULong
featureChannels mpsImage  =
  sendMsg mpsImage (mkSelector "featureChannels") retCULong []

-- | numberOfImages
--
-- numberOfImages for batch processing
--
-- ObjC selector: @- numberOfImages@
numberOfImages :: IsMPSImage mpsImage => mpsImage -> IO CULong
numberOfImages mpsImage  =
  sendMsg mpsImage (mkSelector "numberOfImages") retCULong []

-- | textureType
--
-- The type of the underlying texture, typically MTLTextureType2D              or MTLTextureType2DArray
--
-- ObjC selector: @- textureType@
textureType :: IsMPSImage mpsImage => mpsImage -> IO MTLTextureType
textureType mpsImage  =
  fmap (coerce :: CULong -> MTLTextureType) $ sendMsg mpsImage (mkSelector "textureType") retCULong []

-- | pixelFormat
--
-- The MTLPixelFormat of the underlying texture
--
-- Note that in some cases, this value may be misleading. For example,              float16 data (BFloat16) is sometimes stored in MTLPixelFormatRGBA16Unorm              Please consult the featureChannelFormat.
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsMPSImage mpsImage => mpsImage -> IO MTLPixelFormat
pixelFormat mpsImage  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mpsImage (mkSelector "pixelFormat") retCULong []

-- | precision
--
-- The number of bits of numeric precision available for each feature channel.
--
-- This is precision, not size.  That is, float is 24 bits, not 32. half              precision floating-point is 11 bits, not 16. SNorm formats have one less              bit of precision for the sign bit, etc. For formats like MTLPixelFormatB5G6R5Unorm              it is the precision of the most precise channel, in this case 6.  When this              information is unavailable, typically compressed formats, 0 will be returned.
--
-- ObjC selector: @- precision@
precision :: IsMPSImage mpsImage => mpsImage -> IO CULong
precision mpsImage  =
  sendMsg mpsImage (mkSelector "precision") retCULong []

-- | usage
--
-- Description of texture usage.
--
-- ObjC selector: @- usage@
usage :: IsMPSImage mpsImage => mpsImage -> IO MTLTextureUsage
usage mpsImage  =
  fmap (coerce :: CULong -> MTLTextureUsage) $ sendMsg mpsImage (mkSelector "usage") retCULong []

-- | featureChannelFormat
--
-- The true encoding of the feature channels
--
-- ObjC selector: @- featureChannelFormat@
featureChannelFormat :: IsMPSImage mpsImage => mpsImage -> IO MPSImageFeatureChannelFormat
featureChannelFormat mpsImage  =
  fmap (coerce :: CULong -> MPSImageFeatureChannelFormat) $ sendMsg mpsImage (mkSelector "featureChannelFormat") retCULong []

-- | pixelSize
--
-- Number of bytes from the first byte of one pixel to the first byte of the next               pixel in storage order.  (Includes padding.)
--
-- ObjC selector: @- pixelSize@
pixelSize :: IsMPSImage mpsImage => mpsImage -> IO CULong
pixelSize mpsImage  =
  sendMsg mpsImage (mkSelector "pixelSize") retCULong []

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- label@
label :: IsMPSImage mpsImage => mpsImage -> IO (Id NSString)
label mpsImage  =
  sendMsg mpsImage (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | label
--
-- A string to help identify this object.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMPSImage mpsImage, IsNSString value) => mpsImage -> value -> IO ()
setLabel mpsImage  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsImage (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The MPSImage from which this MPSImage was derived. Otherwise nil.
--
-- This will point to the original image if this image was created using              -batchRepresentation, -batchRepresentationWithRange: or              -subImageWithFeatureChannelRange:.
--
-- ObjC selector: @- parent@
parent :: IsMPSImage mpsImage => mpsImage -> IO (Id MPSImage)
parent mpsImage  =
  sendMsg mpsImage (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultAllocator@
defaultAllocatorSelector :: Selector
defaultAllocatorSelector = mkSelector "defaultAllocator"

-- | @Selector@ for @initWithDevice:imageDescriptor:@
initWithDevice_imageDescriptorSelector :: Selector
initWithDevice_imageDescriptorSelector = mkSelector "initWithDevice:imageDescriptor:"

-- | @Selector@ for @initWithParentImage:sliceRange:featureChannels:@
initWithParentImage_sliceRange_featureChannelsSelector :: Selector
initWithParentImage_sliceRange_featureChannelsSelector = mkSelector "initWithParentImage:sliceRange:featureChannels:"

-- | @Selector@ for @initWithTexture:featureChannels:@
initWithTexture_featureChannelsSelector :: Selector
initWithTexture_featureChannelsSelector = mkSelector "initWithTexture:featureChannels:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @subImageWithFeatureChannelRange:@
subImageWithFeatureChannelRangeSelector :: Selector
subImageWithFeatureChannelRangeSelector = mkSelector "subImageWithFeatureChannelRange:"

-- | @Selector@ for @resourceSize@
resourceSizeSelector :: Selector
resourceSizeSelector = mkSelector "resourceSize"

-- | @Selector@ for @setPurgeableState:@
setPurgeableStateSelector :: Selector
setPurgeableStateSelector = mkSelector "setPurgeableState:"

-- | @Selector@ for @readBytes:dataLayout:imageIndex:@
readBytes_dataLayout_imageIndexSelector :: Selector
readBytes_dataLayout_imageIndexSelector = mkSelector "readBytes:dataLayout:imageIndex:"

-- | @Selector@ for @writeBytes:dataLayout:imageIndex:@
writeBytes_dataLayout_imageIndexSelector :: Selector
writeBytes_dataLayout_imageIndexSelector = mkSelector "writeBytes:dataLayout:imageIndex:"

-- | @Selector@ for @synchronizeOnCommandBuffer:@
synchronizeOnCommandBufferSelector :: Selector
synchronizeOnCommandBufferSelector = mkSelector "synchronizeOnCommandBuffer:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @featureChannels@
featureChannelsSelector :: Selector
featureChannelsSelector = mkSelector "featureChannels"

-- | @Selector@ for @numberOfImages@
numberOfImagesSelector :: Selector
numberOfImagesSelector = mkSelector "numberOfImages"

-- | @Selector@ for @textureType@
textureTypeSelector :: Selector
textureTypeSelector = mkSelector "textureType"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @precision@
precisionSelector :: Selector
precisionSelector = mkSelector "precision"

-- | @Selector@ for @usage@
usageSelector :: Selector
usageSelector = mkSelector "usage"

-- | @Selector@ for @featureChannelFormat@
featureChannelFormatSelector :: Selector
featureChannelFormatSelector = mkSelector "featureChannelFormat"

-- | @Selector@ for @pixelSize@
pixelSizeSelector :: Selector
pixelSizeSelector = mkSelector "pixelSize"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

