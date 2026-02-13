{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIImage@.
module ObjC.CoreImage.CIImage
  ( CIImage
  , IsCIImage(..)
  , imageWithCGImage
  , imageWithCGImage_options
  , imageWithCGImageSource_index_options
  , imageWithCGLayer
  , imageWithCGLayer_options
  , imageWithMTLTexture_options
  , imageWithContentsOfURL
  , imageWithContentsOfURL_options
  , imageWithData
  , imageWithData_options
  , imageWithCVImageBuffer
  , imageWithCVImageBuffer_options
  , imageWithCVPixelBuffer
  , imageWithCVPixelBuffer_options
  , imageWithIOSurface
  , imageWithIOSurface_options
  , imageWithColor
  , emptyImage
  , initWithCGImage
  , initWithCGImage_options
  , initWithCGImageSource_index_options
  , initWithCGLayer
  , initWithCGLayer_options
  , initWithData
  , initWithData_options
  , initWithMTLTexture_options
  , initWithContentsOfURL
  , initWithContentsOfURL_options
  , initWithIOSurface
  , initWithIOSurface_options
  , initWithIOSurface_plane_format_options
  , initWithCVImageBuffer
  , initWithCVImageBuffer_options
  , initWithCVPixelBuffer
  , initWithCVPixelBuffer_options
  , initWithColor
  , imageByApplyingOrientation
  , imageByApplyingCGOrientation
  , imageByCompositingOverImage
  , imageByClampingToExtent
  , imageByApplyingFilter_withInputParameters
  , imageByApplyingFilter
  , imageByColorMatchingColorSpaceToWorkingSpace
  , imageByColorMatchingWorkingSpaceToColorSpace
  , imageByPremultiplyingAlpha
  , imageByUnpremultiplyingAlpha
  , imageByApplyingGaussianBlurWithSigma
  , imageBySettingProperties
  , imageBySamplingLinear
  , imageBySamplingNearest
  , imageByInsertingIntermediate
  , imageByInsertingTiledIntermediate
  , imageByApplyingGainMap
  , imageByApplyingGainMap_headroom
  , imageBySettingContentHeadroom
  , imageBySettingContentAverageLightLevel
  , imageWithImageProvider_size__format_colorSpace_options
  , initWithImageProvider_size__format_colorSpace_options
  , initWithSemanticSegmentationMatte_options
  , initWithSemanticSegmentationMatte
  , imageWithSemanticSegmentationMatte_options
  , imageWithSemanticSegmentationMatte
  , initWithPortaitEffectsMatte_options
  , initWithPortaitEffectsMatte
  , imageWithPortaitEffectsMatte_options
  , imageWithPortaitEffectsMatte
  , initWithDepthData_options
  , initWithDepthData
  , imageWithDepthData_options
  , imageWithDepthData
  , imageByConvertingWorkingSpaceToLab
  , imageByConvertingLabToWorkingSpace
  , autoAdjustmentFilters
  , autoAdjustmentFiltersWithOptions
  , blackImage
  , whiteImage
  , grayImage
  , redImage
  , greenImage
  , blueImage
  , cyanImage
  , magentaImage
  , yellowImage
  , clearImage
  , opaque
  , definition
  , url
  , colorSpace
  , contentHeadroom
  , contentAverageLightLevel
  , pixelBuffer
  , cgImage
  , semanticSegmentationMatte
  , portraitEffectsMatte
  , depthData
  , autoAdjustmentFiltersSelector
  , autoAdjustmentFiltersWithOptionsSelector
  , blackImageSelector
  , blueImageSelector
  , cgImageSelector
  , clearImageSelector
  , colorSpaceSelector
  , contentAverageLightLevelSelector
  , contentHeadroomSelector
  , cyanImageSelector
  , definitionSelector
  , depthDataSelector
  , emptyImageSelector
  , grayImageSelector
  , greenImageSelector
  , imageByApplyingCGOrientationSelector
  , imageByApplyingFilterSelector
  , imageByApplyingFilter_withInputParametersSelector
  , imageByApplyingGainMapSelector
  , imageByApplyingGainMap_headroomSelector
  , imageByApplyingGaussianBlurWithSigmaSelector
  , imageByApplyingOrientationSelector
  , imageByClampingToExtentSelector
  , imageByColorMatchingColorSpaceToWorkingSpaceSelector
  , imageByColorMatchingWorkingSpaceToColorSpaceSelector
  , imageByCompositingOverImageSelector
  , imageByConvertingLabToWorkingSpaceSelector
  , imageByConvertingWorkingSpaceToLabSelector
  , imageByInsertingIntermediateSelector
  , imageByInsertingTiledIntermediateSelector
  , imageByPremultiplyingAlphaSelector
  , imageBySamplingLinearSelector
  , imageBySamplingNearestSelector
  , imageBySettingContentAverageLightLevelSelector
  , imageBySettingContentHeadroomSelector
  , imageBySettingPropertiesSelector
  , imageByUnpremultiplyingAlphaSelector
  , imageWithCGImageSelector
  , imageWithCGImageSource_index_optionsSelector
  , imageWithCGImage_optionsSelector
  , imageWithCGLayerSelector
  , imageWithCGLayer_optionsSelector
  , imageWithCVImageBufferSelector
  , imageWithCVImageBuffer_optionsSelector
  , imageWithCVPixelBufferSelector
  , imageWithCVPixelBuffer_optionsSelector
  , imageWithColorSelector
  , imageWithContentsOfURLSelector
  , imageWithContentsOfURL_optionsSelector
  , imageWithDataSelector
  , imageWithData_optionsSelector
  , imageWithDepthDataSelector
  , imageWithDepthData_optionsSelector
  , imageWithIOSurfaceSelector
  , imageWithIOSurface_optionsSelector
  , imageWithImageProvider_size__format_colorSpace_optionsSelector
  , imageWithMTLTexture_optionsSelector
  , imageWithPortaitEffectsMatteSelector
  , imageWithPortaitEffectsMatte_optionsSelector
  , imageWithSemanticSegmentationMatteSelector
  , imageWithSemanticSegmentationMatte_optionsSelector
  , initWithCGImageSelector
  , initWithCGImageSource_index_optionsSelector
  , initWithCGImage_optionsSelector
  , initWithCGLayerSelector
  , initWithCGLayer_optionsSelector
  , initWithCVImageBufferSelector
  , initWithCVImageBuffer_optionsSelector
  , initWithCVPixelBufferSelector
  , initWithCVPixelBuffer_optionsSelector
  , initWithColorSelector
  , initWithContentsOfURLSelector
  , initWithContentsOfURL_optionsSelector
  , initWithDataSelector
  , initWithData_optionsSelector
  , initWithDepthDataSelector
  , initWithDepthData_optionsSelector
  , initWithIOSurfaceSelector
  , initWithIOSurface_optionsSelector
  , initWithIOSurface_plane_format_optionsSelector
  , initWithImageProvider_size__format_colorSpace_optionsSelector
  , initWithMTLTexture_optionsSelector
  , initWithPortaitEffectsMatteSelector
  , initWithPortaitEffectsMatte_optionsSelector
  , initWithSemanticSegmentationMatteSelector
  , initWithSemanticSegmentationMatte_optionsSelector
  , magentaImageSelector
  , opaqueSelector
  , pixelBufferSelector
  , portraitEffectsMatteSelector
  , redImageSelector
  , semanticSegmentationMatteSelector
  , urlSelector
  , whiteImageSelector
  , yellowImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ imageWithCGImage:@
imageWithCGImage :: Ptr () -> IO (Id CIImage)
imageWithCGImage image =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCGImageSelector image

-- | @+ imageWithCGImage:options:@
imageWithCGImage_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCGImage_options image options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCGImage_optionsSelector image (toNSDictionary options)

-- | @+ imageWithCGImageSource:index:options:@
imageWithCGImageSource_index_options :: IsNSDictionary dict => Ptr () -> CULong -> dict -> IO (Id CIImage)
imageWithCGImageSource_index_options source index dict =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCGImageSource_index_optionsSelector source index (toNSDictionary dict)

-- | @+ imageWithCGLayer:@
imageWithCGLayer :: Ptr () -> IO (Id CIImage)
imageWithCGLayer layer =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCGLayerSelector layer

-- | @+ imageWithCGLayer:options:@
imageWithCGLayer_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCGLayer_options layer options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCGLayer_optionsSelector layer (toNSDictionary options)

-- | @+ imageWithMTLTexture:options:@
imageWithMTLTexture_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithMTLTexture_options texture options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithMTLTexture_optionsSelector texture (toNSDictionary options)

-- | @+ imageWithContentsOfURL:@
imageWithContentsOfURL :: IsNSURL url => url -> IO (Id CIImage)
imageWithContentsOfURL url =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithContentsOfURLSelector (toNSURL url)

-- | @+ imageWithContentsOfURL:options:@
imageWithContentsOfURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id CIImage)
imageWithContentsOfURL_options url options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithContentsOfURL_optionsSelector (toNSURL url) (toNSDictionary options)

-- | @+ imageWithData:@
imageWithData :: IsNSData data_ => data_ -> IO (Id CIImage)
imageWithData data_ =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithDataSelector (toNSData data_)

-- | @+ imageWithData:options:@
imageWithData_options :: (IsNSData data_, IsNSDictionary options) => data_ -> options -> IO (Id CIImage)
imageWithData_options data_ options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithData_optionsSelector (toNSData data_) (toNSDictionary options)

-- | @+ imageWithCVImageBuffer:@
imageWithCVImageBuffer :: Ptr () -> IO (Id CIImage)
imageWithCVImageBuffer imageBuffer =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCVImageBufferSelector imageBuffer

-- | @+ imageWithCVImageBuffer:options:@
imageWithCVImageBuffer_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCVImageBuffer_options imageBuffer options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCVImageBuffer_optionsSelector imageBuffer (toNSDictionary options)

-- | @+ imageWithCVPixelBuffer:@
imageWithCVPixelBuffer :: Ptr () -> IO (Id CIImage)
imageWithCVPixelBuffer pixelBuffer =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCVPixelBufferSelector pixelBuffer

-- | @+ imageWithCVPixelBuffer:options:@
imageWithCVPixelBuffer_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCVPixelBuffer_options pixelBuffer options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithCVPixelBuffer_optionsSelector pixelBuffer (toNSDictionary options)

-- | @+ imageWithIOSurface:@
imageWithIOSurface :: Ptr () -> IO (Id CIImage)
imageWithIOSurface surface =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithIOSurfaceSelector surface

-- | @+ imageWithIOSurface:options:@
imageWithIOSurface_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithIOSurface_options surface options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithIOSurface_optionsSelector surface (toNSDictionary options)

-- | @+ imageWithColor:@
imageWithColor :: IsCIColor color => color -> IO (Id CIImage)
imageWithColor color =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithColorSelector (toCIColor color)

-- | @+ emptyImage@
emptyImage :: IO (Id CIImage)
emptyImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' emptyImageSelector

-- | @- initWithCGImage:@
initWithCGImage :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCGImage ciImage image =
  sendOwnedMessage ciImage initWithCGImageSelector image

-- | @- initWithCGImage:options:@
initWithCGImage_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCGImage_options ciImage image options =
  sendOwnedMessage ciImage initWithCGImage_optionsSelector image (toNSDictionary options)

-- | @- initWithCGImageSource:index:options:@
initWithCGImageSource_index_options :: (IsCIImage ciImage, IsNSDictionary dict) => ciImage -> Ptr () -> CULong -> dict -> IO (Id CIImage)
initWithCGImageSource_index_options ciImage source index dict =
  sendOwnedMessage ciImage initWithCGImageSource_index_optionsSelector source index (toNSDictionary dict)

-- | @- initWithCGLayer:@
initWithCGLayer :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCGLayer ciImage layer =
  sendOwnedMessage ciImage initWithCGLayerSelector layer

-- | @- initWithCGLayer:options:@
initWithCGLayer_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCGLayer_options ciImage layer options =
  sendOwnedMessage ciImage initWithCGLayer_optionsSelector layer (toNSDictionary options)

-- | @- initWithData:@
initWithData :: (IsCIImage ciImage, IsNSData data_) => ciImage -> data_ -> IO (Id CIImage)
initWithData ciImage data_ =
  sendOwnedMessage ciImage initWithDataSelector (toNSData data_)

-- | @- initWithData:options:@
initWithData_options :: (IsCIImage ciImage, IsNSData data_, IsNSDictionary options) => ciImage -> data_ -> options -> IO (Id CIImage)
initWithData_options ciImage data_ options =
  sendOwnedMessage ciImage initWithData_optionsSelector (toNSData data_) (toNSDictionary options)

-- | @- initWithMTLTexture:options:@
initWithMTLTexture_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithMTLTexture_options ciImage texture options =
  sendOwnedMessage ciImage initWithMTLTexture_optionsSelector texture (toNSDictionary options)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsCIImage ciImage, IsNSURL url) => ciImage -> url -> IO (Id CIImage)
initWithContentsOfURL ciImage url =
  sendOwnedMessage ciImage initWithContentsOfURLSelector (toNSURL url)

-- | @- initWithContentsOfURL:options:@
initWithContentsOfURL_options :: (IsCIImage ciImage, IsNSURL url, IsNSDictionary options) => ciImage -> url -> options -> IO (Id CIImage)
initWithContentsOfURL_options ciImage url options =
  sendOwnedMessage ciImage initWithContentsOfURL_optionsSelector (toNSURL url) (toNSDictionary options)

-- | @- initWithIOSurface:@
initWithIOSurface :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithIOSurface ciImage surface =
  sendOwnedMessage ciImage initWithIOSurfaceSelector surface

-- | @- initWithIOSurface:options:@
initWithIOSurface_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithIOSurface_options ciImage surface options =
  sendOwnedMessage ciImage initWithIOSurface_optionsSelector surface (toNSDictionary options)

-- | @- initWithIOSurface:plane:format:options:@
initWithIOSurface_plane_format_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> CULong -> CInt -> options -> IO (Id CIImage)
initWithIOSurface_plane_format_options ciImage surface plane format options =
  sendOwnedMessage ciImage initWithIOSurface_plane_format_optionsSelector surface plane format (toNSDictionary options)

-- | @- initWithCVImageBuffer:@
initWithCVImageBuffer :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCVImageBuffer ciImage imageBuffer =
  sendOwnedMessage ciImage initWithCVImageBufferSelector imageBuffer

-- | @- initWithCVImageBuffer:options:@
initWithCVImageBuffer_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCVImageBuffer_options ciImage imageBuffer options =
  sendOwnedMessage ciImage initWithCVImageBuffer_optionsSelector imageBuffer (toNSDictionary options)

-- | @- initWithCVPixelBuffer:@
initWithCVPixelBuffer :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCVPixelBuffer ciImage pixelBuffer =
  sendOwnedMessage ciImage initWithCVPixelBufferSelector pixelBuffer

-- | @- initWithCVPixelBuffer:options:@
initWithCVPixelBuffer_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCVPixelBuffer_options ciImage pixelBuffer options =
  sendOwnedMessage ciImage initWithCVPixelBuffer_optionsSelector pixelBuffer (toNSDictionary options)

-- | @- initWithColor:@
initWithColor :: (IsCIImage ciImage, IsCIColor color) => ciImage -> color -> IO (Id CIImage)
initWithColor ciImage color =
  sendOwnedMessage ciImage initWithColorSelector (toCIColor color)

-- | @- imageByApplyingOrientation:@
imageByApplyingOrientation :: IsCIImage ciImage => ciImage -> CInt -> IO (Id CIImage)
imageByApplyingOrientation ciImage orientation =
  sendMessage ciImage imageByApplyingOrientationSelector orientation

-- | @- imageByApplyingCGOrientation:@
imageByApplyingCGOrientation :: IsCIImage ciImage => ciImage -> CInt -> IO (Id CIImage)
imageByApplyingCGOrientation ciImage orientation =
  sendMessage ciImage imageByApplyingCGOrientationSelector orientation

-- | @- imageByCompositingOverImage:@
imageByCompositingOverImage :: (IsCIImage ciImage, IsCIImage dest) => ciImage -> dest -> IO (Id CIImage)
imageByCompositingOverImage ciImage dest =
  sendMessage ciImage imageByCompositingOverImageSelector (toCIImage dest)

-- | @- imageByClampingToExtent@
imageByClampingToExtent :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByClampingToExtent ciImage =
  sendMessage ciImage imageByClampingToExtentSelector

-- | @- imageByApplyingFilter:withInputParameters:@
imageByApplyingFilter_withInputParameters :: (IsCIImage ciImage, IsNSString filterName, IsNSDictionary params) => ciImage -> filterName -> params -> IO (Id CIImage)
imageByApplyingFilter_withInputParameters ciImage filterName params =
  sendMessage ciImage imageByApplyingFilter_withInputParametersSelector (toNSString filterName) (toNSDictionary params)

-- | @- imageByApplyingFilter:@
imageByApplyingFilter :: (IsCIImage ciImage, IsNSString filterName) => ciImage -> filterName -> IO (Id CIImage)
imageByApplyingFilter ciImage filterName =
  sendMessage ciImage imageByApplyingFilterSelector (toNSString filterName)

-- | @- imageByColorMatchingColorSpaceToWorkingSpace:@
imageByColorMatchingColorSpaceToWorkingSpace :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
imageByColorMatchingColorSpaceToWorkingSpace ciImage colorSpace =
  sendMessage ciImage imageByColorMatchingColorSpaceToWorkingSpaceSelector colorSpace

-- | @- imageByColorMatchingWorkingSpaceToColorSpace:@
imageByColorMatchingWorkingSpaceToColorSpace :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
imageByColorMatchingWorkingSpaceToColorSpace ciImage colorSpace =
  sendMessage ciImage imageByColorMatchingWorkingSpaceToColorSpaceSelector colorSpace

-- | @- imageByPremultiplyingAlpha@
imageByPremultiplyingAlpha :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByPremultiplyingAlpha ciImage =
  sendMessage ciImage imageByPremultiplyingAlphaSelector

-- | @- imageByUnpremultiplyingAlpha@
imageByUnpremultiplyingAlpha :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByUnpremultiplyingAlpha ciImage =
  sendMessage ciImage imageByUnpremultiplyingAlphaSelector

-- | Create an image by applying a gaussian blur to the receiver. - Parameters:    - sigma: The sigma of the gaussian blur to apply to the receiver.             If the sigma is very small (less than @0.16@) then the receiver is returned. - Returns:     An autoreleased ``CIImage`` instance or the received image.
--
-- ObjC selector: @- imageByApplyingGaussianBlurWithSigma:@
imageByApplyingGaussianBlurWithSigma :: IsCIImage ciImage => ciImage -> CDouble -> IO (Id CIImage)
imageByApplyingGaussianBlurWithSigma ciImage sigma =
  sendMessage ciImage imageByApplyingGaussianBlurWithSigmaSelector sigma

-- | Return a new image by changing the receiver's metadata properties.
--
-- When you create an image, Core Image sets an image’s properties to a metadata  dictionary as described here: ``properties``. Use this method to override an image’s metadata properties with new values.
--
-- - Parameters:    - properties: A dictionary of metadata properties akin to the @CGImageSourceCopyPropertiesAtIndex()@ function. - Returns:     An autoreleased ``CIImage`` instance with a copy of the new properties.
--
-- ObjC selector: @- imageBySettingProperties:@
imageBySettingProperties :: (IsCIImage ciImage, IsNSDictionary properties) => ciImage -> properties -> IO (Id CIImage)
imageBySettingProperties ciImage properties =
  sendMessage ciImage imageBySettingPropertiesSelector (toNSDictionary properties)

-- | Create an image by changing the receiver's sample mode to bilinear interpolation. - Returns:     An autoreleased ``CIImage`` instance with a bilinear sampling.
--
-- ObjC selector: @- imageBySamplingLinear@
imageBySamplingLinear :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageBySamplingLinear ciImage =
  sendMessage ciImage imageBySamplingLinearSelector

-- | Create an image by changing the receiver's sample mode to nearest neighbor. - Returns:     An autoreleased ``CIImage`` instance with a nearest sampling.
--
-- ObjC selector: @- imageBySamplingNearest@
imageBySamplingNearest :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageBySamplingNearest ciImage =
  sendMessage ciImage imageBySamplingNearestSelector

-- | Create an image that inserts a intermediate that is cacheable
--
-- This intermediate will be not be cached if ``kCIContextCacheIntermediates`` is false. - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageByInsertingIntermediate@
imageByInsertingIntermediate :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByInsertingIntermediate ciImage =
  sendMessage ciImage imageByInsertingIntermediateSelector

-- | Create an image that inserts a intermediate that is cached in tiles
--
-- This intermediate will be cacheable even if ``kCIContextCacheIntermediates`` is false. - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageByInsertingTiledIntermediate@
imageByInsertingTiledIntermediate :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByInsertingTiledIntermediate ciImage =
  sendMessage ciImage imageByInsertingTiledIntermediateSelector

-- | Create an image that applies a gain map Core Image image to the received Core Image image.
--
-- The gain map image can be obtained by creating a ``CIImage`` instance from @NSURL@/@NSData@  and setting the ``kCIImageAuxiliaryHDRGainMap`` option set to ``.
--
-- If the gain map ``CIImage`` instance doesn't have the needed ``properties`` metadata,  the received image will be returned as-is.
--
-- - Returns:     An autoreleased ``CIImage`` instance or the received image.
--
-- ObjC selector: @- imageByApplyingGainMap:@
imageByApplyingGainMap :: (IsCIImage ciImage, IsCIImage gainmap) => ciImage -> gainmap -> IO (Id CIImage)
imageByApplyingGainMap ciImage gainmap =
  sendMessage ciImage imageByApplyingGainMapSelector (toCIImage gainmap)

-- | Create an image that applies a gain map Core Image image with a specified headroom to the received Core Image image.
--
-- - Parameters:    - gainmap: The gain map ``CIImage`` instance to apply to the receiver.    - headroom: a float value that specify how much headroom the resulting image should have.                The headroom value will be limited to between 1.0 (i.e. SDR) and                 the full headroom allowed by the gain map. - Returns:     An autoreleased ``CIImage`` instance or the received image.
--
-- ObjC selector: @- imageByApplyingGainMap:headroom:@
imageByApplyingGainMap_headroom :: (IsCIImage ciImage, IsCIImage gainmap) => ciImage -> gainmap -> CFloat -> IO (Id CIImage)
imageByApplyingGainMap_headroom ciImage gainmap headroom =
  sendMessage ciImage imageByApplyingGainMap_headroomSelector (toCIImage gainmap) headroom

-- | Create an image by changing the receiver's contentHeadroom property.
--
-- Changing this value will alter the behavior of the @CIToneMapHeadroom@ and @CISystemToneMap@ filters. * If the value is set to 0.0 then the returned image's headroom is unknown. * If the value is set to 1.0 then the returned image is SDR. * If the value is set to greater 1.0 then the returned image is HDR. * Otherwise the returned image's headroom is unknown.
--
-- - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageBySettingContentHeadroom:@
imageBySettingContentHeadroom :: IsCIImage ciImage => ciImage -> CFloat -> IO (Id CIImage)
imageBySettingContentHeadroom ciImage headroom =
  sendMessage ciImage imageBySettingContentHeadroomSelector headroom

-- | Create an image by changing the receiver's contentAverageLightLevel property.
--
-- Changing this value will alter the behavior of the @CIToneMapHeadroom@ and @CISystemToneMap@ filters. * If the value is set to 0.0 or less then the returned image's ``contentAverageLightLevel`` is unknown.
--
-- - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageBySettingContentAverageLightLevel:@
imageBySettingContentAverageLightLevel :: IsCIImage ciImage => ciImage -> CFloat -> IO (Id CIImage)
imageBySettingContentAverageLightLevel ciImage average =
  sendMessage ciImage imageBySettingContentAverageLightLevelSelector average

-- | Create an image object based on pixels from an image provider object.
--
-- Core Image retains the provider object until the image is deallocated. The image provider object will not be called until the image is rendered.
--
-- - Parameters:    - provider: An object that implements the @CIImageProvider@ protocol.     - width: The width of the image.    - height: The height of the image.    - format: The ``CIFormat`` of the provided pixels.    - colorSpace: The color space that the image is defined in.           If @nil@, then the pixels will not be is not color matched to the Core Image working color space.     - options: A dictionary that contains various ``CIImageOption`` keys that affect the resulting ``CIImage``.            The option ``kCIImageProviderTileSize`` controls if and how the provider object is called in tiles.           The option ``kCIImageProviderUserInfo`` allows additional state to be passed to the provider object. - Returns:    An autoreleased ``CIImage`` object based on the data provider.
--
-- ObjC selector: @+ imageWithImageProvider:size::format:colorSpace:options:@
imageWithImageProvider_size__format_colorSpace_options :: IsNSDictionary options => RawId -> CULong -> CULong -> CInt -> Ptr () -> options -> IO (Id CIImage)
imageWithImageProvider_size__format_colorSpace_options provider width height format colorSpace options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithImageProvider_size__format_colorSpace_optionsSelector provider width height format colorSpace (toNSDictionary options)

-- | Initializes an image object based on pixels from an image provider object.
--
-- Core Image retains the provider object until the image is deallocated. The image provider object will not be called until the image is rendered.
--
-- - Parameters:    - provider: An object that implements the @CIImageProvider@ protocol.     - width: The width of the image.    - height: The height of the image.    - format: The ``CIFormat`` of the provided pixels.    - colorSpace: The color space that the image is defined in.           If @nil@, then the pixels will not be is not color matched to the Core Image working color space.     - options: A dictionary that contains various ``CIImageOption`` keys that affect the resulting ``CIImage``.            The option ``kCIImageProviderTileSize`` controls if and how the provider object is called in tiles.           The option ``kCIImageProviderUserInfo`` allows additional state to be passed to the provider object. - Returns:    An initialized ``CIImage`` object based on the data provider.
--
-- ObjC selector: @- initWithImageProvider:size::format:colorSpace:options:@
initWithImageProvider_size__format_colorSpace_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> CULong -> CULong -> CInt -> Ptr () -> options -> IO (Id CIImage)
initWithImageProvider_size__format_colorSpace_options ciImage provider width height format colorSpace options =
  sendOwnedMessage ciImage initWithImageProvider_size__format_colorSpace_optionsSelector provider width height format colorSpace (toNSDictionary options)

-- | @- initWithSemanticSegmentationMatte:options:@
initWithSemanticSegmentationMatte_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithSemanticSegmentationMatte_options ciImage matte options =
  sendOwnedMessage ciImage initWithSemanticSegmentationMatte_optionsSelector matte (toNSDictionary options)

-- | @- initWithSemanticSegmentationMatte:@
initWithSemanticSegmentationMatte :: IsCIImage ciImage => ciImage -> RawId -> IO (Id CIImage)
initWithSemanticSegmentationMatte ciImage matte =
  sendOwnedMessage ciImage initWithSemanticSegmentationMatteSelector matte

-- | @+ imageWithSemanticSegmentationMatte:options:@
imageWithSemanticSegmentationMatte_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithSemanticSegmentationMatte_options matte options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithSemanticSegmentationMatte_optionsSelector matte (toNSDictionary options)

-- | @+ imageWithSemanticSegmentationMatte:@
imageWithSemanticSegmentationMatte :: RawId -> IO (Id CIImage)
imageWithSemanticSegmentationMatte matte =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithSemanticSegmentationMatteSelector matte

-- | @- initWithPortaitEffectsMatte:options:@
initWithPortaitEffectsMatte_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithPortaitEffectsMatte_options ciImage matte options =
  sendOwnedMessage ciImage initWithPortaitEffectsMatte_optionsSelector matte (toNSDictionary options)

-- | @- initWithPortaitEffectsMatte:@
initWithPortaitEffectsMatte :: IsCIImage ciImage => ciImage -> RawId -> IO (Id CIImage)
initWithPortaitEffectsMatte ciImage matte =
  sendOwnedMessage ciImage initWithPortaitEffectsMatteSelector matte

-- | @+ imageWithPortaitEffectsMatte:options:@
imageWithPortaitEffectsMatte_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithPortaitEffectsMatte_options matte options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithPortaitEffectsMatte_optionsSelector matte (toNSDictionary options)

-- | @+ imageWithPortaitEffectsMatte:@
imageWithPortaitEffectsMatte :: RawId -> IO (Id CIImage)
imageWithPortaitEffectsMatte matte =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithPortaitEffectsMatteSelector matte

-- | @- initWithDepthData:options:@
initWithDepthData_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithDepthData_options ciImage data_ options =
  sendOwnedMessage ciImage initWithDepthData_optionsSelector data_ (toNSDictionary options)

-- | @- initWithDepthData:@
initWithDepthData :: IsCIImage ciImage => ciImage -> RawId -> IO (Id CIImage)
initWithDepthData ciImage data_ =
  sendOwnedMessage ciImage initWithDepthDataSelector data_

-- | @+ imageWithDepthData:options:@
imageWithDepthData_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithDepthData_options data_ options =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithDepthData_optionsSelector data_ (toNSDictionary options)

-- | @+ imageWithDepthData:@
imageWithDepthData :: RawId -> IO (Id CIImage)
imageWithDepthData data_ =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' imageWithDepthDataSelector data_

-- | @- imageByConvertingWorkingSpaceToLab@
imageByConvertingWorkingSpaceToLab :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByConvertingWorkingSpaceToLab ciImage =
  sendMessage ciImage imageByConvertingWorkingSpaceToLabSelector

-- | @- imageByConvertingLabToWorkingSpace@
imageByConvertingLabToWorkingSpace :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByConvertingLabToWorkingSpace ciImage =
  sendMessage ciImage imageByConvertingLabToWorkingSpaceSelector

-- | @- autoAdjustmentFilters@
autoAdjustmentFilters :: IsCIImage ciImage => ciImage -> IO (Id NSArray)
autoAdjustmentFilters ciImage =
  sendMessage ciImage autoAdjustmentFiltersSelector

-- | @- autoAdjustmentFiltersWithOptions:@
autoAdjustmentFiltersWithOptions :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> options -> IO (Id NSArray)
autoAdjustmentFiltersWithOptions ciImage options =
  sendMessage ciImage autoAdjustmentFiltersWithOptionsSelector (toNSDictionary options)

-- | @+ blackImage@
blackImage :: IO RawId
blackImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' blackImageSelector

-- | @+ whiteImage@
whiteImage :: IO RawId
whiteImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' whiteImageSelector

-- | @+ grayImage@
grayImage :: IO RawId
grayImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' grayImageSelector

-- | @+ redImage@
redImage :: IO RawId
redImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' redImageSelector

-- | @+ greenImage@
greenImage :: IO RawId
greenImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' greenImageSelector

-- | @+ blueImage@
blueImage :: IO RawId
blueImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' blueImageSelector

-- | @+ cyanImage@
cyanImage :: IO RawId
cyanImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' cyanImageSelector

-- | @+ magentaImage@
magentaImage :: IO RawId
magentaImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' magentaImageSelector

-- | @+ yellowImage@
yellowImage :: IO RawId
yellowImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' yellowImageSelector

-- | @+ clearImage@
clearImage :: IO RawId
clearImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMessage cls' clearImageSelector

-- | Returns YES if the image is known to have and alpha value of @1.0@ over the entire image extent.
--
-- ObjC selector: @- opaque@
opaque :: IsCIImage ciImage => ciImage -> IO Bool
opaque ciImage =
  sendMessage ciImage opaqueSelector

-- | @- definition@
definition :: IsCIImage ciImage => ciImage -> IO RawId
definition ciImage =
  sendMessage ciImage definitionSelector

-- | @- url@
url :: IsCIImage ciImage => ciImage -> IO RawId
url ciImage =
  sendMessage ciImage urlSelector

-- | @- colorSpace@
colorSpace :: IsCIImage ciImage => ciImage -> IO (Ptr ())
colorSpace ciImage =
  sendMessage ciImage colorSpaceSelector

-- | Returns the content headroom of the image.
--
-- If the image headroom is unknown, then the value 0.0 will be returned.
--
-- If the image headroom is known, then a value greater than or equal to 1.0 will be returned. A value of 1.0 will be returned if the image is SDR. A value greater than 1.0 will be returned if the image is HDR.
--
-- The image headroom may known when a CIImage is first initialized. If the a CIImage is initialized using: * @NSURL@ or @NSData@ : the headroom may be determined by associated metadata                          or deduced from pixel format or colorSpace information. * @CGImage@ : headroom may be determined by @CGImageGetHeadroomInfo()@                or deduced from pixel format or colorSpace information. * @IOSurface@ : then the headroom will be determined by @kIOSurfaceContentHeadroom@.               or deduced from pixel format or colorSpace information. * @CVPixelBuffer@ : then the headroom will be determined by @kCVImageBufferContentLightLevelInfoKey@.               or deduced from pixel format or colorSpace information. * @BitmapData@ : headroom may be deduced from pixel format or colorSpace information.
--
-- If the image is the result of applying a ``CIFilter-class`` or ``CIKernel``, this method will return @0.0@.
--
-- There are exceptions to this.  Applying a @CIWarpKernel@@ or certain @@CIFilter-class@`  (e.g. @CIGaussianBlur@, @CILanczosScaleTransform@, @CIAreaAverage@ and some others)  to an image will result in a ``CIImage`` instance with the same @contentHeadroom@ property value.
--
-- ObjC selector: @- contentHeadroom@
contentHeadroom :: IsCIImage ciImage => ciImage -> IO CFloat
contentHeadroom ciImage =
  sendMessage ciImage contentHeadroomSelector

-- | Returns the content average light level of the image.
--
-- If the image average light level is unknown, then the value 0.0 will be returned.
--
-- If the image headroom is known, then a value greater than or equal to 0.0 will be returned.
--
-- The image average light level may known when a CIImage is first initialized. If the a CIImage is initialized with a:  * @CGImage@ : then the headroom will be determined by @CGImageGetContentAverageLightLevel()@.  * @CVPixelBuffer@ : then the headroom will be determined by @kCVImageBufferContentLightLevelInfoKey@.
--
-- If the image is the result of applying a ``CIFilter-class`` or ``CIKernel``, this property will return @0.0@.
--
-- There are exceptions to this.  Applying a ``CIWarpKernel`` or certain ``CIFilter-class``  (e.g. @CIGaussianBlur@, @CILanczosScaleTransform@, @CIAreaAverage@ and some others)  to an image will result in a ``CIImage`` instance with the same @contentAverageLightLevel@ property value.
--
-- ObjC selector: @- contentAverageLightLevel@
contentAverageLightLevel :: IsCIImage ciImage => ciImage -> IO CFloat
contentAverageLightLevel ciImage =
  sendMessage ciImage contentAverageLightLevelSelector

-- | @- pixelBuffer@
pixelBuffer :: IsCIImage ciImage => ciImage -> IO (Ptr ())
pixelBuffer ciImage =
  sendMessage ciImage pixelBufferSelector

-- | @- CGImage@
cgImage :: IsCIImage ciImage => ciImage -> IO (Ptr ())
cgImage ciImage =
  sendMessage ciImage cgImageSelector

-- | @- semanticSegmentationMatte@
semanticSegmentationMatte :: IsCIImage ciImage => ciImage -> IO RawId
semanticSegmentationMatte ciImage =
  sendMessage ciImage semanticSegmentationMatteSelector

-- | @- portraitEffectsMatte@
portraitEffectsMatte :: IsCIImage ciImage => ciImage -> IO RawId
portraitEffectsMatte ciImage =
  sendMessage ciImage portraitEffectsMatteSelector

-- | @- depthData@
depthData :: IsCIImage ciImage => ciImage -> IO RawId
depthData ciImage =
  sendMessage ciImage depthDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageWithCGImage:@
imageWithCGImageSelector :: Selector '[Ptr ()] (Id CIImage)
imageWithCGImageSelector = mkSelector "imageWithCGImage:"

-- | @Selector@ for @imageWithCGImage:options:@
imageWithCGImage_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
imageWithCGImage_optionsSelector = mkSelector "imageWithCGImage:options:"

-- | @Selector@ for @imageWithCGImageSource:index:options:@
imageWithCGImageSource_index_optionsSelector :: Selector '[Ptr (), CULong, Id NSDictionary] (Id CIImage)
imageWithCGImageSource_index_optionsSelector = mkSelector "imageWithCGImageSource:index:options:"

-- | @Selector@ for @imageWithCGLayer:@
imageWithCGLayerSelector :: Selector '[Ptr ()] (Id CIImage)
imageWithCGLayerSelector = mkSelector "imageWithCGLayer:"

-- | @Selector@ for @imageWithCGLayer:options:@
imageWithCGLayer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
imageWithCGLayer_optionsSelector = mkSelector "imageWithCGLayer:options:"

-- | @Selector@ for @imageWithMTLTexture:options:@
imageWithMTLTexture_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
imageWithMTLTexture_optionsSelector = mkSelector "imageWithMTLTexture:options:"

-- | @Selector@ for @imageWithContentsOfURL:@
imageWithContentsOfURLSelector :: Selector '[Id NSURL] (Id CIImage)
imageWithContentsOfURLSelector = mkSelector "imageWithContentsOfURL:"

-- | @Selector@ for @imageWithContentsOfURL:options:@
imageWithContentsOfURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id CIImage)
imageWithContentsOfURL_optionsSelector = mkSelector "imageWithContentsOfURL:options:"

-- | @Selector@ for @imageWithData:@
imageWithDataSelector :: Selector '[Id NSData] (Id CIImage)
imageWithDataSelector = mkSelector "imageWithData:"

-- | @Selector@ for @imageWithData:options:@
imageWithData_optionsSelector :: Selector '[Id NSData, Id NSDictionary] (Id CIImage)
imageWithData_optionsSelector = mkSelector "imageWithData:options:"

-- | @Selector@ for @imageWithCVImageBuffer:@
imageWithCVImageBufferSelector :: Selector '[Ptr ()] (Id CIImage)
imageWithCVImageBufferSelector = mkSelector "imageWithCVImageBuffer:"

-- | @Selector@ for @imageWithCVImageBuffer:options:@
imageWithCVImageBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
imageWithCVImageBuffer_optionsSelector = mkSelector "imageWithCVImageBuffer:options:"

-- | @Selector@ for @imageWithCVPixelBuffer:@
imageWithCVPixelBufferSelector :: Selector '[Ptr ()] (Id CIImage)
imageWithCVPixelBufferSelector = mkSelector "imageWithCVPixelBuffer:"

-- | @Selector@ for @imageWithCVPixelBuffer:options:@
imageWithCVPixelBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
imageWithCVPixelBuffer_optionsSelector = mkSelector "imageWithCVPixelBuffer:options:"

-- | @Selector@ for @imageWithIOSurface:@
imageWithIOSurfaceSelector :: Selector '[Ptr ()] (Id CIImage)
imageWithIOSurfaceSelector = mkSelector "imageWithIOSurface:"

-- | @Selector@ for @imageWithIOSurface:options:@
imageWithIOSurface_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
imageWithIOSurface_optionsSelector = mkSelector "imageWithIOSurface:options:"

-- | @Selector@ for @imageWithColor:@
imageWithColorSelector :: Selector '[Id CIColor] (Id CIImage)
imageWithColorSelector = mkSelector "imageWithColor:"

-- | @Selector@ for @emptyImage@
emptyImageSelector :: Selector '[] (Id CIImage)
emptyImageSelector = mkSelector "emptyImage"

-- | @Selector@ for @initWithCGImage:@
initWithCGImageSelector :: Selector '[Ptr ()] (Id CIImage)
initWithCGImageSelector = mkSelector "initWithCGImage:"

-- | @Selector@ for @initWithCGImage:options:@
initWithCGImage_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
initWithCGImage_optionsSelector = mkSelector "initWithCGImage:options:"

-- | @Selector@ for @initWithCGImageSource:index:options:@
initWithCGImageSource_index_optionsSelector :: Selector '[Ptr (), CULong, Id NSDictionary] (Id CIImage)
initWithCGImageSource_index_optionsSelector = mkSelector "initWithCGImageSource:index:options:"

-- | @Selector@ for @initWithCGLayer:@
initWithCGLayerSelector :: Selector '[Ptr ()] (Id CIImage)
initWithCGLayerSelector = mkSelector "initWithCGLayer:"

-- | @Selector@ for @initWithCGLayer:options:@
initWithCGLayer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
initWithCGLayer_optionsSelector = mkSelector "initWithCGLayer:options:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id CIImage)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithData:options:@
initWithData_optionsSelector :: Selector '[Id NSData, Id NSDictionary] (Id CIImage)
initWithData_optionsSelector = mkSelector "initWithData:options:"

-- | @Selector@ for @initWithMTLTexture:options:@
initWithMTLTexture_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
initWithMTLTexture_optionsSelector = mkSelector "initWithMTLTexture:options:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id CIImage)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfURL:options:@
initWithContentsOfURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id CIImage)
initWithContentsOfURL_optionsSelector = mkSelector "initWithContentsOfURL:options:"

-- | @Selector@ for @initWithIOSurface:@
initWithIOSurfaceSelector :: Selector '[Ptr ()] (Id CIImage)
initWithIOSurfaceSelector = mkSelector "initWithIOSurface:"

-- | @Selector@ for @initWithIOSurface:options:@
initWithIOSurface_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
initWithIOSurface_optionsSelector = mkSelector "initWithIOSurface:options:"

-- | @Selector@ for @initWithIOSurface:plane:format:options:@
initWithIOSurface_plane_format_optionsSelector :: Selector '[Ptr (), CULong, CInt, Id NSDictionary] (Id CIImage)
initWithIOSurface_plane_format_optionsSelector = mkSelector "initWithIOSurface:plane:format:options:"

-- | @Selector@ for @initWithCVImageBuffer:@
initWithCVImageBufferSelector :: Selector '[Ptr ()] (Id CIImage)
initWithCVImageBufferSelector = mkSelector "initWithCVImageBuffer:"

-- | @Selector@ for @initWithCVImageBuffer:options:@
initWithCVImageBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
initWithCVImageBuffer_optionsSelector = mkSelector "initWithCVImageBuffer:options:"

-- | @Selector@ for @initWithCVPixelBuffer:@
initWithCVPixelBufferSelector :: Selector '[Ptr ()] (Id CIImage)
initWithCVPixelBufferSelector = mkSelector "initWithCVPixelBuffer:"

-- | @Selector@ for @initWithCVPixelBuffer:options:@
initWithCVPixelBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIImage)
initWithCVPixelBuffer_optionsSelector = mkSelector "initWithCVPixelBuffer:options:"

-- | @Selector@ for @initWithColor:@
initWithColorSelector :: Selector '[Id CIColor] (Id CIImage)
initWithColorSelector = mkSelector "initWithColor:"

-- | @Selector@ for @imageByApplyingOrientation:@
imageByApplyingOrientationSelector :: Selector '[CInt] (Id CIImage)
imageByApplyingOrientationSelector = mkSelector "imageByApplyingOrientation:"

-- | @Selector@ for @imageByApplyingCGOrientation:@
imageByApplyingCGOrientationSelector :: Selector '[CInt] (Id CIImage)
imageByApplyingCGOrientationSelector = mkSelector "imageByApplyingCGOrientation:"

-- | @Selector@ for @imageByCompositingOverImage:@
imageByCompositingOverImageSelector :: Selector '[Id CIImage] (Id CIImage)
imageByCompositingOverImageSelector = mkSelector "imageByCompositingOverImage:"

-- | @Selector@ for @imageByClampingToExtent@
imageByClampingToExtentSelector :: Selector '[] (Id CIImage)
imageByClampingToExtentSelector = mkSelector "imageByClampingToExtent"

-- | @Selector@ for @imageByApplyingFilter:withInputParameters:@
imageByApplyingFilter_withInputParametersSelector :: Selector '[Id NSString, Id NSDictionary] (Id CIImage)
imageByApplyingFilter_withInputParametersSelector = mkSelector "imageByApplyingFilter:withInputParameters:"

-- | @Selector@ for @imageByApplyingFilter:@
imageByApplyingFilterSelector :: Selector '[Id NSString] (Id CIImage)
imageByApplyingFilterSelector = mkSelector "imageByApplyingFilter:"

-- | @Selector@ for @imageByColorMatchingColorSpaceToWorkingSpace:@
imageByColorMatchingColorSpaceToWorkingSpaceSelector :: Selector '[Ptr ()] (Id CIImage)
imageByColorMatchingColorSpaceToWorkingSpaceSelector = mkSelector "imageByColorMatchingColorSpaceToWorkingSpace:"

-- | @Selector@ for @imageByColorMatchingWorkingSpaceToColorSpace:@
imageByColorMatchingWorkingSpaceToColorSpaceSelector :: Selector '[Ptr ()] (Id CIImage)
imageByColorMatchingWorkingSpaceToColorSpaceSelector = mkSelector "imageByColorMatchingWorkingSpaceToColorSpace:"

-- | @Selector@ for @imageByPremultiplyingAlpha@
imageByPremultiplyingAlphaSelector :: Selector '[] (Id CIImage)
imageByPremultiplyingAlphaSelector = mkSelector "imageByPremultiplyingAlpha"

-- | @Selector@ for @imageByUnpremultiplyingAlpha@
imageByUnpremultiplyingAlphaSelector :: Selector '[] (Id CIImage)
imageByUnpremultiplyingAlphaSelector = mkSelector "imageByUnpremultiplyingAlpha"

-- | @Selector@ for @imageByApplyingGaussianBlurWithSigma:@
imageByApplyingGaussianBlurWithSigmaSelector :: Selector '[CDouble] (Id CIImage)
imageByApplyingGaussianBlurWithSigmaSelector = mkSelector "imageByApplyingGaussianBlurWithSigma:"

-- | @Selector@ for @imageBySettingProperties:@
imageBySettingPropertiesSelector :: Selector '[Id NSDictionary] (Id CIImage)
imageBySettingPropertiesSelector = mkSelector "imageBySettingProperties:"

-- | @Selector@ for @imageBySamplingLinear@
imageBySamplingLinearSelector :: Selector '[] (Id CIImage)
imageBySamplingLinearSelector = mkSelector "imageBySamplingLinear"

-- | @Selector@ for @imageBySamplingNearest@
imageBySamplingNearestSelector :: Selector '[] (Id CIImage)
imageBySamplingNearestSelector = mkSelector "imageBySamplingNearest"

-- | @Selector@ for @imageByInsertingIntermediate@
imageByInsertingIntermediateSelector :: Selector '[] (Id CIImage)
imageByInsertingIntermediateSelector = mkSelector "imageByInsertingIntermediate"

-- | @Selector@ for @imageByInsertingTiledIntermediate@
imageByInsertingTiledIntermediateSelector :: Selector '[] (Id CIImage)
imageByInsertingTiledIntermediateSelector = mkSelector "imageByInsertingTiledIntermediate"

-- | @Selector@ for @imageByApplyingGainMap:@
imageByApplyingGainMapSelector :: Selector '[Id CIImage] (Id CIImage)
imageByApplyingGainMapSelector = mkSelector "imageByApplyingGainMap:"

-- | @Selector@ for @imageByApplyingGainMap:headroom:@
imageByApplyingGainMap_headroomSelector :: Selector '[Id CIImage, CFloat] (Id CIImage)
imageByApplyingGainMap_headroomSelector = mkSelector "imageByApplyingGainMap:headroom:"

-- | @Selector@ for @imageBySettingContentHeadroom:@
imageBySettingContentHeadroomSelector :: Selector '[CFloat] (Id CIImage)
imageBySettingContentHeadroomSelector = mkSelector "imageBySettingContentHeadroom:"

-- | @Selector@ for @imageBySettingContentAverageLightLevel:@
imageBySettingContentAverageLightLevelSelector :: Selector '[CFloat] (Id CIImage)
imageBySettingContentAverageLightLevelSelector = mkSelector "imageBySettingContentAverageLightLevel:"

-- | @Selector@ for @imageWithImageProvider:size::format:colorSpace:options:@
imageWithImageProvider_size__format_colorSpace_optionsSelector :: Selector '[RawId, CULong, CULong, CInt, Ptr (), Id NSDictionary] (Id CIImage)
imageWithImageProvider_size__format_colorSpace_optionsSelector = mkSelector "imageWithImageProvider:size::format:colorSpace:options:"

-- | @Selector@ for @initWithImageProvider:size::format:colorSpace:options:@
initWithImageProvider_size__format_colorSpace_optionsSelector :: Selector '[RawId, CULong, CULong, CInt, Ptr (), Id NSDictionary] (Id CIImage)
initWithImageProvider_size__format_colorSpace_optionsSelector = mkSelector "initWithImageProvider:size::format:colorSpace:options:"

-- | @Selector@ for @initWithSemanticSegmentationMatte:options:@
initWithSemanticSegmentationMatte_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
initWithSemanticSegmentationMatte_optionsSelector = mkSelector "initWithSemanticSegmentationMatte:options:"

-- | @Selector@ for @initWithSemanticSegmentationMatte:@
initWithSemanticSegmentationMatteSelector :: Selector '[RawId] (Id CIImage)
initWithSemanticSegmentationMatteSelector = mkSelector "initWithSemanticSegmentationMatte:"

-- | @Selector@ for @imageWithSemanticSegmentationMatte:options:@
imageWithSemanticSegmentationMatte_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
imageWithSemanticSegmentationMatte_optionsSelector = mkSelector "imageWithSemanticSegmentationMatte:options:"

-- | @Selector@ for @imageWithSemanticSegmentationMatte:@
imageWithSemanticSegmentationMatteSelector :: Selector '[RawId] (Id CIImage)
imageWithSemanticSegmentationMatteSelector = mkSelector "imageWithSemanticSegmentationMatte:"

-- | @Selector@ for @initWithPortaitEffectsMatte:options:@
initWithPortaitEffectsMatte_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
initWithPortaitEffectsMatte_optionsSelector = mkSelector "initWithPortaitEffectsMatte:options:"

-- | @Selector@ for @initWithPortaitEffectsMatte:@
initWithPortaitEffectsMatteSelector :: Selector '[RawId] (Id CIImage)
initWithPortaitEffectsMatteSelector = mkSelector "initWithPortaitEffectsMatte:"

-- | @Selector@ for @imageWithPortaitEffectsMatte:options:@
imageWithPortaitEffectsMatte_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
imageWithPortaitEffectsMatte_optionsSelector = mkSelector "imageWithPortaitEffectsMatte:options:"

-- | @Selector@ for @imageWithPortaitEffectsMatte:@
imageWithPortaitEffectsMatteSelector :: Selector '[RawId] (Id CIImage)
imageWithPortaitEffectsMatteSelector = mkSelector "imageWithPortaitEffectsMatte:"

-- | @Selector@ for @initWithDepthData:options:@
initWithDepthData_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
initWithDepthData_optionsSelector = mkSelector "initWithDepthData:options:"

-- | @Selector@ for @initWithDepthData:@
initWithDepthDataSelector :: Selector '[RawId] (Id CIImage)
initWithDepthDataSelector = mkSelector "initWithDepthData:"

-- | @Selector@ for @imageWithDepthData:options:@
imageWithDepthData_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIImage)
imageWithDepthData_optionsSelector = mkSelector "imageWithDepthData:options:"

-- | @Selector@ for @imageWithDepthData:@
imageWithDepthDataSelector :: Selector '[RawId] (Id CIImage)
imageWithDepthDataSelector = mkSelector "imageWithDepthData:"

-- | @Selector@ for @imageByConvertingWorkingSpaceToLab@
imageByConvertingWorkingSpaceToLabSelector :: Selector '[] (Id CIImage)
imageByConvertingWorkingSpaceToLabSelector = mkSelector "imageByConvertingWorkingSpaceToLab"

-- | @Selector@ for @imageByConvertingLabToWorkingSpace@
imageByConvertingLabToWorkingSpaceSelector :: Selector '[] (Id CIImage)
imageByConvertingLabToWorkingSpaceSelector = mkSelector "imageByConvertingLabToWorkingSpace"

-- | @Selector@ for @autoAdjustmentFilters@
autoAdjustmentFiltersSelector :: Selector '[] (Id NSArray)
autoAdjustmentFiltersSelector = mkSelector "autoAdjustmentFilters"

-- | @Selector@ for @autoAdjustmentFiltersWithOptions:@
autoAdjustmentFiltersWithOptionsSelector :: Selector '[Id NSDictionary] (Id NSArray)
autoAdjustmentFiltersWithOptionsSelector = mkSelector "autoAdjustmentFiltersWithOptions:"

-- | @Selector@ for @blackImage@
blackImageSelector :: Selector '[] RawId
blackImageSelector = mkSelector "blackImage"

-- | @Selector@ for @whiteImage@
whiteImageSelector :: Selector '[] RawId
whiteImageSelector = mkSelector "whiteImage"

-- | @Selector@ for @grayImage@
grayImageSelector :: Selector '[] RawId
grayImageSelector = mkSelector "grayImage"

-- | @Selector@ for @redImage@
redImageSelector :: Selector '[] RawId
redImageSelector = mkSelector "redImage"

-- | @Selector@ for @greenImage@
greenImageSelector :: Selector '[] RawId
greenImageSelector = mkSelector "greenImage"

-- | @Selector@ for @blueImage@
blueImageSelector :: Selector '[] RawId
blueImageSelector = mkSelector "blueImage"

-- | @Selector@ for @cyanImage@
cyanImageSelector :: Selector '[] RawId
cyanImageSelector = mkSelector "cyanImage"

-- | @Selector@ for @magentaImage@
magentaImageSelector :: Selector '[] RawId
magentaImageSelector = mkSelector "magentaImage"

-- | @Selector@ for @yellowImage@
yellowImageSelector :: Selector '[] RawId
yellowImageSelector = mkSelector "yellowImage"

-- | @Selector@ for @clearImage@
clearImageSelector :: Selector '[] RawId
clearImageSelector = mkSelector "clearImage"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @definition@
definitionSelector :: Selector '[] RawId
definitionSelector = mkSelector "definition"

-- | @Selector@ for @url@
urlSelector :: Selector '[] RawId
urlSelector = mkSelector "url"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Ptr ())
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @contentHeadroom@
contentHeadroomSelector :: Selector '[] CFloat
contentHeadroomSelector = mkSelector "contentHeadroom"

-- | @Selector@ for @contentAverageLightLevel@
contentAverageLightLevelSelector :: Selector '[] CFloat
contentAverageLightLevelSelector = mkSelector "contentAverageLightLevel"

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector '[] (Ptr ())
pixelBufferSelector = mkSelector "pixelBuffer"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector '[] (Ptr ())
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @semanticSegmentationMatte@
semanticSegmentationMatteSelector :: Selector '[] RawId
semanticSegmentationMatteSelector = mkSelector "semanticSegmentationMatte"

-- | @Selector@ for @portraitEffectsMatte@
portraitEffectsMatteSelector :: Selector '[] RawId
portraitEffectsMatteSelector = mkSelector "portraitEffectsMatte"

-- | @Selector@ for @depthData@
depthDataSelector :: Selector '[] RawId
depthDataSelector = mkSelector "depthData"

