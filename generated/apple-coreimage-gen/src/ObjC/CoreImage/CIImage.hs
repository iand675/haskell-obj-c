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
  , imageWithCGImageSelector
  , imageWithCGImage_optionsSelector
  , imageWithCGImageSource_index_optionsSelector
  , imageWithCGLayerSelector
  , imageWithCGLayer_optionsSelector
  , imageWithMTLTexture_optionsSelector
  , imageWithContentsOfURLSelector
  , imageWithContentsOfURL_optionsSelector
  , imageWithDataSelector
  , imageWithData_optionsSelector
  , imageWithCVImageBufferSelector
  , imageWithCVImageBuffer_optionsSelector
  , imageWithCVPixelBufferSelector
  , imageWithCVPixelBuffer_optionsSelector
  , imageWithIOSurfaceSelector
  , imageWithIOSurface_optionsSelector
  , imageWithColorSelector
  , emptyImageSelector
  , initWithCGImageSelector
  , initWithCGImage_optionsSelector
  , initWithCGImageSource_index_optionsSelector
  , initWithCGLayerSelector
  , initWithCGLayer_optionsSelector
  , initWithDataSelector
  , initWithData_optionsSelector
  , initWithMTLTexture_optionsSelector
  , initWithContentsOfURLSelector
  , initWithContentsOfURL_optionsSelector
  , initWithIOSurfaceSelector
  , initWithIOSurface_optionsSelector
  , initWithIOSurface_plane_format_optionsSelector
  , initWithCVImageBufferSelector
  , initWithCVImageBuffer_optionsSelector
  , initWithCVPixelBufferSelector
  , initWithCVPixelBuffer_optionsSelector
  , initWithColorSelector
  , imageByApplyingOrientationSelector
  , imageByApplyingCGOrientationSelector
  , imageByCompositingOverImageSelector
  , imageByClampingToExtentSelector
  , imageByApplyingFilter_withInputParametersSelector
  , imageByApplyingFilterSelector
  , imageByColorMatchingColorSpaceToWorkingSpaceSelector
  , imageByColorMatchingWorkingSpaceToColorSpaceSelector
  , imageByPremultiplyingAlphaSelector
  , imageByUnpremultiplyingAlphaSelector
  , imageByApplyingGaussianBlurWithSigmaSelector
  , imageBySettingPropertiesSelector
  , imageBySamplingLinearSelector
  , imageBySamplingNearestSelector
  , imageByInsertingIntermediateSelector
  , imageByInsertingTiledIntermediateSelector
  , imageByApplyingGainMapSelector
  , imageByApplyingGainMap_headroomSelector
  , imageBySettingContentHeadroomSelector
  , imageBySettingContentAverageLightLevelSelector
  , imageWithImageProvider_size__format_colorSpace_optionsSelector
  , initWithImageProvider_size__format_colorSpace_optionsSelector
  , initWithSemanticSegmentationMatte_optionsSelector
  , initWithSemanticSegmentationMatteSelector
  , imageWithSemanticSegmentationMatte_optionsSelector
  , imageWithSemanticSegmentationMatteSelector
  , initWithPortaitEffectsMatte_optionsSelector
  , initWithPortaitEffectsMatteSelector
  , imageWithPortaitEffectsMatte_optionsSelector
  , imageWithPortaitEffectsMatteSelector
  , initWithDepthData_optionsSelector
  , initWithDepthDataSelector
  , imageWithDepthData_optionsSelector
  , imageWithDepthDataSelector
  , imageByConvertingWorkingSpaceToLabSelector
  , imageByConvertingLabToWorkingSpaceSelector
  , autoAdjustmentFiltersSelector
  , autoAdjustmentFiltersWithOptionsSelector
  , blackImageSelector
  , whiteImageSelector
  , grayImageSelector
  , redImageSelector
  , greenImageSelector
  , blueImageSelector
  , cyanImageSelector
  , magentaImageSelector
  , yellowImageSelector
  , clearImageSelector
  , opaqueSelector
  , definitionSelector
  , urlSelector
  , colorSpaceSelector
  , contentHeadroomSelector
  , contentAverageLightLevelSelector
  , pixelBufferSelector
  , cgImageSelector
  , semanticSegmentationMatteSelector
  , portraitEffectsMatteSelector
  , depthDataSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ imageWithCGImage:@
imageWithCGImage :: Ptr () -> IO (Id CIImage)
imageWithCGImage image =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithCGImage:") (retPtr retVoid) [argPtr image] >>= retainedObject . castPtr

-- | @+ imageWithCGImage:options:@
imageWithCGImage_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCGImage_options image options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithCGImage:options:") (retPtr retVoid) [argPtr image, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithCGImageSource:index:options:@
imageWithCGImageSource_index_options :: IsNSDictionary dict => Ptr () -> CULong -> dict -> IO (Id CIImage)
imageWithCGImageSource_index_options source index dict =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr dict $ \raw_dict ->
      sendClassMsg cls' (mkSelector "imageWithCGImageSource:index:options:") (retPtr retVoid) [argPtr source, argCULong index, argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithCGLayer:@
imageWithCGLayer :: Ptr () -> IO (Id CIImage)
imageWithCGLayer layer =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithCGLayer:") (retPtr retVoid) [argPtr layer] >>= retainedObject . castPtr

-- | @+ imageWithCGLayer:options:@
imageWithCGLayer_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCGLayer_options layer options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithCGLayer:options:") (retPtr retVoid) [argPtr layer, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithMTLTexture:options:@
imageWithMTLTexture_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithMTLTexture_options texture options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithMTLTexture:options:") (retPtr retVoid) [argPtr (castPtr (unRawId texture) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithContentsOfURL:@
imageWithContentsOfURL :: IsNSURL url => url -> IO (Id CIImage)
imageWithContentsOfURL url =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "imageWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithContentsOfURL:options:@
imageWithContentsOfURL_options :: (IsNSURL url, IsNSDictionary options) => url -> options -> IO (Id CIImage)
imageWithContentsOfURL_options url options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "imageWithContentsOfURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithData:@
imageWithData :: IsNSData data_ => data_ -> IO (Id CIImage)
imageWithData data_ =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "imageWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithData:options:@
imageWithData_options :: (IsNSData data_, IsNSDictionary options) => data_ -> options -> IO (Id CIImage)
imageWithData_options data_ options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "imageWithData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithCVImageBuffer:@
imageWithCVImageBuffer :: Ptr () -> IO (Id CIImage)
imageWithCVImageBuffer imageBuffer =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithCVImageBuffer:") (retPtr retVoid) [argPtr imageBuffer] >>= retainedObject . castPtr

-- | @+ imageWithCVImageBuffer:options:@
imageWithCVImageBuffer_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCVImageBuffer_options imageBuffer options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithCVImageBuffer:options:") (retPtr retVoid) [argPtr imageBuffer, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithCVPixelBuffer:@
imageWithCVPixelBuffer :: Ptr () -> IO (Id CIImage)
imageWithCVPixelBuffer pixelBuffer =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithCVPixelBuffer:") (retPtr retVoid) [argPtr pixelBuffer] >>= retainedObject . castPtr

-- | @+ imageWithCVPixelBuffer:options:@
imageWithCVPixelBuffer_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithCVPixelBuffer_options pixelBuffer options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithCVPixelBuffer:options:") (retPtr retVoid) [argPtr pixelBuffer, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithIOSurface:@
imageWithIOSurface :: Ptr () -> IO (Id CIImage)
imageWithIOSurface surface =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithIOSurface:") (retPtr retVoid) [argPtr surface] >>= retainedObject . castPtr

-- | @+ imageWithIOSurface:options:@
imageWithIOSurface_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIImage)
imageWithIOSurface_options surface options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithIOSurface:options:") (retPtr retVoid) [argPtr surface, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithColor:@
imageWithColor :: IsCIColor color => color -> IO (Id CIImage)
imageWithColor color =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr color $ \raw_color ->
      sendClassMsg cls' (mkSelector "imageWithColor:") (retPtr retVoid) [argPtr (castPtr raw_color :: Ptr ())] >>= retainedObject . castPtr

-- | @+ emptyImage@
emptyImage :: IO (Id CIImage)
emptyImage  =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "emptyImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithCGImage:@
initWithCGImage :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCGImage ciImage  image =
    sendMsg ciImage (mkSelector "initWithCGImage:") (retPtr retVoid) [argPtr image] >>= ownedObject . castPtr

-- | @- initWithCGImage:options:@
initWithCGImage_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCGImage_options ciImage  image options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithCGImage:options:") (retPtr retVoid) [argPtr image, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCGImageSource:index:options:@
initWithCGImageSource_index_options :: (IsCIImage ciImage, IsNSDictionary dict) => ciImage -> Ptr () -> CULong -> dict -> IO (Id CIImage)
initWithCGImageSource_index_options ciImage  source index dict =
  withObjCPtr dict $ \raw_dict ->
      sendMsg ciImage (mkSelector "initWithCGImageSource:index:options:") (retPtr retVoid) [argPtr source, argCULong index, argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCGLayer:@
initWithCGLayer :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCGLayer ciImage  layer =
    sendMsg ciImage (mkSelector "initWithCGLayer:") (retPtr retVoid) [argPtr layer] >>= ownedObject . castPtr

-- | @- initWithCGLayer:options:@
initWithCGLayer_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCGLayer_options ciImage  layer options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithCGLayer:options:") (retPtr retVoid) [argPtr layer, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsCIImage ciImage, IsNSData data_) => ciImage -> data_ -> IO (Id CIImage)
initWithData ciImage  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg ciImage (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:options:@
initWithData_options :: (IsCIImage ciImage, IsNSData data_, IsNSDictionary options) => ciImage -> data_ -> options -> IO (Id CIImage)
initWithData_options ciImage  data_ options =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr options $ \raw_options ->
        sendMsg ciImage (mkSelector "initWithData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithMTLTexture:options:@
initWithMTLTexture_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithMTLTexture_options ciImage  texture options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithMTLTexture:options:") (retPtr retVoid) [argPtr (castPtr (unRawId texture) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsCIImage ciImage, IsNSURL url) => ciImage -> url -> IO (Id CIImage)
initWithContentsOfURL ciImage  url =
  withObjCPtr url $ \raw_url ->
      sendMsg ciImage (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:options:@
initWithContentsOfURL_options :: (IsCIImage ciImage, IsNSURL url, IsNSDictionary options) => ciImage -> url -> options -> IO (Id CIImage)
initWithContentsOfURL_options ciImage  url options =
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
        sendMsg ciImage (mkSelector "initWithContentsOfURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIOSurface:@
initWithIOSurface :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithIOSurface ciImage  surface =
    sendMsg ciImage (mkSelector "initWithIOSurface:") (retPtr retVoid) [argPtr surface] >>= ownedObject . castPtr

-- | @- initWithIOSurface:options:@
initWithIOSurface_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithIOSurface_options ciImage  surface options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithIOSurface:options:") (retPtr retVoid) [argPtr surface, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIOSurface:plane:format:options:@
initWithIOSurface_plane_format_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> CULong -> CInt -> options -> IO (Id CIImage)
initWithIOSurface_plane_format_options ciImage  surface plane format options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithIOSurface:plane:format:options:") (retPtr retVoid) [argPtr surface, argCULong plane, argCInt format, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCVImageBuffer:@
initWithCVImageBuffer :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCVImageBuffer ciImage  imageBuffer =
    sendMsg ciImage (mkSelector "initWithCVImageBuffer:") (retPtr retVoid) [argPtr imageBuffer] >>= ownedObject . castPtr

-- | @- initWithCVImageBuffer:options:@
initWithCVImageBuffer_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCVImageBuffer_options ciImage  imageBuffer options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithCVImageBuffer:options:") (retPtr retVoid) [argPtr imageBuffer, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCVPixelBuffer:@
initWithCVPixelBuffer :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
initWithCVPixelBuffer ciImage  pixelBuffer =
    sendMsg ciImage (mkSelector "initWithCVPixelBuffer:") (retPtr retVoid) [argPtr pixelBuffer] >>= ownedObject . castPtr

-- | @- initWithCVPixelBuffer:options:@
initWithCVPixelBuffer_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> Ptr () -> options -> IO (Id CIImage)
initWithCVPixelBuffer_options ciImage  pixelBuffer options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithCVPixelBuffer:options:") (retPtr retVoid) [argPtr pixelBuffer, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithColor:@
initWithColor :: (IsCIImage ciImage, IsCIColor color) => ciImage -> color -> IO (Id CIImage)
initWithColor ciImage  color =
  withObjCPtr color $ \raw_color ->
      sendMsg ciImage (mkSelector "initWithColor:") (retPtr retVoid) [argPtr (castPtr raw_color :: Ptr ())] >>= ownedObject . castPtr

-- | @- imageByApplyingOrientation:@
imageByApplyingOrientation :: IsCIImage ciImage => ciImage -> CInt -> IO (Id CIImage)
imageByApplyingOrientation ciImage  orientation =
    sendMsg ciImage (mkSelector "imageByApplyingOrientation:") (retPtr retVoid) [argCInt orientation] >>= retainedObject . castPtr

-- | @- imageByApplyingCGOrientation:@
imageByApplyingCGOrientation :: IsCIImage ciImage => ciImage -> CInt -> IO (Id CIImage)
imageByApplyingCGOrientation ciImage  orientation =
    sendMsg ciImage (mkSelector "imageByApplyingCGOrientation:") (retPtr retVoid) [argCInt (fromIntegral orientation)] >>= retainedObject . castPtr

-- | @- imageByCompositingOverImage:@
imageByCompositingOverImage :: (IsCIImage ciImage, IsCIImage dest) => ciImage -> dest -> IO (Id CIImage)
imageByCompositingOverImage ciImage  dest =
  withObjCPtr dest $ \raw_dest ->
      sendMsg ciImage (mkSelector "imageByCompositingOverImage:") (retPtr retVoid) [argPtr (castPtr raw_dest :: Ptr ())] >>= retainedObject . castPtr

-- | @- imageByClampingToExtent@
imageByClampingToExtent :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByClampingToExtent ciImage  =
    sendMsg ciImage (mkSelector "imageByClampingToExtent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- imageByApplyingFilter:withInputParameters:@
imageByApplyingFilter_withInputParameters :: (IsCIImage ciImage, IsNSString filterName, IsNSDictionary params) => ciImage -> filterName -> params -> IO (Id CIImage)
imageByApplyingFilter_withInputParameters ciImage  filterName params =
  withObjCPtr filterName $ \raw_filterName ->
    withObjCPtr params $ \raw_params ->
        sendMsg ciImage (mkSelector "imageByApplyingFilter:withInputParameters:") (retPtr retVoid) [argPtr (castPtr raw_filterName :: Ptr ()), argPtr (castPtr raw_params :: Ptr ())] >>= retainedObject . castPtr

-- | @- imageByApplyingFilter:@
imageByApplyingFilter :: (IsCIImage ciImage, IsNSString filterName) => ciImage -> filterName -> IO (Id CIImage)
imageByApplyingFilter ciImage  filterName =
  withObjCPtr filterName $ \raw_filterName ->
      sendMsg ciImage (mkSelector "imageByApplyingFilter:") (retPtr retVoid) [argPtr (castPtr raw_filterName :: Ptr ())] >>= retainedObject . castPtr

-- | @- imageByColorMatchingColorSpaceToWorkingSpace:@
imageByColorMatchingColorSpaceToWorkingSpace :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
imageByColorMatchingColorSpaceToWorkingSpace ciImage  colorSpace =
    sendMsg ciImage (mkSelector "imageByColorMatchingColorSpaceToWorkingSpace:") (retPtr retVoid) [argPtr colorSpace] >>= retainedObject . castPtr

-- | @- imageByColorMatchingWorkingSpaceToColorSpace:@
imageByColorMatchingWorkingSpaceToColorSpace :: IsCIImage ciImage => ciImage -> Ptr () -> IO (Id CIImage)
imageByColorMatchingWorkingSpaceToColorSpace ciImage  colorSpace =
    sendMsg ciImage (mkSelector "imageByColorMatchingWorkingSpaceToColorSpace:") (retPtr retVoid) [argPtr colorSpace] >>= retainedObject . castPtr

-- | @- imageByPremultiplyingAlpha@
imageByPremultiplyingAlpha :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByPremultiplyingAlpha ciImage  =
    sendMsg ciImage (mkSelector "imageByPremultiplyingAlpha") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- imageByUnpremultiplyingAlpha@
imageByUnpremultiplyingAlpha :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByUnpremultiplyingAlpha ciImage  =
    sendMsg ciImage (mkSelector "imageByUnpremultiplyingAlpha") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create an image by applying a gaussian blur to the receiver. - Parameters:    - sigma: The sigma of the gaussian blur to apply to the receiver.             If the sigma is very small (less than @0.16@) then the receiver is returned. - Returns:     An autoreleased ``CIImage`` instance or the received image.
--
-- ObjC selector: @- imageByApplyingGaussianBlurWithSigma:@
imageByApplyingGaussianBlurWithSigma :: IsCIImage ciImage => ciImage -> CDouble -> IO (Id CIImage)
imageByApplyingGaussianBlurWithSigma ciImage  sigma =
    sendMsg ciImage (mkSelector "imageByApplyingGaussianBlurWithSigma:") (retPtr retVoid) [argCDouble sigma] >>= retainedObject . castPtr

-- | Return a new image by changing the receiver's metadata properties.
--
-- When you create an image, Core Image sets an image’s properties to a metadata  dictionary as described here: ``properties``. Use this method to override an image’s metadata properties with new values.
--
-- - Parameters:    - properties: A dictionary of metadata properties akin to the @CGImageSourceCopyPropertiesAtIndex()@ function. - Returns:     An autoreleased ``CIImage`` instance with a copy of the new properties.
--
-- ObjC selector: @- imageBySettingProperties:@
imageBySettingProperties :: (IsCIImage ciImage, IsNSDictionary properties) => ciImage -> properties -> IO (Id CIImage)
imageBySettingProperties ciImage  properties =
  withObjCPtr properties $ \raw_properties ->
      sendMsg ciImage (mkSelector "imageBySettingProperties:") (retPtr retVoid) [argPtr (castPtr raw_properties :: Ptr ())] >>= retainedObject . castPtr

-- | Create an image by changing the receiver's sample mode to bilinear interpolation. - Returns:     An autoreleased ``CIImage`` instance with a bilinear sampling.
--
-- ObjC selector: @- imageBySamplingLinear@
imageBySamplingLinear :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageBySamplingLinear ciImage  =
    sendMsg ciImage (mkSelector "imageBySamplingLinear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create an image by changing the receiver's sample mode to nearest neighbor. - Returns:     An autoreleased ``CIImage`` instance with a nearest sampling.
--
-- ObjC selector: @- imageBySamplingNearest@
imageBySamplingNearest :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageBySamplingNearest ciImage  =
    sendMsg ciImage (mkSelector "imageBySamplingNearest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create an image that inserts a intermediate that is cacheable
--
-- This intermediate will be not be cached if ``kCIContextCacheIntermediates`` is false. - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageByInsertingIntermediate@
imageByInsertingIntermediate :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByInsertingIntermediate ciImage  =
    sendMsg ciImage (mkSelector "imageByInsertingIntermediate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create an image that inserts a intermediate that is cached in tiles
--
-- This intermediate will be cacheable even if ``kCIContextCacheIntermediates`` is false. - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageByInsertingTiledIntermediate@
imageByInsertingTiledIntermediate :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByInsertingTiledIntermediate ciImage  =
    sendMsg ciImage (mkSelector "imageByInsertingTiledIntermediate") (retPtr retVoid) [] >>= retainedObject . castPtr

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
imageByApplyingGainMap ciImage  gainmap =
  withObjCPtr gainmap $ \raw_gainmap ->
      sendMsg ciImage (mkSelector "imageByApplyingGainMap:") (retPtr retVoid) [argPtr (castPtr raw_gainmap :: Ptr ())] >>= retainedObject . castPtr

-- | Create an image that applies a gain map Core Image image with a specified headroom to the received Core Image image.
--
-- - Parameters:    - gainmap: The gain map ``CIImage`` instance to apply to the receiver.    - headroom: a float value that specify how much headroom the resulting image should have.                The headroom value will be limited to between 1.0 (i.e. SDR) and                 the full headroom allowed by the gain map. - Returns:     An autoreleased ``CIImage`` instance or the received image.
--
-- ObjC selector: @- imageByApplyingGainMap:headroom:@
imageByApplyingGainMap_headroom :: (IsCIImage ciImage, IsCIImage gainmap) => ciImage -> gainmap -> CFloat -> IO (Id CIImage)
imageByApplyingGainMap_headroom ciImage  gainmap headroom =
  withObjCPtr gainmap $ \raw_gainmap ->
      sendMsg ciImage (mkSelector "imageByApplyingGainMap:headroom:") (retPtr retVoid) [argPtr (castPtr raw_gainmap :: Ptr ()), argCFloat headroom] >>= retainedObject . castPtr

-- | Create an image by changing the receiver's contentHeadroom property.
--
-- Changing this value will alter the behavior of the @CIToneMapHeadroom@ and @CISystemToneMap@ filters. * If the value is set to 0.0 then the returned image's headroom is unknown. * If the value is set to 1.0 then the returned image is SDR. * If the value is set to greater 1.0 then the returned image is HDR. * Otherwise the returned image's headroom is unknown.
--
-- - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageBySettingContentHeadroom:@
imageBySettingContentHeadroom :: IsCIImage ciImage => ciImage -> CFloat -> IO (Id CIImage)
imageBySettingContentHeadroom ciImage  headroom =
    sendMsg ciImage (mkSelector "imageBySettingContentHeadroom:") (retPtr retVoid) [argCFloat headroom] >>= retainedObject . castPtr

-- | Create an image by changing the receiver's contentAverageLightLevel property.
--
-- Changing this value will alter the behavior of the @CIToneMapHeadroom@ and @CISystemToneMap@ filters. * If the value is set to 0.0 or less then the returned image's ``contentAverageLightLevel`` is unknown.
--
-- - Returns:     An autoreleased ``CIImage``.
--
-- ObjC selector: @- imageBySettingContentAverageLightLevel:@
imageBySettingContentAverageLightLevel :: IsCIImage ciImage => ciImage -> CFloat -> IO (Id CIImage)
imageBySettingContentAverageLightLevel ciImage  average =
    sendMsg ciImage (mkSelector "imageBySettingContentAverageLightLevel:") (retPtr retVoid) [argCFloat average] >>= retainedObject . castPtr

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
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithImageProvider:size::format:colorSpace:options:") (retPtr retVoid) [argPtr (castPtr (unRawId provider) :: Ptr ()), argCULong width, argCULong height, argCInt format, argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an image object based on pixels from an image provider object.
--
-- Core Image retains the provider object until the image is deallocated. The image provider object will not be called until the image is rendered.
--
-- - Parameters:    - provider: An object that implements the @CIImageProvider@ protocol.     - width: The width of the image.    - height: The height of the image.    - format: The ``CIFormat`` of the provided pixels.    - colorSpace: The color space that the image is defined in.           If @nil@, then the pixels will not be is not color matched to the Core Image working color space.     - options: A dictionary that contains various ``CIImageOption`` keys that affect the resulting ``CIImage``.            The option ``kCIImageProviderTileSize`` controls if and how the provider object is called in tiles.           The option ``kCIImageProviderUserInfo`` allows additional state to be passed to the provider object. - Returns:    An initialized ``CIImage`` object based on the data provider.
--
-- ObjC selector: @- initWithImageProvider:size::format:colorSpace:options:@
initWithImageProvider_size__format_colorSpace_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> CULong -> CULong -> CInt -> Ptr () -> options -> IO (Id CIImage)
initWithImageProvider_size__format_colorSpace_options ciImage  provider width height format colorSpace options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithImageProvider:size::format:colorSpace:options:") (retPtr retVoid) [argPtr (castPtr (unRawId provider) :: Ptr ()), argCULong width, argCULong height, argCInt format, argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSemanticSegmentationMatte:options:@
initWithSemanticSegmentationMatte_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithSemanticSegmentationMatte_options ciImage  matte options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithSemanticSegmentationMatte:options:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSemanticSegmentationMatte:@
initWithSemanticSegmentationMatte :: IsCIImage ciImage => ciImage -> RawId -> IO (Id CIImage)
initWithSemanticSegmentationMatte ciImage  matte =
    sendMsg ciImage (mkSelector "initWithSemanticSegmentationMatte:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ())] >>= ownedObject . castPtr

-- | @+ imageWithSemanticSegmentationMatte:options:@
imageWithSemanticSegmentationMatte_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithSemanticSegmentationMatte_options matte options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithSemanticSegmentationMatte:options:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithSemanticSegmentationMatte:@
imageWithSemanticSegmentationMatte :: RawId -> IO (Id CIImage)
imageWithSemanticSegmentationMatte matte =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithSemanticSegmentationMatte:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithPortaitEffectsMatte:options:@
initWithPortaitEffectsMatte_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithPortaitEffectsMatte_options ciImage  matte options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithPortaitEffectsMatte:options:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPortaitEffectsMatte:@
initWithPortaitEffectsMatte :: IsCIImage ciImage => ciImage -> RawId -> IO (Id CIImage)
initWithPortaitEffectsMatte ciImage  matte =
    sendMsg ciImage (mkSelector "initWithPortaitEffectsMatte:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ())] >>= ownedObject . castPtr

-- | @+ imageWithPortaitEffectsMatte:options:@
imageWithPortaitEffectsMatte_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithPortaitEffectsMatte_options matte options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithPortaitEffectsMatte:options:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithPortaitEffectsMatte:@
imageWithPortaitEffectsMatte :: RawId -> IO (Id CIImage)
imageWithPortaitEffectsMatte matte =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithPortaitEffectsMatte:") (retPtr retVoid) [argPtr (castPtr (unRawId matte) :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithDepthData:options:@
initWithDepthData_options :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> RawId -> options -> IO (Id CIImage)
initWithDepthData_options ciImage  data_ options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "initWithDepthData:options:") (retPtr retVoid) [argPtr (castPtr (unRawId data_) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDepthData:@
initWithDepthData :: IsCIImage ciImage => ciImage -> RawId -> IO (Id CIImage)
initWithDepthData ciImage  data_ =
    sendMsg ciImage (mkSelector "initWithDepthData:") (retPtr retVoid) [argPtr (castPtr (unRawId data_) :: Ptr ())] >>= ownedObject . castPtr

-- | @+ imageWithDepthData:options:@
imageWithDepthData_options :: IsNSDictionary options => RawId -> options -> IO (Id CIImage)
imageWithDepthData_options data_ options =
  do
    cls' <- getRequiredClass "CIImage"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "imageWithDepthData:options:") (retPtr retVoid) [argPtr (castPtr (unRawId data_) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ imageWithDepthData:@
imageWithDepthData :: RawId -> IO (Id CIImage)
imageWithDepthData data_ =
  do
    cls' <- getRequiredClass "CIImage"
    sendClassMsg cls' (mkSelector "imageWithDepthData:") (retPtr retVoid) [argPtr (castPtr (unRawId data_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- imageByConvertingWorkingSpaceToLab@
imageByConvertingWorkingSpaceToLab :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByConvertingWorkingSpaceToLab ciImage  =
    sendMsg ciImage (mkSelector "imageByConvertingWorkingSpaceToLab") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- imageByConvertingLabToWorkingSpace@
imageByConvertingLabToWorkingSpace :: IsCIImage ciImage => ciImage -> IO (Id CIImage)
imageByConvertingLabToWorkingSpace ciImage  =
    sendMsg ciImage (mkSelector "imageByConvertingLabToWorkingSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- autoAdjustmentFilters@
autoAdjustmentFilters :: IsCIImage ciImage => ciImage -> IO (Id NSArray)
autoAdjustmentFilters ciImage  =
    sendMsg ciImage (mkSelector "autoAdjustmentFilters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- autoAdjustmentFiltersWithOptions:@
autoAdjustmentFiltersWithOptions :: (IsCIImage ciImage, IsNSDictionary options) => ciImage -> options -> IO (Id NSArray)
autoAdjustmentFiltersWithOptions ciImage  options =
  withObjCPtr options $ \raw_options ->
      sendMsg ciImage (mkSelector "autoAdjustmentFiltersWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ blackImage@
blackImage :: IO RawId
blackImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "blackImage") (retPtr retVoid) []

-- | @+ whiteImage@
whiteImage :: IO RawId
whiteImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "whiteImage") (retPtr retVoid) []

-- | @+ grayImage@
grayImage :: IO RawId
grayImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "grayImage") (retPtr retVoid) []

-- | @+ redImage@
redImage :: IO RawId
redImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "redImage") (retPtr retVoid) []

-- | @+ greenImage@
greenImage :: IO RawId
greenImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "greenImage") (retPtr retVoid) []

-- | @+ blueImage@
blueImage :: IO RawId
blueImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "blueImage") (retPtr retVoid) []

-- | @+ cyanImage@
cyanImage :: IO RawId
cyanImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "cyanImage") (retPtr retVoid) []

-- | @+ magentaImage@
magentaImage :: IO RawId
magentaImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "magentaImage") (retPtr retVoid) []

-- | @+ yellowImage@
yellowImage :: IO RawId
yellowImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "yellowImage") (retPtr retVoid) []

-- | @+ clearImage@
clearImage :: IO RawId
clearImage  =
  do
    cls' <- getRequiredClass "CIImage"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "clearImage") (retPtr retVoid) []

-- | Returns YES if the image is known to have and alpha value of @1.0@ over the entire image extent.
--
-- ObjC selector: @- opaque@
opaque :: IsCIImage ciImage => ciImage -> IO Bool
opaque ciImage  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciImage (mkSelector "opaque") retCULong []

-- | @- definition@
definition :: IsCIImage ciImage => ciImage -> IO RawId
definition ciImage  =
    fmap (RawId . castPtr) $ sendMsg ciImage (mkSelector "definition") (retPtr retVoid) []

-- | @- url@
url :: IsCIImage ciImage => ciImage -> IO RawId
url ciImage  =
    fmap (RawId . castPtr) $ sendMsg ciImage (mkSelector "url") (retPtr retVoid) []

-- | @- colorSpace@
colorSpace :: IsCIImage ciImage => ciImage -> IO (Ptr ())
colorSpace ciImage  =
    fmap castPtr $ sendMsg ciImage (mkSelector "colorSpace") (retPtr retVoid) []

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
contentHeadroom ciImage  =
    sendMsg ciImage (mkSelector "contentHeadroom") retCFloat []

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
contentAverageLightLevel ciImage  =
    sendMsg ciImage (mkSelector "contentAverageLightLevel") retCFloat []

-- | @- pixelBuffer@
pixelBuffer :: IsCIImage ciImage => ciImage -> IO (Ptr ())
pixelBuffer ciImage  =
    fmap castPtr $ sendMsg ciImage (mkSelector "pixelBuffer") (retPtr retVoid) []

-- | @- CGImage@
cgImage :: IsCIImage ciImage => ciImage -> IO (Ptr ())
cgImage ciImage  =
    fmap castPtr $ sendMsg ciImage (mkSelector "CGImage") (retPtr retVoid) []

-- | @- semanticSegmentationMatte@
semanticSegmentationMatte :: IsCIImage ciImage => ciImage -> IO RawId
semanticSegmentationMatte ciImage  =
    fmap (RawId . castPtr) $ sendMsg ciImage (mkSelector "semanticSegmentationMatte") (retPtr retVoid) []

-- | @- portraitEffectsMatte@
portraitEffectsMatte :: IsCIImage ciImage => ciImage -> IO RawId
portraitEffectsMatte ciImage  =
    fmap (RawId . castPtr) $ sendMsg ciImage (mkSelector "portraitEffectsMatte") (retPtr retVoid) []

-- | @- depthData@
depthData :: IsCIImage ciImage => ciImage -> IO RawId
depthData ciImage  =
    fmap (RawId . castPtr) $ sendMsg ciImage (mkSelector "depthData") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageWithCGImage:@
imageWithCGImageSelector :: Selector
imageWithCGImageSelector = mkSelector "imageWithCGImage:"

-- | @Selector@ for @imageWithCGImage:options:@
imageWithCGImage_optionsSelector :: Selector
imageWithCGImage_optionsSelector = mkSelector "imageWithCGImage:options:"

-- | @Selector@ for @imageWithCGImageSource:index:options:@
imageWithCGImageSource_index_optionsSelector :: Selector
imageWithCGImageSource_index_optionsSelector = mkSelector "imageWithCGImageSource:index:options:"

-- | @Selector@ for @imageWithCGLayer:@
imageWithCGLayerSelector :: Selector
imageWithCGLayerSelector = mkSelector "imageWithCGLayer:"

-- | @Selector@ for @imageWithCGLayer:options:@
imageWithCGLayer_optionsSelector :: Selector
imageWithCGLayer_optionsSelector = mkSelector "imageWithCGLayer:options:"

-- | @Selector@ for @imageWithMTLTexture:options:@
imageWithMTLTexture_optionsSelector :: Selector
imageWithMTLTexture_optionsSelector = mkSelector "imageWithMTLTexture:options:"

-- | @Selector@ for @imageWithContentsOfURL:@
imageWithContentsOfURLSelector :: Selector
imageWithContentsOfURLSelector = mkSelector "imageWithContentsOfURL:"

-- | @Selector@ for @imageWithContentsOfURL:options:@
imageWithContentsOfURL_optionsSelector :: Selector
imageWithContentsOfURL_optionsSelector = mkSelector "imageWithContentsOfURL:options:"

-- | @Selector@ for @imageWithData:@
imageWithDataSelector :: Selector
imageWithDataSelector = mkSelector "imageWithData:"

-- | @Selector@ for @imageWithData:options:@
imageWithData_optionsSelector :: Selector
imageWithData_optionsSelector = mkSelector "imageWithData:options:"

-- | @Selector@ for @imageWithCVImageBuffer:@
imageWithCVImageBufferSelector :: Selector
imageWithCVImageBufferSelector = mkSelector "imageWithCVImageBuffer:"

-- | @Selector@ for @imageWithCVImageBuffer:options:@
imageWithCVImageBuffer_optionsSelector :: Selector
imageWithCVImageBuffer_optionsSelector = mkSelector "imageWithCVImageBuffer:options:"

-- | @Selector@ for @imageWithCVPixelBuffer:@
imageWithCVPixelBufferSelector :: Selector
imageWithCVPixelBufferSelector = mkSelector "imageWithCVPixelBuffer:"

-- | @Selector@ for @imageWithCVPixelBuffer:options:@
imageWithCVPixelBuffer_optionsSelector :: Selector
imageWithCVPixelBuffer_optionsSelector = mkSelector "imageWithCVPixelBuffer:options:"

-- | @Selector@ for @imageWithIOSurface:@
imageWithIOSurfaceSelector :: Selector
imageWithIOSurfaceSelector = mkSelector "imageWithIOSurface:"

-- | @Selector@ for @imageWithIOSurface:options:@
imageWithIOSurface_optionsSelector :: Selector
imageWithIOSurface_optionsSelector = mkSelector "imageWithIOSurface:options:"

-- | @Selector@ for @imageWithColor:@
imageWithColorSelector :: Selector
imageWithColorSelector = mkSelector "imageWithColor:"

-- | @Selector@ for @emptyImage@
emptyImageSelector :: Selector
emptyImageSelector = mkSelector "emptyImage"

-- | @Selector@ for @initWithCGImage:@
initWithCGImageSelector :: Selector
initWithCGImageSelector = mkSelector "initWithCGImage:"

-- | @Selector@ for @initWithCGImage:options:@
initWithCGImage_optionsSelector :: Selector
initWithCGImage_optionsSelector = mkSelector "initWithCGImage:options:"

-- | @Selector@ for @initWithCGImageSource:index:options:@
initWithCGImageSource_index_optionsSelector :: Selector
initWithCGImageSource_index_optionsSelector = mkSelector "initWithCGImageSource:index:options:"

-- | @Selector@ for @initWithCGLayer:@
initWithCGLayerSelector :: Selector
initWithCGLayerSelector = mkSelector "initWithCGLayer:"

-- | @Selector@ for @initWithCGLayer:options:@
initWithCGLayer_optionsSelector :: Selector
initWithCGLayer_optionsSelector = mkSelector "initWithCGLayer:options:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithData:options:@
initWithData_optionsSelector :: Selector
initWithData_optionsSelector = mkSelector "initWithData:options:"

-- | @Selector@ for @initWithMTLTexture:options:@
initWithMTLTexture_optionsSelector :: Selector
initWithMTLTexture_optionsSelector = mkSelector "initWithMTLTexture:options:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initWithContentsOfURL:options:@
initWithContentsOfURL_optionsSelector :: Selector
initWithContentsOfURL_optionsSelector = mkSelector "initWithContentsOfURL:options:"

-- | @Selector@ for @initWithIOSurface:@
initWithIOSurfaceSelector :: Selector
initWithIOSurfaceSelector = mkSelector "initWithIOSurface:"

-- | @Selector@ for @initWithIOSurface:options:@
initWithIOSurface_optionsSelector :: Selector
initWithIOSurface_optionsSelector = mkSelector "initWithIOSurface:options:"

-- | @Selector@ for @initWithIOSurface:plane:format:options:@
initWithIOSurface_plane_format_optionsSelector :: Selector
initWithIOSurface_plane_format_optionsSelector = mkSelector "initWithIOSurface:plane:format:options:"

-- | @Selector@ for @initWithCVImageBuffer:@
initWithCVImageBufferSelector :: Selector
initWithCVImageBufferSelector = mkSelector "initWithCVImageBuffer:"

-- | @Selector@ for @initWithCVImageBuffer:options:@
initWithCVImageBuffer_optionsSelector :: Selector
initWithCVImageBuffer_optionsSelector = mkSelector "initWithCVImageBuffer:options:"

-- | @Selector@ for @initWithCVPixelBuffer:@
initWithCVPixelBufferSelector :: Selector
initWithCVPixelBufferSelector = mkSelector "initWithCVPixelBuffer:"

-- | @Selector@ for @initWithCVPixelBuffer:options:@
initWithCVPixelBuffer_optionsSelector :: Selector
initWithCVPixelBuffer_optionsSelector = mkSelector "initWithCVPixelBuffer:options:"

-- | @Selector@ for @initWithColor:@
initWithColorSelector :: Selector
initWithColorSelector = mkSelector "initWithColor:"

-- | @Selector@ for @imageByApplyingOrientation:@
imageByApplyingOrientationSelector :: Selector
imageByApplyingOrientationSelector = mkSelector "imageByApplyingOrientation:"

-- | @Selector@ for @imageByApplyingCGOrientation:@
imageByApplyingCGOrientationSelector :: Selector
imageByApplyingCGOrientationSelector = mkSelector "imageByApplyingCGOrientation:"

-- | @Selector@ for @imageByCompositingOverImage:@
imageByCompositingOverImageSelector :: Selector
imageByCompositingOverImageSelector = mkSelector "imageByCompositingOverImage:"

-- | @Selector@ for @imageByClampingToExtent@
imageByClampingToExtentSelector :: Selector
imageByClampingToExtentSelector = mkSelector "imageByClampingToExtent"

-- | @Selector@ for @imageByApplyingFilter:withInputParameters:@
imageByApplyingFilter_withInputParametersSelector :: Selector
imageByApplyingFilter_withInputParametersSelector = mkSelector "imageByApplyingFilter:withInputParameters:"

-- | @Selector@ for @imageByApplyingFilter:@
imageByApplyingFilterSelector :: Selector
imageByApplyingFilterSelector = mkSelector "imageByApplyingFilter:"

-- | @Selector@ for @imageByColorMatchingColorSpaceToWorkingSpace:@
imageByColorMatchingColorSpaceToWorkingSpaceSelector :: Selector
imageByColorMatchingColorSpaceToWorkingSpaceSelector = mkSelector "imageByColorMatchingColorSpaceToWorkingSpace:"

-- | @Selector@ for @imageByColorMatchingWorkingSpaceToColorSpace:@
imageByColorMatchingWorkingSpaceToColorSpaceSelector :: Selector
imageByColorMatchingWorkingSpaceToColorSpaceSelector = mkSelector "imageByColorMatchingWorkingSpaceToColorSpace:"

-- | @Selector@ for @imageByPremultiplyingAlpha@
imageByPremultiplyingAlphaSelector :: Selector
imageByPremultiplyingAlphaSelector = mkSelector "imageByPremultiplyingAlpha"

-- | @Selector@ for @imageByUnpremultiplyingAlpha@
imageByUnpremultiplyingAlphaSelector :: Selector
imageByUnpremultiplyingAlphaSelector = mkSelector "imageByUnpremultiplyingAlpha"

-- | @Selector@ for @imageByApplyingGaussianBlurWithSigma:@
imageByApplyingGaussianBlurWithSigmaSelector :: Selector
imageByApplyingGaussianBlurWithSigmaSelector = mkSelector "imageByApplyingGaussianBlurWithSigma:"

-- | @Selector@ for @imageBySettingProperties:@
imageBySettingPropertiesSelector :: Selector
imageBySettingPropertiesSelector = mkSelector "imageBySettingProperties:"

-- | @Selector@ for @imageBySamplingLinear@
imageBySamplingLinearSelector :: Selector
imageBySamplingLinearSelector = mkSelector "imageBySamplingLinear"

-- | @Selector@ for @imageBySamplingNearest@
imageBySamplingNearestSelector :: Selector
imageBySamplingNearestSelector = mkSelector "imageBySamplingNearest"

-- | @Selector@ for @imageByInsertingIntermediate@
imageByInsertingIntermediateSelector :: Selector
imageByInsertingIntermediateSelector = mkSelector "imageByInsertingIntermediate"

-- | @Selector@ for @imageByInsertingTiledIntermediate@
imageByInsertingTiledIntermediateSelector :: Selector
imageByInsertingTiledIntermediateSelector = mkSelector "imageByInsertingTiledIntermediate"

-- | @Selector@ for @imageByApplyingGainMap:@
imageByApplyingGainMapSelector :: Selector
imageByApplyingGainMapSelector = mkSelector "imageByApplyingGainMap:"

-- | @Selector@ for @imageByApplyingGainMap:headroom:@
imageByApplyingGainMap_headroomSelector :: Selector
imageByApplyingGainMap_headroomSelector = mkSelector "imageByApplyingGainMap:headroom:"

-- | @Selector@ for @imageBySettingContentHeadroom:@
imageBySettingContentHeadroomSelector :: Selector
imageBySettingContentHeadroomSelector = mkSelector "imageBySettingContentHeadroom:"

-- | @Selector@ for @imageBySettingContentAverageLightLevel:@
imageBySettingContentAverageLightLevelSelector :: Selector
imageBySettingContentAverageLightLevelSelector = mkSelector "imageBySettingContentAverageLightLevel:"

-- | @Selector@ for @imageWithImageProvider:size::format:colorSpace:options:@
imageWithImageProvider_size__format_colorSpace_optionsSelector :: Selector
imageWithImageProvider_size__format_colorSpace_optionsSelector = mkSelector "imageWithImageProvider:size::format:colorSpace:options:"

-- | @Selector@ for @initWithImageProvider:size::format:colorSpace:options:@
initWithImageProvider_size__format_colorSpace_optionsSelector :: Selector
initWithImageProvider_size__format_colorSpace_optionsSelector = mkSelector "initWithImageProvider:size::format:colorSpace:options:"

-- | @Selector@ for @initWithSemanticSegmentationMatte:options:@
initWithSemanticSegmentationMatte_optionsSelector :: Selector
initWithSemanticSegmentationMatte_optionsSelector = mkSelector "initWithSemanticSegmentationMatte:options:"

-- | @Selector@ for @initWithSemanticSegmentationMatte:@
initWithSemanticSegmentationMatteSelector :: Selector
initWithSemanticSegmentationMatteSelector = mkSelector "initWithSemanticSegmentationMatte:"

-- | @Selector@ for @imageWithSemanticSegmentationMatte:options:@
imageWithSemanticSegmentationMatte_optionsSelector :: Selector
imageWithSemanticSegmentationMatte_optionsSelector = mkSelector "imageWithSemanticSegmentationMatte:options:"

-- | @Selector@ for @imageWithSemanticSegmentationMatte:@
imageWithSemanticSegmentationMatteSelector :: Selector
imageWithSemanticSegmentationMatteSelector = mkSelector "imageWithSemanticSegmentationMatte:"

-- | @Selector@ for @initWithPortaitEffectsMatte:options:@
initWithPortaitEffectsMatte_optionsSelector :: Selector
initWithPortaitEffectsMatte_optionsSelector = mkSelector "initWithPortaitEffectsMatte:options:"

-- | @Selector@ for @initWithPortaitEffectsMatte:@
initWithPortaitEffectsMatteSelector :: Selector
initWithPortaitEffectsMatteSelector = mkSelector "initWithPortaitEffectsMatte:"

-- | @Selector@ for @imageWithPortaitEffectsMatte:options:@
imageWithPortaitEffectsMatte_optionsSelector :: Selector
imageWithPortaitEffectsMatte_optionsSelector = mkSelector "imageWithPortaitEffectsMatte:options:"

-- | @Selector@ for @imageWithPortaitEffectsMatte:@
imageWithPortaitEffectsMatteSelector :: Selector
imageWithPortaitEffectsMatteSelector = mkSelector "imageWithPortaitEffectsMatte:"

-- | @Selector@ for @initWithDepthData:options:@
initWithDepthData_optionsSelector :: Selector
initWithDepthData_optionsSelector = mkSelector "initWithDepthData:options:"

-- | @Selector@ for @initWithDepthData:@
initWithDepthDataSelector :: Selector
initWithDepthDataSelector = mkSelector "initWithDepthData:"

-- | @Selector@ for @imageWithDepthData:options:@
imageWithDepthData_optionsSelector :: Selector
imageWithDepthData_optionsSelector = mkSelector "imageWithDepthData:options:"

-- | @Selector@ for @imageWithDepthData:@
imageWithDepthDataSelector :: Selector
imageWithDepthDataSelector = mkSelector "imageWithDepthData:"

-- | @Selector@ for @imageByConvertingWorkingSpaceToLab@
imageByConvertingWorkingSpaceToLabSelector :: Selector
imageByConvertingWorkingSpaceToLabSelector = mkSelector "imageByConvertingWorkingSpaceToLab"

-- | @Selector@ for @imageByConvertingLabToWorkingSpace@
imageByConvertingLabToWorkingSpaceSelector :: Selector
imageByConvertingLabToWorkingSpaceSelector = mkSelector "imageByConvertingLabToWorkingSpace"

-- | @Selector@ for @autoAdjustmentFilters@
autoAdjustmentFiltersSelector :: Selector
autoAdjustmentFiltersSelector = mkSelector "autoAdjustmentFilters"

-- | @Selector@ for @autoAdjustmentFiltersWithOptions:@
autoAdjustmentFiltersWithOptionsSelector :: Selector
autoAdjustmentFiltersWithOptionsSelector = mkSelector "autoAdjustmentFiltersWithOptions:"

-- | @Selector@ for @blackImage@
blackImageSelector :: Selector
blackImageSelector = mkSelector "blackImage"

-- | @Selector@ for @whiteImage@
whiteImageSelector :: Selector
whiteImageSelector = mkSelector "whiteImage"

-- | @Selector@ for @grayImage@
grayImageSelector :: Selector
grayImageSelector = mkSelector "grayImage"

-- | @Selector@ for @redImage@
redImageSelector :: Selector
redImageSelector = mkSelector "redImage"

-- | @Selector@ for @greenImage@
greenImageSelector :: Selector
greenImageSelector = mkSelector "greenImage"

-- | @Selector@ for @blueImage@
blueImageSelector :: Selector
blueImageSelector = mkSelector "blueImage"

-- | @Selector@ for @cyanImage@
cyanImageSelector :: Selector
cyanImageSelector = mkSelector "cyanImage"

-- | @Selector@ for @magentaImage@
magentaImageSelector :: Selector
magentaImageSelector = mkSelector "magentaImage"

-- | @Selector@ for @yellowImage@
yellowImageSelector :: Selector
yellowImageSelector = mkSelector "yellowImage"

-- | @Selector@ for @clearImage@
clearImageSelector :: Selector
clearImageSelector = mkSelector "clearImage"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @definition@
definitionSelector :: Selector
definitionSelector = mkSelector "definition"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @contentHeadroom@
contentHeadroomSelector :: Selector
contentHeadroomSelector = mkSelector "contentHeadroom"

-- | @Selector@ for @contentAverageLightLevel@
contentAverageLightLevelSelector :: Selector
contentAverageLightLevelSelector = mkSelector "contentAverageLightLevel"

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector
pixelBufferSelector = mkSelector "pixelBuffer"

-- | @Selector@ for @CGImage@
cgImageSelector :: Selector
cgImageSelector = mkSelector "CGImage"

-- | @Selector@ for @semanticSegmentationMatte@
semanticSegmentationMatteSelector :: Selector
semanticSegmentationMatteSelector = mkSelector "semanticSegmentationMatte"

-- | @Selector@ for @portraitEffectsMatte@
portraitEffectsMatteSelector :: Selector
portraitEffectsMatteSelector = mkSelector "portraitEffectsMatte"

-- | @Selector@ for @depthData@
depthDataSelector :: Selector
depthDataSelector = mkSelector "depthData"

