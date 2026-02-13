{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The Core Image context class provides an evaluation context for Core Image processing with Metal, OpenGL, or OpenCL.
--
-- You use a @CIContext@ instance to render a ``CIImage`` instance which represents a graph of image processing operations which are built using other Core Image classes, such as ``CIFilter-class``, ``CIKernel``, ``CIColor`` and ``CIImage``.  You can also use a @CIContext@ with the ``CIDetector`` class to analyze images — for example, to detect faces  or barcodes.
--
-- Contexts support automatic color management by performing all processing operations in a working color space. This means that unless told otherwise: * All input images are color matched from the input's color space to the working space. * All renders are color matched from the working space to the destination space. (For more information on @CGColorSpace@ see <doc://com.apple.documentation/documentation/coregraphics/cgcolorspace>)
--
-- @CIContext@ and ``CIImage`` instances are immutable, so multiple threads can use the same ``CIContext`` instance  to render ``CIImage`` instances. However, ``CIFilter-class`` instances are mutable and thus cannot be shared safely among  threads. Each thread must take case not to access or modify a ``CIFilter-class`` instance while it is being used by  another thread.
--
-- The @CIContext@ manages various internal state such as @MTLCommandQueue@ and caches for compiled kernels and intermediate buffers.  For this reason it is not recommended to create many @CIContext@ instances.  As a rule, it recommended that you create one @CIContext@ instance for each view that renders ``CIImage`` or each background task.
--
-- Generated bindings for @CIContext@.
module ObjC.CoreImage.CIContext
  ( CIContext
  , IsCIContext(..)
  , contextWithCGLContext_pixelFormat_colorSpace_options
  , contextWithCGLContext_pixelFormat_options
  , contextWithCGContext_options
  , contextWithOptions
  , context
  , initWithOptions
  , init_
  , contextWithMTLDevice
  , contextWithMTLDevice_options
  , contextWithMTLCommandQueue
  , contextWithMTLCommandQueue_options
  , render_toCVPixelBuffer
  , reclaimResources
  , clearCaches
  , startTaskToRender_toDestination_error
  , startTaskToClear_error
  , depthBlurEffectFilterForImageURL_options
  , depthBlurEffectFilterForImageData_options
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_options
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_options
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_options
  , tiffRepresentationOfImage_format_colorSpace_options
  , jpegRepresentationOfImage_colorSpace_options
  , heifRepresentationOfImage_format_colorSpace_options
  , heiF10RepresentationOfImage_colorSpace_options_error
  , pngRepresentationOfImage_format_colorSpace_options
  , openEXRRepresentationOfImage_options_error
  , writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_error
  , writePNGRepresentationOfImage_toURL_format_colorSpace_options_error
  , writeJPEGRepresentationOfImage_toURL_colorSpace_options_error
  , writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_error
  , writeHEIF10RepresentationOfImage_toURL_colorSpace_options_error
  , writeOpenEXRRepresentationOfImage_toURL_options_error
  , offlineGPUCount
  , contextForOfflineGPUAtIndex
  , contextForOfflineGPUAtIndex_colorSpace_options_sharedContext
  , calculateHDRStatsForIOSurface
  , calculateHDRStatsForCVPixelBuffer
  , calculateHDRStatsForCGImage
  , calculateHDRStatsForImage
  , workingColorSpace
  , workingFormat
  , calculateHDRStatsForCGImageSelector
  , calculateHDRStatsForCVPixelBufferSelector
  , calculateHDRStatsForIOSurfaceSelector
  , calculateHDRStatsForImageSelector
  , clearCachesSelector
  , contextForOfflineGPUAtIndexSelector
  , contextForOfflineGPUAtIndex_colorSpace_options_sharedContextSelector
  , contextSelector
  , contextWithCGContext_optionsSelector
  , contextWithCGLContext_pixelFormat_colorSpace_optionsSelector
  , contextWithCGLContext_pixelFormat_optionsSelector
  , contextWithMTLCommandQueueSelector
  , contextWithMTLCommandQueue_optionsSelector
  , contextWithMTLDeviceSelector
  , contextWithMTLDevice_optionsSelector
  , contextWithOptionsSelector
  , depthBlurEffectFilterForImageData_optionsSelector
  , depthBlurEffectFilterForImageURL_optionsSelector
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_optionsSelector
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_optionsSelector
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_optionsSelector
  , heiF10RepresentationOfImage_colorSpace_options_errorSelector
  , heifRepresentationOfImage_format_colorSpace_optionsSelector
  , initSelector
  , initWithOptionsSelector
  , jpegRepresentationOfImage_colorSpace_optionsSelector
  , offlineGPUCountSelector
  , openEXRRepresentationOfImage_options_errorSelector
  , pngRepresentationOfImage_format_colorSpace_optionsSelector
  , reclaimResourcesSelector
  , render_toCVPixelBufferSelector
  , startTaskToClear_errorSelector
  , startTaskToRender_toDestination_errorSelector
  , tiffRepresentationOfImage_format_colorSpace_optionsSelector
  , workingColorSpaceSelector
  , workingFormatSelector
  , writeHEIF10RepresentationOfImage_toURL_colorSpace_options_errorSelector
  , writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector
  , writeJPEGRepresentationOfImage_toURL_colorSpace_options_errorSelector
  , writeOpenEXRRepresentationOfImage_toURL_options_errorSelector
  , writePNGRepresentationOfImage_toURL_format_colorSpace_options_errorSelector
  , writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ contextWithCGLContext:pixelFormat:colorSpace:options:@
contextWithCGLContext_pixelFormat_colorSpace_options :: IsNSDictionary options => Ptr () -> Ptr () -> Ptr () -> options -> IO (Id CIContext)
contextWithCGLContext_pixelFormat_colorSpace_options cglctx pixelFormat colorSpace options =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithCGLContext_pixelFormat_colorSpace_optionsSelector cglctx pixelFormat colorSpace (toNSDictionary options)

-- | @+ contextWithCGLContext:pixelFormat:options:@
contextWithCGLContext_pixelFormat_options :: IsNSDictionary options => Ptr () -> Ptr () -> options -> IO (Id CIContext)
contextWithCGLContext_pixelFormat_options cglctx pixelFormat options =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithCGLContext_pixelFormat_optionsSelector cglctx pixelFormat (toNSDictionary options)

-- | @+ contextWithCGContext:options:@
contextWithCGContext_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIContext)
contextWithCGContext_options cgctx options =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithCGContext_optionsSelector cgctx (toNSDictionary options)

-- | @+ contextWithOptions:@
contextWithOptions :: IsNSDictionary options => options -> IO (Id CIContext)
contextWithOptions options =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithOptionsSelector (toNSDictionary options)

-- | @+ context@
context :: IO (Id CIContext)
context  =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextSelector

-- | @- initWithOptions:@
initWithOptions :: (IsCIContext ciContext, IsNSDictionary options) => ciContext -> options -> IO (Id CIContext)
initWithOptions ciContext options =
  sendOwnedMessage ciContext initWithOptionsSelector (toNSDictionary options)

-- | @- init@
init_ :: IsCIContext ciContext => ciContext -> IO (Id CIContext)
init_ ciContext =
  sendOwnedMessage ciContext initSelector

-- | @+ contextWithMTLDevice:@
contextWithMTLDevice :: RawId -> IO (Id CIContext)
contextWithMTLDevice device =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithMTLDeviceSelector device

-- | @+ contextWithMTLDevice:options:@
contextWithMTLDevice_options :: IsNSDictionary options => RawId -> options -> IO (Id CIContext)
contextWithMTLDevice_options device options =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithMTLDevice_optionsSelector device (toNSDictionary options)

-- | @+ contextWithMTLCommandQueue:@
contextWithMTLCommandQueue :: RawId -> IO (Id CIContext)
contextWithMTLCommandQueue commandQueue =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithMTLCommandQueueSelector commandQueue

-- | @+ contextWithMTLCommandQueue:options:@
contextWithMTLCommandQueue_options :: IsNSDictionary options => RawId -> options -> IO (Id CIContext)
contextWithMTLCommandQueue_options commandQueue options =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextWithMTLCommandQueue_optionsSelector commandQueue (toNSDictionary options)

-- | @- render:toCVPixelBuffer:@
render_toCVPixelBuffer :: (IsCIContext ciContext, IsCIImage image) => ciContext -> image -> Ptr () -> IO ()
render_toCVPixelBuffer ciContext image buffer =
  sendMessage ciContext render_toCVPixelBufferSelector (toCIImage image) buffer

-- | @- reclaimResources@
reclaimResources :: IsCIContext ciContext => ciContext -> IO ()
reclaimResources ciContext =
  sendMessage ciContext reclaimResourcesSelector

-- | @- clearCaches@
clearCaches :: IsCIContext ciContext => ciContext -> IO ()
clearCaches ciContext =
  sendMessage ciContext clearCachesSelector

-- | @- startTaskToRender:toDestination:error:@
startTaskToRender_toDestination_error :: (IsCIContext ciContext, IsCIImage image, IsCIRenderDestination destination, IsNSError error_) => ciContext -> image -> destination -> error_ -> IO (Id CIRenderTask)
startTaskToRender_toDestination_error ciContext image destination error_ =
  sendMessage ciContext startTaskToRender_toDestination_errorSelector (toCIImage image) (toCIRenderDestination destination) (toNSError error_)

-- | @- startTaskToClear:error:@
startTaskToClear_error :: (IsCIContext ciContext, IsCIRenderDestination destination, IsNSError error_) => ciContext -> destination -> error_ -> IO (Id CIRenderTask)
startTaskToClear_error ciContext destination error_ =
  sendMessage ciContext startTaskToClear_errorSelector (toCIRenderDestination destination) (toNSError error_)

-- | @- depthBlurEffectFilterForImageURL:options:@
depthBlurEffectFilterForImageURL_options :: (IsCIContext ciContext, IsNSURL url, IsNSDictionary options) => ciContext -> url -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImageURL_options ciContext url options =
  sendMessage ciContext depthBlurEffectFilterForImageURL_optionsSelector (toNSURL url) (toNSDictionary options)

-- | @- depthBlurEffectFilterForImageData:options:@
depthBlurEffectFilterForImageData_options :: (IsCIContext ciContext, IsNSData data_, IsNSDictionary options) => ciContext -> data_ -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImageData_options ciContext data_ options =
  sendMessage ciContext depthBlurEffectFilterForImageData_optionsSelector (toNSData data_) (toNSDictionary options)

-- | @- depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_options :: (IsCIContext ciContext, IsCIImage image, IsCIImage disparityImage, IsCIImage portraitEffectsMatte, IsNSDictionary options) => ciContext -> image -> disparityImage -> portraitEffectsMatte -> CInt -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_options ciContext image disparityImage portraitEffectsMatte orientation options =
  sendMessage ciContext depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_optionsSelector (toCIImage image) (toCIImage disparityImage) (toCIImage portraitEffectsMatte) orientation (toNSDictionary options)

-- | @- depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_options :: (IsCIContext ciContext, IsCIImage image, IsCIImage disparityImage, IsCIImage portraitEffectsMatte, IsCIImage hairSemanticSegmentation, IsNSDictionary options) => ciContext -> image -> disparityImage -> portraitEffectsMatte -> hairSemanticSegmentation -> CInt -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_options ciContext image disparityImage portraitEffectsMatte hairSemanticSegmentation orientation options =
  sendMessage ciContext depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_optionsSelector (toCIImage image) (toCIImage disparityImage) (toCIImage portraitEffectsMatte) (toCIImage hairSemanticSegmentation) orientation (toNSDictionary options)

-- | @- depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:glassesMatte:gainMap:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_options :: (IsCIContext ciContext, IsCIImage image, IsCIImage disparityImage, IsCIImage portraitEffectsMatte, IsCIImage hairSemanticSegmentation, IsCIImage glassesMatte, IsCIImage gainMap, IsNSDictionary options) => ciContext -> image -> disparityImage -> portraitEffectsMatte -> hairSemanticSegmentation -> glassesMatte -> gainMap -> CInt -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_options ciContext image disparityImage portraitEffectsMatte hairSemanticSegmentation glassesMatte gainMap orientation options =
  sendMessage ciContext depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_optionsSelector (toCIImage image) (toCIImage disparityImage) (toCIImage portraitEffectsMatte) (toCIImage hairSemanticSegmentation) (toCIImage glassesMatte) (toCIImage gainMap) orientation (toNSDictionary options)

-- | @- TIFFRepresentationOfImage:format:colorSpace:options:@
tiffRepresentationOfImage_format_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> CInt -> Ptr () -> options -> IO (Id NSData)
tiffRepresentationOfImage_format_colorSpace_options ciContext image format colorSpace options =
  sendMessage ciContext tiffRepresentationOfImage_format_colorSpace_optionsSelector (toCIImage image) format colorSpace (toNSDictionary options)

-- | @- JPEGRepresentationOfImage:colorSpace:options:@
jpegRepresentationOfImage_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> Ptr () -> options -> IO (Id NSData)
jpegRepresentationOfImage_colorSpace_options ciContext image colorSpace options =
  sendMessage ciContext jpegRepresentationOfImage_colorSpace_optionsSelector (toCIImage image) colorSpace (toNSDictionary options)

-- | @- HEIFRepresentationOfImage:format:colorSpace:options:@
heifRepresentationOfImage_format_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> CInt -> Ptr () -> options -> IO (Id NSData)
heifRepresentationOfImage_format_colorSpace_options ciContext image format colorSpace options =
  sendMessage ciContext heifRepresentationOfImage_format_colorSpace_optionsSelector (toCIImage image) format colorSpace (toNSDictionary options)

-- | @- HEIF10RepresentationOfImage:colorSpace:options:error:@
heiF10RepresentationOfImage_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> Ptr () -> options -> errorPtr -> IO (Id NSData)
heiF10RepresentationOfImage_colorSpace_options_error ciContext image colorSpace options errorPtr =
  sendMessage ciContext heiF10RepresentationOfImage_colorSpace_options_errorSelector (toCIImage image) colorSpace (toNSDictionary options) (toNSError errorPtr)

-- | @- PNGRepresentationOfImage:format:colorSpace:options:@
pngRepresentationOfImage_format_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> CInt -> Ptr () -> options -> IO (Id NSData)
pngRepresentationOfImage_format_colorSpace_options ciContext image format colorSpace options =
  sendMessage ciContext pngRepresentationOfImage_format_colorSpace_optionsSelector (toCIImage image) format colorSpace (toNSDictionary options)

-- | @- OpenEXRRepresentationOfImage:options:error:@
openEXRRepresentationOfImage_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> options -> errorPtr -> IO (Id NSData)
openEXRRepresentationOfImage_options_error ciContext image options errorPtr =
  sendMessage ciContext openEXRRepresentationOfImage_options_errorSelector (toCIImage image) (toNSDictionary options) (toNSError errorPtr)

-- | @- writeTIFFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> CInt -> Ptr () -> options -> errorPtr -> IO Bool
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_error ciContext image url format colorSpace options errorPtr =
  sendMessage ciContext writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector (toCIImage image) (toNSURL url) format colorSpace (toNSDictionary options) (toNSError errorPtr)

-- | @- writePNGRepresentationOfImage:toURL:format:colorSpace:options:error:@
writePNGRepresentationOfImage_toURL_format_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> CInt -> Ptr () -> options -> errorPtr -> IO Bool
writePNGRepresentationOfImage_toURL_format_colorSpace_options_error ciContext image url format colorSpace options errorPtr =
  sendMessage ciContext writePNGRepresentationOfImage_toURL_format_colorSpace_options_errorSelector (toCIImage image) (toNSURL url) format colorSpace (toNSDictionary options) (toNSError errorPtr)

-- | @- writeJPEGRepresentationOfImage:toURL:colorSpace:options:error:@
writeJPEGRepresentationOfImage_toURL_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> Ptr () -> options -> errorPtr -> IO Bool
writeJPEGRepresentationOfImage_toURL_colorSpace_options_error ciContext image url colorSpace options errorPtr =
  sendMessage ciContext writeJPEGRepresentationOfImage_toURL_colorSpace_options_errorSelector (toCIImage image) (toNSURL url) colorSpace (toNSDictionary options) (toNSError errorPtr)

-- | @- writeHEIFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> CInt -> Ptr () -> options -> errorPtr -> IO Bool
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_error ciContext image url format colorSpace options errorPtr =
  sendMessage ciContext writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector (toCIImage image) (toNSURL url) format colorSpace (toNSDictionary options) (toNSError errorPtr)

-- | @- writeHEIF10RepresentationOfImage:toURL:colorSpace:options:error:@
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> Ptr () -> options -> errorPtr -> IO Bool
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_error ciContext image url colorSpace options errorPtr =
  sendMessage ciContext writeHEIF10RepresentationOfImage_toURL_colorSpace_options_errorSelector (toCIImage image) (toNSURL url) colorSpace (toNSDictionary options) (toNSError errorPtr)

-- | @- writeOpenEXRRepresentationOfImage:toURL:options:error:@
writeOpenEXRRepresentationOfImage_toURL_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> options -> errorPtr -> IO Bool
writeOpenEXRRepresentationOfImage_toURL_options_error ciContext image url options errorPtr =
  sendMessage ciContext writeOpenEXRRepresentationOfImage_toURL_options_errorSelector (toCIImage image) (toNSURL url) (toNSDictionary options) (toNSError errorPtr)

-- | @+ offlineGPUCount@
offlineGPUCount :: IO CUInt
offlineGPUCount  =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' offlineGPUCountSelector

-- | @+ contextForOfflineGPUAtIndex:@
contextForOfflineGPUAtIndex :: CUInt -> IO (Id CIContext)
contextForOfflineGPUAtIndex index =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextForOfflineGPUAtIndexSelector index

-- | @+ contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:@
contextForOfflineGPUAtIndex_colorSpace_options_sharedContext :: IsNSDictionary options => CUInt -> Ptr () -> options -> Ptr () -> IO (Id CIContext)
contextForOfflineGPUAtIndex_colorSpace_options_sharedContext index colorSpace options sharedContext =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMessage cls' contextForOfflineGPUAtIndex_colorSpace_options_sharedContextSelector index colorSpace (toNSDictionary options) sharedContext

-- | Given an IOSurface, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then update the surface's attachments to store the values.
--
-- If the @IOSurface@ has a Clean Aperture rectangle then only pixels within that rectangle are considered.
--
-- - Parameters:    - surface: A mutable @IOSurfaceRef@ for which to calculate and attach statistics.
--
-- ObjC selector: @- calculateHDRStatsForIOSurface:@
calculateHDRStatsForIOSurface :: IsCIContext ciContext => ciContext -> Ptr () -> IO ()
calculateHDRStatsForIOSurface ciContext surface =
  sendMessage ciContext calculateHDRStatsForIOSurfaceSelector surface

-- | Given a CVPixelBuffer, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then update the buffers's attachments to store the values.
--
-- If the @CVPixelBuffer@ has a Clean Aperture rectangle then only pixels within that rectangle are considered.
--
-- - Parameters:    - buffer: A mutable @CVPixelBuffer@ for which to calculate and attach statistics.
--
-- ObjC selector: @- calculateHDRStatsForCVPixelBuffer:@
calculateHDRStatsForCVPixelBuffer :: IsCIContext ciContext => ciContext -> Ptr () -> IO ()
calculateHDRStatsForCVPixelBuffer ciContext buffer =
  sendMessage ciContext calculateHDRStatsForCVPixelBufferSelector buffer

-- | Given a Core Graphics image, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then return a new Core Graphics image that has the calculated values.
--
-- - Parameters:    - cgimage: An immutable @CGImage@ for which to calculate statistics. - Returns:    Returns a new @CGImage@ instance that has the calculated statistics attached.
--
-- ObjC selector: @- calculateHDRStatsForCGImage:@
calculateHDRStatsForCGImage :: IsCIContext ciContext => ciContext -> Ptr () -> IO (Ptr ())
calculateHDRStatsForCGImage ciContext cgimage =
  sendMessage ciContext calculateHDRStatsForCGImageSelector cgimage

-- | Given a Core Image image, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then return a new Core Image image that has the calculated values.
--
-- If the image extent is not finite, then nil will be returned.
--
-- - Parameters:    - image: An immutable ``CIImage`` for which to calculate statistics. - Returns:    Returns a new ``CIImage`` instance that has the calculated statistics attached.
--
-- ObjC selector: @- calculateHDRStatsForImage:@
calculateHDRStatsForImage :: (IsCIContext ciContext, IsCIImage image) => ciContext -> image -> IO (Id CIImage)
calculateHDRStatsForImage ciContext image =
  sendMessage ciContext calculateHDRStatsForImageSelector (toCIImage image)

-- | The working color space of the CIContext.
--
-- The working color space determines the color space used when executing filter kernels.  You specify a working color space using the ``kCIContextWorkingColorSpace`` option when creating a ``CIContext``. * All input images are color matched from the input's color space to the working space. * All renders are color matched from the working space to the destination space.
--
-- The property will be @null@ if the context was created with color management disabled.
--
-- ObjC selector: @- workingColorSpace@
workingColorSpace :: IsCIContext ciContext => ciContext -> IO (Ptr ())
workingColorSpace ciContext =
  sendMessage ciContext workingColorSpaceSelector

-- | The working pixel format that the CIContext uses for intermediate buffers.
--
-- The working format determines the pixel format that Core Image uses to create intermediate buffers for rendering images.  You specify a working pixel format using the ``kCIContextWorkingFormat`` option when creating a ``CIContext``.
--
-- ObjC selector: @- workingFormat@
workingFormat :: IsCIContext ciContext => ciContext -> IO CInt
workingFormat ciContext =
  sendMessage ciContext workingFormatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contextWithCGLContext:pixelFormat:colorSpace:options:@
contextWithCGLContext_pixelFormat_colorSpace_optionsSelector :: Selector '[Ptr (), Ptr (), Ptr (), Id NSDictionary] (Id CIContext)
contextWithCGLContext_pixelFormat_colorSpace_optionsSelector = mkSelector "contextWithCGLContext:pixelFormat:colorSpace:options:"

-- | @Selector@ for @contextWithCGLContext:pixelFormat:options:@
contextWithCGLContext_pixelFormat_optionsSelector :: Selector '[Ptr (), Ptr (), Id NSDictionary] (Id CIContext)
contextWithCGLContext_pixelFormat_optionsSelector = mkSelector "contextWithCGLContext:pixelFormat:options:"

-- | @Selector@ for @contextWithCGContext:options:@
contextWithCGContext_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CIContext)
contextWithCGContext_optionsSelector = mkSelector "contextWithCGContext:options:"

-- | @Selector@ for @contextWithOptions:@
contextWithOptionsSelector :: Selector '[Id NSDictionary] (Id CIContext)
contextWithOptionsSelector = mkSelector "contextWithOptions:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id CIContext)
contextSelector = mkSelector "context"

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector '[Id NSDictionary] (Id CIContext)
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CIContext)
initSelector = mkSelector "init"

-- | @Selector@ for @contextWithMTLDevice:@
contextWithMTLDeviceSelector :: Selector '[RawId] (Id CIContext)
contextWithMTLDeviceSelector = mkSelector "contextWithMTLDevice:"

-- | @Selector@ for @contextWithMTLDevice:options:@
contextWithMTLDevice_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIContext)
contextWithMTLDevice_optionsSelector = mkSelector "contextWithMTLDevice:options:"

-- | @Selector@ for @contextWithMTLCommandQueue:@
contextWithMTLCommandQueueSelector :: Selector '[RawId] (Id CIContext)
contextWithMTLCommandQueueSelector = mkSelector "contextWithMTLCommandQueue:"

-- | @Selector@ for @contextWithMTLCommandQueue:options:@
contextWithMTLCommandQueue_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CIContext)
contextWithMTLCommandQueue_optionsSelector = mkSelector "contextWithMTLCommandQueue:options:"

-- | @Selector@ for @render:toCVPixelBuffer:@
render_toCVPixelBufferSelector :: Selector '[Id CIImage, Ptr ()] ()
render_toCVPixelBufferSelector = mkSelector "render:toCVPixelBuffer:"

-- | @Selector@ for @reclaimResources@
reclaimResourcesSelector :: Selector '[] ()
reclaimResourcesSelector = mkSelector "reclaimResources"

-- | @Selector@ for @clearCaches@
clearCachesSelector :: Selector '[] ()
clearCachesSelector = mkSelector "clearCaches"

-- | @Selector@ for @startTaskToRender:toDestination:error:@
startTaskToRender_toDestination_errorSelector :: Selector '[Id CIImage, Id CIRenderDestination, Id NSError] (Id CIRenderTask)
startTaskToRender_toDestination_errorSelector = mkSelector "startTaskToRender:toDestination:error:"

-- | @Selector@ for @startTaskToClear:error:@
startTaskToClear_errorSelector :: Selector '[Id CIRenderDestination, Id NSError] (Id CIRenderTask)
startTaskToClear_errorSelector = mkSelector "startTaskToClear:error:"

-- | @Selector@ for @depthBlurEffectFilterForImageURL:options:@
depthBlurEffectFilterForImageURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id CIFilter)
depthBlurEffectFilterForImageURL_optionsSelector = mkSelector "depthBlurEffectFilterForImageURL:options:"

-- | @Selector@ for @depthBlurEffectFilterForImageData:options:@
depthBlurEffectFilterForImageData_optionsSelector :: Selector '[Id NSData, Id NSDictionary] (Id CIFilter)
depthBlurEffectFilterForImageData_optionsSelector = mkSelector "depthBlurEffectFilterForImageData:options:"

-- | @Selector@ for @depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_optionsSelector :: Selector '[Id CIImage, Id CIImage, Id CIImage, CInt, Id NSDictionary] (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_optionsSelector = mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:orientation:options:"

-- | @Selector@ for @depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_optionsSelector :: Selector '[Id CIImage, Id CIImage, Id CIImage, Id CIImage, CInt, Id NSDictionary] (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_optionsSelector = mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:orientation:options:"

-- | @Selector@ for @depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:glassesMatte:gainMap:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_optionsSelector :: Selector '[Id CIImage, Id CIImage, Id CIImage, Id CIImage, Id CIImage, Id CIImage, CInt, Id NSDictionary] (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_optionsSelector = mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:glassesMatte:gainMap:orientation:options:"

-- | @Selector@ for @TIFFRepresentationOfImage:format:colorSpace:options:@
tiffRepresentationOfImage_format_colorSpace_optionsSelector :: Selector '[Id CIImage, CInt, Ptr (), Id NSDictionary] (Id NSData)
tiffRepresentationOfImage_format_colorSpace_optionsSelector = mkSelector "TIFFRepresentationOfImage:format:colorSpace:options:"

-- | @Selector@ for @JPEGRepresentationOfImage:colorSpace:options:@
jpegRepresentationOfImage_colorSpace_optionsSelector :: Selector '[Id CIImage, Ptr (), Id NSDictionary] (Id NSData)
jpegRepresentationOfImage_colorSpace_optionsSelector = mkSelector "JPEGRepresentationOfImage:colorSpace:options:"

-- | @Selector@ for @HEIFRepresentationOfImage:format:colorSpace:options:@
heifRepresentationOfImage_format_colorSpace_optionsSelector :: Selector '[Id CIImage, CInt, Ptr (), Id NSDictionary] (Id NSData)
heifRepresentationOfImage_format_colorSpace_optionsSelector = mkSelector "HEIFRepresentationOfImage:format:colorSpace:options:"

-- | @Selector@ for @HEIF10RepresentationOfImage:colorSpace:options:error:@
heiF10RepresentationOfImage_colorSpace_options_errorSelector :: Selector '[Id CIImage, Ptr (), Id NSDictionary, Id NSError] (Id NSData)
heiF10RepresentationOfImage_colorSpace_options_errorSelector = mkSelector "HEIF10RepresentationOfImage:colorSpace:options:error:"

-- | @Selector@ for @PNGRepresentationOfImage:format:colorSpace:options:@
pngRepresentationOfImage_format_colorSpace_optionsSelector :: Selector '[Id CIImage, CInt, Ptr (), Id NSDictionary] (Id NSData)
pngRepresentationOfImage_format_colorSpace_optionsSelector = mkSelector "PNGRepresentationOfImage:format:colorSpace:options:"

-- | @Selector@ for @OpenEXRRepresentationOfImage:options:error:@
openEXRRepresentationOfImage_options_errorSelector :: Selector '[Id CIImage, Id NSDictionary, Id NSError] (Id NSData)
openEXRRepresentationOfImage_options_errorSelector = mkSelector "OpenEXRRepresentationOfImage:options:error:"

-- | @Selector@ for @writeTIFFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector :: Selector '[Id CIImage, Id NSURL, CInt, Ptr (), Id NSDictionary, Id NSError] Bool
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector = mkSelector "writeTIFFRepresentationOfImage:toURL:format:colorSpace:options:error:"

-- | @Selector@ for @writePNGRepresentationOfImage:toURL:format:colorSpace:options:error:@
writePNGRepresentationOfImage_toURL_format_colorSpace_options_errorSelector :: Selector '[Id CIImage, Id NSURL, CInt, Ptr (), Id NSDictionary, Id NSError] Bool
writePNGRepresentationOfImage_toURL_format_colorSpace_options_errorSelector = mkSelector "writePNGRepresentationOfImage:toURL:format:colorSpace:options:error:"

-- | @Selector@ for @writeJPEGRepresentationOfImage:toURL:colorSpace:options:error:@
writeJPEGRepresentationOfImage_toURL_colorSpace_options_errorSelector :: Selector '[Id CIImage, Id NSURL, Ptr (), Id NSDictionary, Id NSError] Bool
writeJPEGRepresentationOfImage_toURL_colorSpace_options_errorSelector = mkSelector "writeJPEGRepresentationOfImage:toURL:colorSpace:options:error:"

-- | @Selector@ for @writeHEIFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector :: Selector '[Id CIImage, Id NSURL, CInt, Ptr (), Id NSDictionary, Id NSError] Bool
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector = mkSelector "writeHEIFRepresentationOfImage:toURL:format:colorSpace:options:error:"

-- | @Selector@ for @writeHEIF10RepresentationOfImage:toURL:colorSpace:options:error:@
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_errorSelector :: Selector '[Id CIImage, Id NSURL, Ptr (), Id NSDictionary, Id NSError] Bool
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_errorSelector = mkSelector "writeHEIF10RepresentationOfImage:toURL:colorSpace:options:error:"

-- | @Selector@ for @writeOpenEXRRepresentationOfImage:toURL:options:error:@
writeOpenEXRRepresentationOfImage_toURL_options_errorSelector :: Selector '[Id CIImage, Id NSURL, Id NSDictionary, Id NSError] Bool
writeOpenEXRRepresentationOfImage_toURL_options_errorSelector = mkSelector "writeOpenEXRRepresentationOfImage:toURL:options:error:"

-- | @Selector@ for @offlineGPUCount@
offlineGPUCountSelector :: Selector '[] CUInt
offlineGPUCountSelector = mkSelector "offlineGPUCount"

-- | @Selector@ for @contextForOfflineGPUAtIndex:@
contextForOfflineGPUAtIndexSelector :: Selector '[CUInt] (Id CIContext)
contextForOfflineGPUAtIndexSelector = mkSelector "contextForOfflineGPUAtIndex:"

-- | @Selector@ for @contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:@
contextForOfflineGPUAtIndex_colorSpace_options_sharedContextSelector :: Selector '[CUInt, Ptr (), Id NSDictionary, Ptr ()] (Id CIContext)
contextForOfflineGPUAtIndex_colorSpace_options_sharedContextSelector = mkSelector "contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:"

-- | @Selector@ for @calculateHDRStatsForIOSurface:@
calculateHDRStatsForIOSurfaceSelector :: Selector '[Ptr ()] ()
calculateHDRStatsForIOSurfaceSelector = mkSelector "calculateHDRStatsForIOSurface:"

-- | @Selector@ for @calculateHDRStatsForCVPixelBuffer:@
calculateHDRStatsForCVPixelBufferSelector :: Selector '[Ptr ()] ()
calculateHDRStatsForCVPixelBufferSelector = mkSelector "calculateHDRStatsForCVPixelBuffer:"

-- | @Selector@ for @calculateHDRStatsForCGImage:@
calculateHDRStatsForCGImageSelector :: Selector '[Ptr ()] (Ptr ())
calculateHDRStatsForCGImageSelector = mkSelector "calculateHDRStatsForCGImage:"

-- | @Selector@ for @calculateHDRStatsForImage:@
calculateHDRStatsForImageSelector :: Selector '[Id CIImage] (Id CIImage)
calculateHDRStatsForImageSelector = mkSelector "calculateHDRStatsForImage:"

-- | @Selector@ for @workingColorSpace@
workingColorSpaceSelector :: Selector '[] (Ptr ())
workingColorSpaceSelector = mkSelector "workingColorSpace"

-- | @Selector@ for @workingFormat@
workingFormatSelector :: Selector '[] CInt
workingFormatSelector = mkSelector "workingFormat"

