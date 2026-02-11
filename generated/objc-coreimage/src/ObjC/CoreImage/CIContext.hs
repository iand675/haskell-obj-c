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
  , contextWithCGLContext_pixelFormat_colorSpace_optionsSelector
  , contextWithCGLContext_pixelFormat_optionsSelector
  , contextWithCGContext_optionsSelector
  , contextWithOptionsSelector
  , contextSelector
  , initWithOptionsSelector
  , initSelector
  , contextWithMTLDeviceSelector
  , contextWithMTLDevice_optionsSelector
  , contextWithMTLCommandQueueSelector
  , contextWithMTLCommandQueue_optionsSelector
  , render_toCVPixelBufferSelector
  , reclaimResourcesSelector
  , clearCachesSelector
  , startTaskToRender_toDestination_errorSelector
  , startTaskToClear_errorSelector
  , depthBlurEffectFilterForImageURL_optionsSelector
  , depthBlurEffectFilterForImageData_optionsSelector
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_optionsSelector
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_optionsSelector
  , depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_optionsSelector
  , tiffRepresentationOfImage_format_colorSpace_optionsSelector
  , jpegRepresentationOfImage_colorSpace_optionsSelector
  , heifRepresentationOfImage_format_colorSpace_optionsSelector
  , heiF10RepresentationOfImage_colorSpace_options_errorSelector
  , pngRepresentationOfImage_format_colorSpace_optionsSelector
  , openEXRRepresentationOfImage_options_errorSelector
  , writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector
  , writePNGRepresentationOfImage_toURL_format_colorSpace_options_errorSelector
  , writeJPEGRepresentationOfImage_toURL_colorSpace_options_errorSelector
  , writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector
  , writeHEIF10RepresentationOfImage_toURL_colorSpace_options_errorSelector
  , writeOpenEXRRepresentationOfImage_toURL_options_errorSelector
  , offlineGPUCountSelector
  , contextForOfflineGPUAtIndexSelector
  , contextForOfflineGPUAtIndex_colorSpace_options_sharedContextSelector
  , calculateHDRStatsForIOSurfaceSelector
  , calculateHDRStatsForCVPixelBufferSelector
  , calculateHDRStatsForCGImageSelector
  , calculateHDRStatsForImageSelector
  , workingColorSpaceSelector
  , workingFormatSelector


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

-- | @+ contextWithCGLContext:pixelFormat:colorSpace:options:@
contextWithCGLContext_pixelFormat_colorSpace_options :: IsNSDictionary options => Ptr () -> Ptr () -> Ptr () -> options -> IO (Id CIContext)
contextWithCGLContext_pixelFormat_colorSpace_options cglctx pixelFormat colorSpace options =
  do
    cls' <- getRequiredClass "CIContext"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "contextWithCGLContext:pixelFormat:colorSpace:options:") (retPtr retVoid) [argPtr cglctx, argPtr pixelFormat, argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextWithCGLContext:pixelFormat:options:@
contextWithCGLContext_pixelFormat_options :: IsNSDictionary options => Ptr () -> Ptr () -> options -> IO (Id CIContext)
contextWithCGLContext_pixelFormat_options cglctx pixelFormat options =
  do
    cls' <- getRequiredClass "CIContext"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "contextWithCGLContext:pixelFormat:options:") (retPtr retVoid) [argPtr cglctx, argPtr pixelFormat, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextWithCGContext:options:@
contextWithCGContext_options :: IsNSDictionary options => Ptr () -> options -> IO (Id CIContext)
contextWithCGContext_options cgctx options =
  do
    cls' <- getRequiredClass "CIContext"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "contextWithCGContext:options:") (retPtr retVoid) [argPtr cgctx, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextWithOptions:@
contextWithOptions :: IsNSDictionary options => options -> IO (Id CIContext)
contextWithOptions options =
  do
    cls' <- getRequiredClass "CIContext"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "contextWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ context@
context :: IO (Id CIContext)
context  =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMsg cls' (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithOptions:@
initWithOptions :: (IsCIContext ciContext, IsNSDictionary options) => ciContext -> options -> IO (Id CIContext)
initWithOptions ciContext  options =
withObjCPtr options $ \raw_options ->
    sendMsg ciContext (mkSelector "initWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCIContext ciContext => ciContext -> IO (Id CIContext)
init_ ciContext  =
  sendMsg ciContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ contextWithMTLDevice:@
contextWithMTLDevice :: RawId -> IO (Id CIContext)
contextWithMTLDevice device =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMsg cls' (mkSelector "contextWithMTLDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextWithMTLDevice:options:@
contextWithMTLDevice_options :: IsNSDictionary options => RawId -> options -> IO (Id CIContext)
contextWithMTLDevice_options device options =
  do
    cls' <- getRequiredClass "CIContext"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "contextWithMTLDevice:options:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextWithMTLCommandQueue:@
contextWithMTLCommandQueue :: RawId -> IO (Id CIContext)
contextWithMTLCommandQueue commandQueue =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMsg cls' (mkSelector "contextWithMTLCommandQueue:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ contextWithMTLCommandQueue:options:@
contextWithMTLCommandQueue_options :: IsNSDictionary options => RawId -> options -> IO (Id CIContext)
contextWithMTLCommandQueue_options commandQueue options =
  do
    cls' <- getRequiredClass "CIContext"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "contextWithMTLCommandQueue:options:") (retPtr retVoid) [argPtr (castPtr (unRawId commandQueue) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- render:toCVPixelBuffer:@
render_toCVPixelBuffer :: (IsCIContext ciContext, IsCIImage image) => ciContext -> image -> Ptr () -> IO ()
render_toCVPixelBuffer ciContext  image buffer =
withObjCPtr image $ \raw_image ->
    sendMsg ciContext (mkSelector "render:toCVPixelBuffer:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argPtr buffer]

-- | @- reclaimResources@
reclaimResources :: IsCIContext ciContext => ciContext -> IO ()
reclaimResources ciContext  =
  sendMsg ciContext (mkSelector "reclaimResources") retVoid []

-- | @- clearCaches@
clearCaches :: IsCIContext ciContext => ciContext -> IO ()
clearCaches ciContext  =
  sendMsg ciContext (mkSelector "clearCaches") retVoid []

-- | @- startTaskToRender:toDestination:error:@
startTaskToRender_toDestination_error :: (IsCIContext ciContext, IsCIImage image, IsCIRenderDestination destination, IsNSError error_) => ciContext -> image -> destination -> error_ -> IO (Id CIRenderTask)
startTaskToRender_toDestination_error ciContext  image destination error_ =
withObjCPtr image $ \raw_image ->
  withObjCPtr destination $ \raw_destination ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg ciContext (mkSelector "startTaskToRender:toDestination:error:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- startTaskToClear:error:@
startTaskToClear_error :: (IsCIContext ciContext, IsCIRenderDestination destination, IsNSError error_) => ciContext -> destination -> error_ -> IO (Id CIRenderTask)
startTaskToClear_error ciContext  destination error_ =
withObjCPtr destination $ \raw_destination ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg ciContext (mkSelector "startTaskToClear:error:") (retPtr retVoid) [argPtr (castPtr raw_destination :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- depthBlurEffectFilterForImageURL:options:@
depthBlurEffectFilterForImageURL_options :: (IsCIContext ciContext, IsNSURL url, IsNSDictionary options) => ciContext -> url -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImageURL_options ciContext  url options =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
      sendMsg ciContext (mkSelector "depthBlurEffectFilterForImageURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- depthBlurEffectFilterForImageData:options:@
depthBlurEffectFilterForImageData_options :: (IsCIContext ciContext, IsNSData data_, IsNSDictionary options) => ciContext -> data_ -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImageData_options ciContext  data_ options =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr options $ \raw_options ->
      sendMsg ciContext (mkSelector "depthBlurEffectFilterForImageData:options:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_options :: (IsCIContext ciContext, IsCIImage image, IsCIImage disparityImage, IsCIImage portraitEffectsMatte, IsNSDictionary options) => ciContext -> image -> disparityImage -> portraitEffectsMatte -> CInt -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_options ciContext  image disparityImage portraitEffectsMatte orientation options =
withObjCPtr image $ \raw_image ->
  withObjCPtr disparityImage $ \raw_disparityImage ->
    withObjCPtr portraitEffectsMatte $ \raw_portraitEffectsMatte ->
      withObjCPtr options $ \raw_options ->
          sendMsg ciContext (mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:orientation:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_disparityImage :: Ptr ()), argPtr (castPtr raw_portraitEffectsMatte :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_options :: (IsCIContext ciContext, IsCIImage image, IsCIImage disparityImage, IsCIImage portraitEffectsMatte, IsCIImage hairSemanticSegmentation, IsNSDictionary options) => ciContext -> image -> disparityImage -> portraitEffectsMatte -> hairSemanticSegmentation -> CInt -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_options ciContext  image disparityImage portraitEffectsMatte hairSemanticSegmentation orientation options =
withObjCPtr image $ \raw_image ->
  withObjCPtr disparityImage $ \raw_disparityImage ->
    withObjCPtr portraitEffectsMatte $ \raw_portraitEffectsMatte ->
      withObjCPtr hairSemanticSegmentation $ \raw_hairSemanticSegmentation ->
        withObjCPtr options $ \raw_options ->
            sendMsg ciContext (mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:orientation:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_disparityImage :: Ptr ()), argPtr (castPtr raw_portraitEffectsMatte :: Ptr ()), argPtr (castPtr raw_hairSemanticSegmentation :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:glassesMatte:gainMap:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_options :: (IsCIContext ciContext, IsCIImage image, IsCIImage disparityImage, IsCIImage portraitEffectsMatte, IsCIImage hairSemanticSegmentation, IsCIImage glassesMatte, IsCIImage gainMap, IsNSDictionary options) => ciContext -> image -> disparityImage -> portraitEffectsMatte -> hairSemanticSegmentation -> glassesMatte -> gainMap -> CInt -> options -> IO (Id CIFilter)
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_options ciContext  image disparityImage portraitEffectsMatte hairSemanticSegmentation glassesMatte gainMap orientation options =
withObjCPtr image $ \raw_image ->
  withObjCPtr disparityImage $ \raw_disparityImage ->
    withObjCPtr portraitEffectsMatte $ \raw_portraitEffectsMatte ->
      withObjCPtr hairSemanticSegmentation $ \raw_hairSemanticSegmentation ->
        withObjCPtr glassesMatte $ \raw_glassesMatte ->
          withObjCPtr gainMap $ \raw_gainMap ->
            withObjCPtr options $ \raw_options ->
                sendMsg ciContext (mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:glassesMatte:gainMap:orientation:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_disparityImage :: Ptr ()), argPtr (castPtr raw_portraitEffectsMatte :: Ptr ()), argPtr (castPtr raw_hairSemanticSegmentation :: Ptr ()), argPtr (castPtr raw_glassesMatte :: Ptr ()), argPtr (castPtr raw_gainMap :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- TIFFRepresentationOfImage:format:colorSpace:options:@
tiffRepresentationOfImage_format_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> CInt -> Ptr () -> options -> IO (Id NSData)
tiffRepresentationOfImage_format_colorSpace_options ciContext  image format colorSpace options =
withObjCPtr image $ \raw_image ->
  withObjCPtr options $ \raw_options ->
      sendMsg ciContext (mkSelector "TIFFRepresentationOfImage:format:colorSpace:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argCInt (fromIntegral format), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- JPEGRepresentationOfImage:colorSpace:options:@
jpegRepresentationOfImage_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> Ptr () -> options -> IO (Id NSData)
jpegRepresentationOfImage_colorSpace_options ciContext  image colorSpace options =
withObjCPtr image $ \raw_image ->
  withObjCPtr options $ \raw_options ->
      sendMsg ciContext (mkSelector "JPEGRepresentationOfImage:colorSpace:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- HEIFRepresentationOfImage:format:colorSpace:options:@
heifRepresentationOfImage_format_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> CInt -> Ptr () -> options -> IO (Id NSData)
heifRepresentationOfImage_format_colorSpace_options ciContext  image format colorSpace options =
withObjCPtr image $ \raw_image ->
  withObjCPtr options $ \raw_options ->
      sendMsg ciContext (mkSelector "HEIFRepresentationOfImage:format:colorSpace:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argCInt (fromIntegral format), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- HEIF10RepresentationOfImage:colorSpace:options:error:@
heiF10RepresentationOfImage_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> Ptr () -> options -> errorPtr -> IO (Id NSData)
heiF10RepresentationOfImage_colorSpace_options_error ciContext  image colorSpace options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr errorPtr $ \raw_errorPtr ->
        sendMsg ciContext (mkSelector "HEIF10RepresentationOfImage:colorSpace:options:error:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())] >>= retainedObject . castPtr

-- | @- PNGRepresentationOfImage:format:colorSpace:options:@
pngRepresentationOfImage_format_colorSpace_options :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options) => ciContext -> image -> CInt -> Ptr () -> options -> IO (Id NSData)
pngRepresentationOfImage_format_colorSpace_options ciContext  image format colorSpace options =
withObjCPtr image $ \raw_image ->
  withObjCPtr options $ \raw_options ->
      sendMsg ciContext (mkSelector "PNGRepresentationOfImage:format:colorSpace:options:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argCInt (fromIntegral format), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- OpenEXRRepresentationOfImage:options:error:@
openEXRRepresentationOfImage_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> options -> errorPtr -> IO (Id NSData)
openEXRRepresentationOfImage_options_error ciContext  image options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr options $ \raw_options ->
    withObjCPtr errorPtr $ \raw_errorPtr ->
        sendMsg ciContext (mkSelector "OpenEXRRepresentationOfImage:options:error:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeTIFFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> CInt -> Ptr () -> options -> errorPtr -> IO Bool
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_error ciContext  image url format colorSpace options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciContext (mkSelector "writeTIFFRepresentationOfImage:toURL:format:colorSpace:options:error:") retCULong [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argCInt (fromIntegral format), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @- writePNGRepresentationOfImage:toURL:format:colorSpace:options:error:@
writePNGRepresentationOfImage_toURL_format_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> CInt -> Ptr () -> options -> errorPtr -> IO Bool
writePNGRepresentationOfImage_toURL_format_colorSpace_options_error ciContext  image url format colorSpace options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciContext (mkSelector "writePNGRepresentationOfImage:toURL:format:colorSpace:options:error:") retCULong [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argCInt (fromIntegral format), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @- writeJPEGRepresentationOfImage:toURL:colorSpace:options:error:@
writeJPEGRepresentationOfImage_toURL_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> Ptr () -> options -> errorPtr -> IO Bool
writeJPEGRepresentationOfImage_toURL_colorSpace_options_error ciContext  image url colorSpace options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciContext (mkSelector "writeJPEGRepresentationOfImage:toURL:colorSpace:options:error:") retCULong [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @- writeHEIFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> CInt -> Ptr () -> options -> errorPtr -> IO Bool
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_error ciContext  image url format colorSpace options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciContext (mkSelector "writeHEIFRepresentationOfImage:toURL:format:colorSpace:options:error:") retCULong [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argCInt (fromIntegral format), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @- writeHEIF10RepresentationOfImage:toURL:colorSpace:options:error:@
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> Ptr () -> options -> errorPtr -> IO Bool
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_error ciContext  image url colorSpace options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciContext (mkSelector "writeHEIF10RepresentationOfImage:toURL:colorSpace:options:error:") retCULong [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @- writeOpenEXRRepresentationOfImage:toURL:options:error:@
writeOpenEXRRepresentationOfImage_toURL_options_error :: (IsCIContext ciContext, IsCIImage image, IsNSURL url, IsNSDictionary options, IsNSError errorPtr) => ciContext -> image -> url -> options -> errorPtr -> IO Bool
writeOpenEXRRepresentationOfImage_toURL_options_error ciContext  image url options errorPtr =
withObjCPtr image $ \raw_image ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr errorPtr $ \raw_errorPtr ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciContext (mkSelector "writeOpenEXRRepresentationOfImage:toURL:options:error:") retCULong [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_errorPtr :: Ptr ())]

-- | @+ offlineGPUCount@
offlineGPUCount :: IO CUInt
offlineGPUCount  =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMsg cls' (mkSelector "offlineGPUCount") retCUInt []

-- | @+ contextForOfflineGPUAtIndex:@
contextForOfflineGPUAtIndex :: CUInt -> IO (Id CIContext)
contextForOfflineGPUAtIndex index =
  do
    cls' <- getRequiredClass "CIContext"
    sendClassMsg cls' (mkSelector "contextForOfflineGPUAtIndex:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @+ contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:@
contextForOfflineGPUAtIndex_colorSpace_options_sharedContext :: IsNSDictionary options => CUInt -> Ptr () -> options -> Ptr () -> IO (Id CIContext)
contextForOfflineGPUAtIndex_colorSpace_options_sharedContext index colorSpace options sharedContext =
  do
    cls' <- getRequiredClass "CIContext"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:") (retPtr retVoid) [argCUInt (fromIntegral index), argPtr colorSpace, argPtr (castPtr raw_options :: Ptr ()), argPtr sharedContext] >>= retainedObject . castPtr

-- | Given an IOSurface, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then update the surface's attachments to store the values.
--
-- If the @IOSurface@ has a Clean Aperture rectangle then only pixels within that rectangle are considered.
--
-- - Parameters:    - surface: A mutable @IOSurfaceRef@ for which to calculate and attach statistics.
--
-- ObjC selector: @- calculateHDRStatsForIOSurface:@
calculateHDRStatsForIOSurface :: IsCIContext ciContext => ciContext -> Ptr () -> IO ()
calculateHDRStatsForIOSurface ciContext  surface =
  sendMsg ciContext (mkSelector "calculateHDRStatsForIOSurface:") retVoid [argPtr surface]

-- | Given a CVPixelBuffer, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then update the buffers's attachments to store the values.
--
-- If the @CVPixelBuffer@ has a Clean Aperture rectangle then only pixels within that rectangle are considered.
--
-- - Parameters:    - buffer: A mutable @CVPixelBuffer@ for which to calculate and attach statistics.
--
-- ObjC selector: @- calculateHDRStatsForCVPixelBuffer:@
calculateHDRStatsForCVPixelBuffer :: IsCIContext ciContext => ciContext -> Ptr () -> IO ()
calculateHDRStatsForCVPixelBuffer ciContext  buffer =
  sendMsg ciContext (mkSelector "calculateHDRStatsForCVPixelBuffer:") retVoid [argPtr buffer]

-- | Given a Core Graphics image, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then return a new Core Graphics image that has the calculated values.
--
-- - Parameters:    - cgimage: An immutable @CGImage@ for which to calculate statistics. - Returns:    Returns a new @CGImage@ instance that has the calculated statistics attached.
--
-- ObjC selector: @- calculateHDRStatsForCGImage:@
calculateHDRStatsForCGImage :: IsCIContext ciContext => ciContext -> Ptr () -> IO (Ptr ())
calculateHDRStatsForCGImage ciContext  cgimage =
  fmap castPtr $ sendMsg ciContext (mkSelector "calculateHDRStatsForCGImage:") (retPtr retVoid) [argPtr cgimage]

-- | Given a Core Image image, use the receiving Core Image context to calculate its  HDR statistics (content headroom and content average light level) and then return a new Core Image image that has the calculated values.
--
-- If the image extent is not finite, then nil will be returned.
--
-- - Parameters:    - image: An immutable ``CIImage`` for which to calculate statistics. - Returns:    Returns a new ``CIImage`` instance that has the calculated statistics attached.
--
-- ObjC selector: @- calculateHDRStatsForImage:@
calculateHDRStatsForImage :: (IsCIContext ciContext, IsCIImage image) => ciContext -> image -> IO (Id CIImage)
calculateHDRStatsForImage ciContext  image =
withObjCPtr image $ \raw_image ->
    sendMsg ciContext (mkSelector "calculateHDRStatsForImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | The working color space of the CIContext.
--
-- The working color space determines the color space used when executing filter kernels.  You specify a working color space using the ``kCIContextWorkingColorSpace`` option when creating a ``CIContext``. * All input images are color matched from the input's color space to the working space. * All renders are color matched from the working space to the destination space.
--
-- The property will be @null@ if the context was created with color management disabled.
--
-- ObjC selector: @- workingColorSpace@
workingColorSpace :: IsCIContext ciContext => ciContext -> IO (Ptr ())
workingColorSpace ciContext  =
  fmap castPtr $ sendMsg ciContext (mkSelector "workingColorSpace") (retPtr retVoid) []

-- | The working pixel format that the CIContext uses for intermediate buffers.
--
-- The working format determines the pixel format that Core Image uses to create intermediate buffers for rendering images.  You specify a working pixel format using the ``kCIContextWorkingFormat`` option when creating a ``CIContext``.
--
-- ObjC selector: @- workingFormat@
workingFormat :: IsCIContext ciContext => ciContext -> IO CInt
workingFormat ciContext  =
  sendMsg ciContext (mkSelector "workingFormat") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contextWithCGLContext:pixelFormat:colorSpace:options:@
contextWithCGLContext_pixelFormat_colorSpace_optionsSelector :: Selector
contextWithCGLContext_pixelFormat_colorSpace_optionsSelector = mkSelector "contextWithCGLContext:pixelFormat:colorSpace:options:"

-- | @Selector@ for @contextWithCGLContext:pixelFormat:options:@
contextWithCGLContext_pixelFormat_optionsSelector :: Selector
contextWithCGLContext_pixelFormat_optionsSelector = mkSelector "contextWithCGLContext:pixelFormat:options:"

-- | @Selector@ for @contextWithCGContext:options:@
contextWithCGContext_optionsSelector :: Selector
contextWithCGContext_optionsSelector = mkSelector "contextWithCGContext:options:"

-- | @Selector@ for @contextWithOptions:@
contextWithOptionsSelector :: Selector
contextWithOptionsSelector = mkSelector "contextWithOptions:"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @contextWithMTLDevice:@
contextWithMTLDeviceSelector :: Selector
contextWithMTLDeviceSelector = mkSelector "contextWithMTLDevice:"

-- | @Selector@ for @contextWithMTLDevice:options:@
contextWithMTLDevice_optionsSelector :: Selector
contextWithMTLDevice_optionsSelector = mkSelector "contextWithMTLDevice:options:"

-- | @Selector@ for @contextWithMTLCommandQueue:@
contextWithMTLCommandQueueSelector :: Selector
contextWithMTLCommandQueueSelector = mkSelector "contextWithMTLCommandQueue:"

-- | @Selector@ for @contextWithMTLCommandQueue:options:@
contextWithMTLCommandQueue_optionsSelector :: Selector
contextWithMTLCommandQueue_optionsSelector = mkSelector "contextWithMTLCommandQueue:options:"

-- | @Selector@ for @render:toCVPixelBuffer:@
render_toCVPixelBufferSelector :: Selector
render_toCVPixelBufferSelector = mkSelector "render:toCVPixelBuffer:"

-- | @Selector@ for @reclaimResources@
reclaimResourcesSelector :: Selector
reclaimResourcesSelector = mkSelector "reclaimResources"

-- | @Selector@ for @clearCaches@
clearCachesSelector :: Selector
clearCachesSelector = mkSelector "clearCaches"

-- | @Selector@ for @startTaskToRender:toDestination:error:@
startTaskToRender_toDestination_errorSelector :: Selector
startTaskToRender_toDestination_errorSelector = mkSelector "startTaskToRender:toDestination:error:"

-- | @Selector@ for @startTaskToClear:error:@
startTaskToClear_errorSelector :: Selector
startTaskToClear_errorSelector = mkSelector "startTaskToClear:error:"

-- | @Selector@ for @depthBlurEffectFilterForImageURL:options:@
depthBlurEffectFilterForImageURL_optionsSelector :: Selector
depthBlurEffectFilterForImageURL_optionsSelector = mkSelector "depthBlurEffectFilterForImageURL:options:"

-- | @Selector@ for @depthBlurEffectFilterForImageData:options:@
depthBlurEffectFilterForImageData_optionsSelector :: Selector
depthBlurEffectFilterForImageData_optionsSelector = mkSelector "depthBlurEffectFilterForImageData:options:"

-- | @Selector@ for @depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_optionsSelector :: Selector
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_orientation_optionsSelector = mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:orientation:options:"

-- | @Selector@ for @depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_optionsSelector :: Selector
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_orientation_optionsSelector = mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:orientation:options:"

-- | @Selector@ for @depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:glassesMatte:gainMap:orientation:options:@
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_optionsSelector :: Selector
depthBlurEffectFilterForImage_disparityImage_portraitEffectsMatte_hairSemanticSegmentation_glassesMatte_gainMap_orientation_optionsSelector = mkSelector "depthBlurEffectFilterForImage:disparityImage:portraitEffectsMatte:hairSemanticSegmentation:glassesMatte:gainMap:orientation:options:"

-- | @Selector@ for @TIFFRepresentationOfImage:format:colorSpace:options:@
tiffRepresentationOfImage_format_colorSpace_optionsSelector :: Selector
tiffRepresentationOfImage_format_colorSpace_optionsSelector = mkSelector "TIFFRepresentationOfImage:format:colorSpace:options:"

-- | @Selector@ for @JPEGRepresentationOfImage:colorSpace:options:@
jpegRepresentationOfImage_colorSpace_optionsSelector :: Selector
jpegRepresentationOfImage_colorSpace_optionsSelector = mkSelector "JPEGRepresentationOfImage:colorSpace:options:"

-- | @Selector@ for @HEIFRepresentationOfImage:format:colorSpace:options:@
heifRepresentationOfImage_format_colorSpace_optionsSelector :: Selector
heifRepresentationOfImage_format_colorSpace_optionsSelector = mkSelector "HEIFRepresentationOfImage:format:colorSpace:options:"

-- | @Selector@ for @HEIF10RepresentationOfImage:colorSpace:options:error:@
heiF10RepresentationOfImage_colorSpace_options_errorSelector :: Selector
heiF10RepresentationOfImage_colorSpace_options_errorSelector = mkSelector "HEIF10RepresentationOfImage:colorSpace:options:error:"

-- | @Selector@ for @PNGRepresentationOfImage:format:colorSpace:options:@
pngRepresentationOfImage_format_colorSpace_optionsSelector :: Selector
pngRepresentationOfImage_format_colorSpace_optionsSelector = mkSelector "PNGRepresentationOfImage:format:colorSpace:options:"

-- | @Selector@ for @OpenEXRRepresentationOfImage:options:error:@
openEXRRepresentationOfImage_options_errorSelector :: Selector
openEXRRepresentationOfImage_options_errorSelector = mkSelector "OpenEXRRepresentationOfImage:options:error:"

-- | @Selector@ for @writeTIFFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector :: Selector
writeTIFFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector = mkSelector "writeTIFFRepresentationOfImage:toURL:format:colorSpace:options:error:"

-- | @Selector@ for @writePNGRepresentationOfImage:toURL:format:colorSpace:options:error:@
writePNGRepresentationOfImage_toURL_format_colorSpace_options_errorSelector :: Selector
writePNGRepresentationOfImage_toURL_format_colorSpace_options_errorSelector = mkSelector "writePNGRepresentationOfImage:toURL:format:colorSpace:options:error:"

-- | @Selector@ for @writeJPEGRepresentationOfImage:toURL:colorSpace:options:error:@
writeJPEGRepresentationOfImage_toURL_colorSpace_options_errorSelector :: Selector
writeJPEGRepresentationOfImage_toURL_colorSpace_options_errorSelector = mkSelector "writeJPEGRepresentationOfImage:toURL:colorSpace:options:error:"

-- | @Selector@ for @writeHEIFRepresentationOfImage:toURL:format:colorSpace:options:error:@
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector :: Selector
writeHEIFRepresentationOfImage_toURL_format_colorSpace_options_errorSelector = mkSelector "writeHEIFRepresentationOfImage:toURL:format:colorSpace:options:error:"

-- | @Selector@ for @writeHEIF10RepresentationOfImage:toURL:colorSpace:options:error:@
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_errorSelector :: Selector
writeHEIF10RepresentationOfImage_toURL_colorSpace_options_errorSelector = mkSelector "writeHEIF10RepresentationOfImage:toURL:colorSpace:options:error:"

-- | @Selector@ for @writeOpenEXRRepresentationOfImage:toURL:options:error:@
writeOpenEXRRepresentationOfImage_toURL_options_errorSelector :: Selector
writeOpenEXRRepresentationOfImage_toURL_options_errorSelector = mkSelector "writeOpenEXRRepresentationOfImage:toURL:options:error:"

-- | @Selector@ for @offlineGPUCount@
offlineGPUCountSelector :: Selector
offlineGPUCountSelector = mkSelector "offlineGPUCount"

-- | @Selector@ for @contextForOfflineGPUAtIndex:@
contextForOfflineGPUAtIndexSelector :: Selector
contextForOfflineGPUAtIndexSelector = mkSelector "contextForOfflineGPUAtIndex:"

-- | @Selector@ for @contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:@
contextForOfflineGPUAtIndex_colorSpace_options_sharedContextSelector :: Selector
contextForOfflineGPUAtIndex_colorSpace_options_sharedContextSelector = mkSelector "contextForOfflineGPUAtIndex:colorSpace:options:sharedContext:"

-- | @Selector@ for @calculateHDRStatsForIOSurface:@
calculateHDRStatsForIOSurfaceSelector :: Selector
calculateHDRStatsForIOSurfaceSelector = mkSelector "calculateHDRStatsForIOSurface:"

-- | @Selector@ for @calculateHDRStatsForCVPixelBuffer:@
calculateHDRStatsForCVPixelBufferSelector :: Selector
calculateHDRStatsForCVPixelBufferSelector = mkSelector "calculateHDRStatsForCVPixelBuffer:"

-- | @Selector@ for @calculateHDRStatsForCGImage:@
calculateHDRStatsForCGImageSelector :: Selector
calculateHDRStatsForCGImageSelector = mkSelector "calculateHDRStatsForCGImage:"

-- | @Selector@ for @calculateHDRStatsForImage:@
calculateHDRStatsForImageSelector :: Selector
calculateHDRStatsForImageSelector = mkSelector "calculateHDRStatsForImage:"

-- | @Selector@ for @workingColorSpace@
workingColorSpaceSelector :: Selector
workingColorSpaceSelector = mkSelector "workingColorSpace"

-- | @Selector@ for @workingFormat@
workingFormatSelector :: Selector
workingFormatSelector = mkSelector "workingFormat"

