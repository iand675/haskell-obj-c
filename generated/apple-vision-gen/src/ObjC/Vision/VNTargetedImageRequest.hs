{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that requires an initial image as a starting point for its work.
--
-- Generated bindings for @VNTargetedImageRequest@.
module ObjC.Vision.VNTargetedImageRequest
  ( VNTargetedImageRequest
  , IsVNTargetedImageRequest(..)
  , init_
  , initWithCompletionHandler
  , initWithTargetedCVPixelBuffer_options
  , initWithTargetedCVPixelBuffer_options_completionHandler
  , initWithTargetedCVPixelBuffer_orientation_options
  , initWithTargetedCVPixelBuffer_orientation_options_completionHandler
  , initWithTargetedCGImage_options
  , initWithTargetedCGImage_options_completionHandler
  , initWithTargetedCGImage_orientation_options
  , initWithTargetedCGImage_orientation_options_completionHandler
  , initWithTargetedCIImage_options
  , initWithTargetedCIImage_options_completionHandler
  , initWithTargetedCIImage_orientation_options
  , initWithTargetedCIImage_orientation_options_completionHandler
  , initWithTargetedImageURL_options
  , initWithTargetedImageURL_options_completionHandler
  , initWithTargetedImageURL_orientation_options
  , initWithTargetedImageURL_orientation_options_completionHandler
  , initWithTargetedImageData_options
  , initWithTargetedImageData_options_completionHandler
  , initWithTargetedImageData_orientation_options
  , initWithTargetedImageData_orientation_options_completionHandler
  , initWithTargetedCMSampleBuffer_options
  , initWithTargetedCMSampleBuffer_options_completionHandler
  , initWithTargetedCMSampleBuffer_orientation_options
  , initWithTargetedCMSampleBuffer_orientation_options_completionHandler
  , initSelector
  , initWithCompletionHandlerSelector
  , initWithTargetedCGImage_optionsSelector
  , initWithTargetedCGImage_options_completionHandlerSelector
  , initWithTargetedCGImage_orientation_optionsSelector
  , initWithTargetedCGImage_orientation_options_completionHandlerSelector
  , initWithTargetedCIImage_optionsSelector
  , initWithTargetedCIImage_options_completionHandlerSelector
  , initWithTargetedCIImage_orientation_optionsSelector
  , initWithTargetedCIImage_orientation_options_completionHandlerSelector
  , initWithTargetedCMSampleBuffer_optionsSelector
  , initWithTargetedCMSampleBuffer_options_completionHandlerSelector
  , initWithTargetedCMSampleBuffer_orientation_optionsSelector
  , initWithTargetedCMSampleBuffer_orientation_options_completionHandlerSelector
  , initWithTargetedCVPixelBuffer_optionsSelector
  , initWithTargetedCVPixelBuffer_options_completionHandlerSelector
  , initWithTargetedCVPixelBuffer_orientation_optionsSelector
  , initWithTargetedCVPixelBuffer_orientation_options_completionHandlerSelector
  , initWithTargetedImageData_optionsSelector
  , initWithTargetedImageData_options_completionHandlerSelector
  , initWithTargetedImageData_orientation_optionsSelector
  , initWithTargetedImageData_orientation_options_completionHandlerSelector
  , initWithTargetedImageURL_optionsSelector
  , initWithTargetedImageURL_options_completionHandlerSelector
  , initWithTargetedImageURL_orientation_optionsSelector
  , initWithTargetedImageURL_orientation_options_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNTargetedImageRequest vnTargetedImageRequest => vnTargetedImageRequest -> IO (Id VNTargetedImageRequest)
init_ vnTargetedImageRequest =
  sendOwnedMessage vnTargetedImageRequest initSelector

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTargetedImageRequest vnTargetedImageRequest => vnTargetedImageRequest -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithCompletionHandler vnTargetedImageRequest completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithCompletionHandlerSelector completionHandler

-- | Create a new request that targets an image in a pixel buffer.
--
-- @pixelBuffer@ — The pixel buffer containing the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCVPixelBuffer:options:@
initWithTargetedCVPixelBuffer_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_options vnTargetedImageRequest pixelBuffer options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCVPixelBuffer_optionsSelector pixelBuffer (toNSDictionary options)

-- | Create a new request that targets an image in a pixel buffer.
--
-- @pixelBuffer@ — The pixel buffer containing the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedCVPixelBuffer:options:completionHandler:@
initWithTargetedCVPixelBuffer_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_options_completionHandler vnTargetedImageRequest pixelBuffer options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCVPixelBuffer_options_completionHandlerSelector pixelBuffer (toNSDictionary options) completionHandler

-- | Create a new request that targets an image in a pixel buffer.
--
-- @pixelBuffer@ — The pixel buffer containing the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCVPixelBuffer:orientation:options:@
initWithTargetedCVPixelBuffer_orientation_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> CInt -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_orientation_options vnTargetedImageRequest pixelBuffer orientation options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCVPixelBuffer_orientation_optionsSelector pixelBuffer orientation (toNSDictionary options)

-- | Create a new request that targets an image in a pixel buffer.
--
-- @pixelBuffer@ — The pixel buffer containing the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedCVPixelBuffer:orientation:options:completionHandler:@
initWithTargetedCVPixelBuffer_orientation_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> CInt -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_orientation_options_completionHandler vnTargetedImageRequest pixelBuffer orientation options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCVPixelBuffer_orientation_options_completionHandlerSelector pixelBuffer orientation (toNSDictionary options) completionHandler

-- | Create a new request with a targeted CGImage.
--
-- @cgImage@ — The CGImageRef of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCGImage:options:@
initWithTargetedCGImage_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCGImage_options vnTargetedImageRequest cgImage options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCGImage_optionsSelector cgImage (toNSDictionary options)

-- | Create a new request with a targeted CGImage.
--
-- @cgImage@ — The CGImageRef of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedCGImage:options:completionHandler:@
initWithTargetedCGImage_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCGImage_options_completionHandler vnTargetedImageRequest cgImage options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCGImage_options_completionHandlerSelector cgImage (toNSDictionary options) completionHandler

-- | Create a new request with a targeted CGImage.
--
-- @cgImage@ — The CGImageRef of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCGImage:orientation:options:@
initWithTargetedCGImage_orientation_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> CInt -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCGImage_orientation_options vnTargetedImageRequest cgImage orientation options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCGImage_orientation_optionsSelector cgImage orientation (toNSDictionary options)

-- | Create a new request with a targeted CGImage.
--
-- @cgImage@ — The CGImageRef of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedCGImage:orientation:options:completionHandler:@
initWithTargetedCGImage_orientation_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> CInt -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCGImage_orientation_options_completionHandler vnTargetedImageRequest cgImage orientation options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCGImage_orientation_options_completionHandlerSelector cgImage orientation (toNSDictionary options) completionHandler

-- | Create a new request with a targeted CIImage.
--
-- @ciImage@ — The CIImage of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCIImage:options:@
initWithTargetedCIImage_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsCIImage ciImage, IsNSDictionary options) => vnTargetedImageRequest -> ciImage -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCIImage_options vnTargetedImageRequest ciImage options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCIImage_optionsSelector (toCIImage ciImage) (toNSDictionary options)

-- | Create a new request with a targeted CIImage.
--
-- @ciImage@ — The CIImage of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedCIImage:options:completionHandler:@
initWithTargetedCIImage_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsCIImage ciImage, IsNSDictionary options) => vnTargetedImageRequest -> ciImage -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCIImage_options_completionHandler vnTargetedImageRequest ciImage options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCIImage_options_completionHandlerSelector (toCIImage ciImage) (toNSDictionary options) completionHandler

-- | Create a new request with a targeted CIImage.
--
-- @ciImage@ — The CIImage of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCIImage:orientation:options:@
initWithTargetedCIImage_orientation_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsCIImage ciImage, IsNSDictionary options) => vnTargetedImageRequest -> ciImage -> CInt -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCIImage_orientation_options vnTargetedImageRequest ciImage orientation options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCIImage_orientation_optionsSelector (toCIImage ciImage) orientation (toNSDictionary options)

-- | Create a new request with a targeted CIImage.
--
-- @ciImage@ — The CIImage of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedCIImage:orientation:options:completionHandler:@
initWithTargetedCIImage_orientation_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsCIImage ciImage, IsNSDictionary options) => vnTargetedImageRequest -> ciImage -> CInt -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCIImage_orientation_options_completionHandler vnTargetedImageRequest ciImage orientation options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCIImage_orientation_options_completionHandlerSelector (toCIImage ciImage) orientation (toNSDictionary options) completionHandler

-- | Create a new request with a targeted image URL.
--
-- @imageURL@ — The URL of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedImageURL:options:@
initWithTargetedImageURL_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSURL imageURL, IsNSDictionary options) => vnTargetedImageRequest -> imageURL -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedImageURL_options vnTargetedImageRequest imageURL options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageURL_optionsSelector (toNSURL imageURL) (toNSDictionary options)

-- | Create a new request with a targeted image URL.
--
-- @imageURL@ — The URL of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedImageURL:options:completionHandler:@
initWithTargetedImageURL_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSURL imageURL, IsNSDictionary options) => vnTargetedImageRequest -> imageURL -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedImageURL_options_completionHandler vnTargetedImageRequest imageURL options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageURL_options_completionHandlerSelector (toNSURL imageURL) (toNSDictionary options) completionHandler

-- | Create a new request with a targeted image URL.
--
-- @imageURL@ — The URL of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedImageURL:orientation:options:@
initWithTargetedImageURL_orientation_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSURL imageURL, IsNSDictionary options) => vnTargetedImageRequest -> imageURL -> CInt -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedImageURL_orientation_options vnTargetedImageRequest imageURL orientation options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageURL_orientation_optionsSelector (toNSURL imageURL) orientation (toNSDictionary options)

-- | Create a new request with a targeted image URL.
--
-- @imageURL@ — The URL of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedImageURL:orientation:options:completionHandler:@
initWithTargetedImageURL_orientation_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSURL imageURL, IsNSDictionary options) => vnTargetedImageRequest -> imageURL -> CInt -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedImageURL_orientation_options_completionHandler vnTargetedImageRequest imageURL orientation options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageURL_orientation_options_completionHandlerSelector (toNSURL imageURL) orientation (toNSDictionary options) completionHandler

-- | Create a new request with a targeted image data.
--
-- @imageData@ — The data of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedImageData:options:@
initWithTargetedImageData_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSData imageData, IsNSDictionary options) => vnTargetedImageRequest -> imageData -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedImageData_options vnTargetedImageRequest imageData options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageData_optionsSelector (toNSData imageData) (toNSDictionary options)

-- | Create a new request with a targeted image data.
--
-- @imageData@ — The data of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedImageData:options:completionHandler:@
initWithTargetedImageData_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSData imageData, IsNSDictionary options) => vnTargetedImageRequest -> imageData -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedImageData_options_completionHandler vnTargetedImageRequest imageData options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageData_options_completionHandlerSelector (toNSData imageData) (toNSDictionary options) completionHandler

-- | Create a new request with a targeted image data.
--
-- @imageData@ — The data of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedImageData:orientation:options:@
initWithTargetedImageData_orientation_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSData imageData, IsNSDictionary options) => vnTargetedImageRequest -> imageData -> CInt -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedImageData_orientation_options vnTargetedImageRequest imageData orientation options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageData_orientation_optionsSelector (toNSData imageData) orientation (toNSDictionary options)

-- | Create a new request with a targeted image data.
--
-- @imageData@ — The data of the targeted image.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked when the request has been performed.
--
-- ObjC selector: @- initWithTargetedImageData:orientation:options:completionHandler:@
initWithTargetedImageData_orientation_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSData imageData, IsNSDictionary options) => vnTargetedImageRequest -> imageData -> CInt -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedImageData_orientation_options_completionHandler vnTargetedImageRequest imageData orientation options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedImageData_orientation_options_completionHandlerSelector (toNSData imageData) orientation (toNSDictionary options) completionHandler

-- | Create a new request with a targeted CMSampleBuffer.
--
-- @sampleBuffer@ — The CMSampleBuffer containing the CVImageBuffer to be used by the request.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCMSampleBuffer:options:@
initWithTargetedCMSampleBuffer_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_options vnTargetedImageRequest sampleBuffer options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCMSampleBuffer_optionsSelector sampleBuffer (toNSDictionary options)

-- | Create a new request with a targeted CMSampleBuffer.
--
-- @sampleBuffer@ — The CMSampleBuffer containing the CVImageBuffer to be used by the request.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked after the request has been performed.
--
-- ObjC selector: @- initWithTargetedCMSampleBuffer:options:completionHandler:@
initWithTargetedCMSampleBuffer_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_options_completionHandler vnTargetedImageRequest sampleBuffer options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCMSampleBuffer_options_completionHandlerSelector sampleBuffer (toNSDictionary options) completionHandler

-- | Create a new request with a targeted CMSampleBuffer.
--
-- @sampleBuffer@ — The CMSampleBuffer containing the CVImageBuffer to be used by the request.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCMSampleBuffer:orientation:options:@
initWithTargetedCMSampleBuffer_orientation_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> CInt -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_orientation_options vnTargetedImageRequest sampleBuffer orientation options =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCMSampleBuffer_orientation_optionsSelector sampleBuffer orientation (toNSDictionary options)

-- | Create a new request with a targeted CMSampleBuffer.
--
-- @sampleBuffer@ — The CMSampleBuffer containing the CVImageBuffer to be used by the request.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- @completionHandler@ — The block that is invoked after the request has been performed.
--
-- ObjC selector: @- initWithTargetedCMSampleBuffer:orientation:options:completionHandler:@
initWithTargetedCMSampleBuffer_orientation_options_completionHandler :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> CInt -> options -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_orientation_options_completionHandler vnTargetedImageRequest sampleBuffer orientation options completionHandler =
  sendOwnedMessage vnTargetedImageRequest initWithTargetedCMSampleBuffer_orientation_options_completionHandlerSelector sampleBuffer orientation (toNSDictionary options) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNTargetedImageRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNTargetedImageRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:options:@
initWithTargetedCVPixelBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_optionsSelector = mkSelector "initWithTargetedCVPixelBuffer:options:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:options:completionHandler:@
initWithTargetedCVPixelBuffer_options_completionHandlerSelector :: Selector '[Ptr (), Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_options_completionHandlerSelector = mkSelector "initWithTargetedCVPixelBuffer:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:orientation:options:@
initWithTargetedCVPixelBuffer_orientation_optionsSelector :: Selector '[Ptr (), CInt, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_orientation_optionsSelector = mkSelector "initWithTargetedCVPixelBuffer:orientation:options:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:orientation:options:completionHandler:@
initWithTargetedCVPixelBuffer_orientation_options_completionHandlerSelector :: Selector '[Ptr (), CInt, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCVPixelBuffer:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCGImage:options:@
initWithTargetedCGImage_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCGImage_optionsSelector = mkSelector "initWithTargetedCGImage:options:"

-- | @Selector@ for @initWithTargetedCGImage:options:completionHandler:@
initWithTargetedCGImage_options_completionHandlerSelector :: Selector '[Ptr (), Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCGImage_options_completionHandlerSelector = mkSelector "initWithTargetedCGImage:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCGImage:orientation:options:@
initWithTargetedCGImage_orientation_optionsSelector :: Selector '[Ptr (), CInt, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCGImage_orientation_optionsSelector = mkSelector "initWithTargetedCGImage:orientation:options:"

-- | @Selector@ for @initWithTargetedCGImage:orientation:options:completionHandler:@
initWithTargetedCGImage_orientation_options_completionHandlerSelector :: Selector '[Ptr (), CInt, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCGImage_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCGImage:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCIImage:options:@
initWithTargetedCIImage_optionsSelector :: Selector '[Id CIImage, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCIImage_optionsSelector = mkSelector "initWithTargetedCIImage:options:"

-- | @Selector@ for @initWithTargetedCIImage:options:completionHandler:@
initWithTargetedCIImage_options_completionHandlerSelector :: Selector '[Id CIImage, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCIImage_options_completionHandlerSelector = mkSelector "initWithTargetedCIImage:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCIImage:orientation:options:@
initWithTargetedCIImage_orientation_optionsSelector :: Selector '[Id CIImage, CInt, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCIImage_orientation_optionsSelector = mkSelector "initWithTargetedCIImage:orientation:options:"

-- | @Selector@ for @initWithTargetedCIImage:orientation:options:completionHandler:@
initWithTargetedCIImage_orientation_options_completionHandlerSelector :: Selector '[Id CIImage, CInt, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCIImage_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCIImage:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageURL:options:@
initWithTargetedImageURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedImageURL_optionsSelector = mkSelector "initWithTargetedImageURL:options:"

-- | @Selector@ for @initWithTargetedImageURL:options:completionHandler:@
initWithTargetedImageURL_options_completionHandlerSelector :: Selector '[Id NSURL, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedImageURL_options_completionHandlerSelector = mkSelector "initWithTargetedImageURL:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageURL:orientation:options:@
initWithTargetedImageURL_orientation_optionsSelector :: Selector '[Id NSURL, CInt, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedImageURL_orientation_optionsSelector = mkSelector "initWithTargetedImageURL:orientation:options:"

-- | @Selector@ for @initWithTargetedImageURL:orientation:options:completionHandler:@
initWithTargetedImageURL_orientation_options_completionHandlerSelector :: Selector '[Id NSURL, CInt, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedImageURL_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedImageURL:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageData:options:@
initWithTargetedImageData_optionsSelector :: Selector '[Id NSData, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedImageData_optionsSelector = mkSelector "initWithTargetedImageData:options:"

-- | @Selector@ for @initWithTargetedImageData:options:completionHandler:@
initWithTargetedImageData_options_completionHandlerSelector :: Selector '[Id NSData, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedImageData_options_completionHandlerSelector = mkSelector "initWithTargetedImageData:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageData:orientation:options:@
initWithTargetedImageData_orientation_optionsSelector :: Selector '[Id NSData, CInt, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedImageData_orientation_optionsSelector = mkSelector "initWithTargetedImageData:orientation:options:"

-- | @Selector@ for @initWithTargetedImageData:orientation:options:completionHandler:@
initWithTargetedImageData_orientation_options_completionHandlerSelector :: Selector '[Id NSData, CInt, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedImageData_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedImageData:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:options:@
initWithTargetedCMSampleBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_optionsSelector = mkSelector "initWithTargetedCMSampleBuffer:options:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:options:completionHandler:@
initWithTargetedCMSampleBuffer_options_completionHandlerSelector :: Selector '[Ptr (), Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_options_completionHandlerSelector = mkSelector "initWithTargetedCMSampleBuffer:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:orientation:options:@
initWithTargetedCMSampleBuffer_orientation_optionsSelector :: Selector '[Ptr (), CInt, Id NSDictionary] (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_orientation_optionsSelector = mkSelector "initWithTargetedCMSampleBuffer:orientation:options:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:orientation:options:completionHandler:@
initWithTargetedCMSampleBuffer_orientation_options_completionHandlerSelector :: Selector '[Ptr (), CInt, Id NSDictionary, Ptr ()] (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCMSampleBuffer:orientation:options:completionHandler:"

