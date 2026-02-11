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
  , initWithTargetedCVPixelBuffer_optionsSelector
  , initWithTargetedCVPixelBuffer_options_completionHandlerSelector
  , initWithTargetedCVPixelBuffer_orientation_optionsSelector
  , initWithTargetedCVPixelBuffer_orientation_options_completionHandlerSelector
  , initWithTargetedCGImage_optionsSelector
  , initWithTargetedCGImage_options_completionHandlerSelector
  , initWithTargetedCGImage_orientation_optionsSelector
  , initWithTargetedCGImage_orientation_options_completionHandlerSelector
  , initWithTargetedCIImage_optionsSelector
  , initWithTargetedCIImage_options_completionHandlerSelector
  , initWithTargetedCIImage_orientation_optionsSelector
  , initWithTargetedCIImage_orientation_options_completionHandlerSelector
  , initWithTargetedImageURL_optionsSelector
  , initWithTargetedImageURL_options_completionHandlerSelector
  , initWithTargetedImageURL_orientation_optionsSelector
  , initWithTargetedImageURL_orientation_options_completionHandlerSelector
  , initWithTargetedImageData_optionsSelector
  , initWithTargetedImageData_options_completionHandlerSelector
  , initWithTargetedImageData_orientation_optionsSelector
  , initWithTargetedImageData_orientation_options_completionHandlerSelector
  , initWithTargetedCMSampleBuffer_optionsSelector
  , initWithTargetedCMSampleBuffer_options_completionHandlerSelector
  , initWithTargetedCMSampleBuffer_orientation_optionsSelector
  , initWithTargetedCMSampleBuffer_orientation_options_completionHandlerSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNTargetedImageRequest vnTargetedImageRequest => vnTargetedImageRequest -> IO (Id VNTargetedImageRequest)
init_ vnTargetedImageRequest  =
  sendMsg vnTargetedImageRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNTargetedImageRequest vnTargetedImageRequest => vnTargetedImageRequest -> Ptr () -> IO (Id VNTargetedImageRequest)
initWithCompletionHandler vnTargetedImageRequest  completionHandler =
  sendMsg vnTargetedImageRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request that targets an image in a pixel buffer.
--
-- @pixelBuffer@ — The pixel buffer containing the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCVPixelBuffer:options:@
initWithTargetedCVPixelBuffer_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCVPixelBuffer_options vnTargetedImageRequest  pixelBuffer options =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCVPixelBuffer:options:") (retPtr retVoid) [argPtr pixelBuffer, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCVPixelBuffer_options_completionHandler vnTargetedImageRequest  pixelBuffer options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCVPixelBuffer:options:completionHandler:") (retPtr retVoid) [argPtr pixelBuffer, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCVPixelBuffer_orientation_options vnTargetedImageRequest  pixelBuffer orientation options =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCVPixelBuffer:orientation:options:") (retPtr retVoid) [argPtr pixelBuffer, argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCVPixelBuffer_orientation_options_completionHandler vnTargetedImageRequest  pixelBuffer orientation options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCVPixelBuffer:orientation:options:completionHandler:") (retPtr retVoid) [argPtr pixelBuffer, argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request with a targeted CGImage.
--
-- @cgImage@ — The CGImageRef of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCGImage:options:@
initWithTargetedCGImage_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCGImage_options vnTargetedImageRequest  cgImage options =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCGImage:options:") (retPtr retVoid) [argPtr cgImage, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCGImage_options_completionHandler vnTargetedImageRequest  cgImage options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCGImage:options:completionHandler:") (retPtr retVoid) [argPtr cgImage, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCGImage_orientation_options vnTargetedImageRequest  cgImage orientation options =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCGImage:orientation:options:") (retPtr retVoid) [argPtr cgImage, argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCGImage_orientation_options_completionHandler vnTargetedImageRequest  cgImage orientation options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCGImage:orientation:options:completionHandler:") (retPtr retVoid) [argPtr cgImage, argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request with a targeted CIImage.
--
-- @ciImage@ — The CIImage of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCIImage:options:@
initWithTargetedCIImage_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsCIImage ciImage, IsNSDictionary options) => vnTargetedImageRequest -> ciImage -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCIImage_options vnTargetedImageRequest  ciImage options =
withObjCPtr ciImage $ \raw_ciImage ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCIImage:options:") (retPtr retVoid) [argPtr (castPtr raw_ciImage :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCIImage_options_completionHandler vnTargetedImageRequest  ciImage options completionHandler =
withObjCPtr ciImage $ \raw_ciImage ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCIImage:options:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_ciImage :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCIImage_orientation_options vnTargetedImageRequest  ciImage orientation options =
withObjCPtr ciImage $ \raw_ciImage ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCIImage:orientation:options:") (retPtr retVoid) [argPtr (castPtr raw_ciImage :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCIImage_orientation_options_completionHandler vnTargetedImageRequest  ciImage orientation options completionHandler =
withObjCPtr ciImage $ \raw_ciImage ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCIImage:orientation:options:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_ciImage :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request with a targeted image URL.
--
-- @imageURL@ — The URL of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedImageURL:options:@
initWithTargetedImageURL_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSURL imageURL, IsNSDictionary options) => vnTargetedImageRequest -> imageURL -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedImageURL_options vnTargetedImageRequest  imageURL options =
withObjCPtr imageURL $ \raw_imageURL ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageURL:options:") (retPtr retVoid) [argPtr (castPtr raw_imageURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedImageURL_options_completionHandler vnTargetedImageRequest  imageURL options completionHandler =
withObjCPtr imageURL $ \raw_imageURL ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageURL:options:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_imageURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedImageURL_orientation_options vnTargetedImageRequest  imageURL orientation options =
withObjCPtr imageURL $ \raw_imageURL ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageURL:orientation:options:") (retPtr retVoid) [argPtr (castPtr raw_imageURL :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedImageURL_orientation_options_completionHandler vnTargetedImageRequest  imageURL orientation options completionHandler =
withObjCPtr imageURL $ \raw_imageURL ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageURL:orientation:options:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_imageURL :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request with a targeted image data.
--
-- @imageData@ — The data of the targeted image.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedImageData:options:@
initWithTargetedImageData_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSData imageData, IsNSDictionary options) => vnTargetedImageRequest -> imageData -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedImageData_options vnTargetedImageRequest  imageData options =
withObjCPtr imageData $ \raw_imageData ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageData:options:") (retPtr retVoid) [argPtr (castPtr raw_imageData :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedImageData_options_completionHandler vnTargetedImageRequest  imageData options completionHandler =
withObjCPtr imageData $ \raw_imageData ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageData:options:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_imageData :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedImageData_orientation_options vnTargetedImageRequest  imageData orientation options =
withObjCPtr imageData $ \raw_imageData ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageData:orientation:options:") (retPtr retVoid) [argPtr (castPtr raw_imageData :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedImageData_orientation_options_completionHandler vnTargetedImageRequest  imageData orientation options completionHandler =
withObjCPtr imageData $ \raw_imageData ->
  withObjCPtr options $ \raw_options ->
      sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedImageData:orientation:options:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_imageData :: Ptr ()), argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new request with a targeted CMSampleBuffer.
--
-- @sampleBuffer@ — The CMSampleBuffer containing the CVImageBuffer to be used by the request.
--
-- @options@ — A dictionary with options specifying auxiliary information for the image.
--
-- ObjC selector: @- initWithTargetedCMSampleBuffer:options:@
initWithTargetedCMSampleBuffer_options :: (IsVNTargetedImageRequest vnTargetedImageRequest, IsNSDictionary options) => vnTargetedImageRequest -> Ptr () -> options -> IO (Id VNTargetedImageRequest)
initWithTargetedCMSampleBuffer_options vnTargetedImageRequest  sampleBuffer options =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCMSampleBuffer:options:") (retPtr retVoid) [argPtr sampleBuffer, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCMSampleBuffer_options_completionHandler vnTargetedImageRequest  sampleBuffer options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCMSampleBuffer:options:completionHandler:") (retPtr retVoid) [argPtr sampleBuffer, argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCMSampleBuffer_orientation_options vnTargetedImageRequest  sampleBuffer orientation options =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCMSampleBuffer:orientation:options:") (retPtr retVoid) [argPtr sampleBuffer, argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

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
initWithTargetedCMSampleBuffer_orientation_options_completionHandler vnTargetedImageRequest  sampleBuffer orientation options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg vnTargetedImageRequest (mkSelector "initWithTargetedCMSampleBuffer:orientation:options:completionHandler:") (retPtr retVoid) [argPtr sampleBuffer, argCInt (fromIntegral orientation), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:options:@
initWithTargetedCVPixelBuffer_optionsSelector :: Selector
initWithTargetedCVPixelBuffer_optionsSelector = mkSelector "initWithTargetedCVPixelBuffer:options:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:options:completionHandler:@
initWithTargetedCVPixelBuffer_options_completionHandlerSelector :: Selector
initWithTargetedCVPixelBuffer_options_completionHandlerSelector = mkSelector "initWithTargetedCVPixelBuffer:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:orientation:options:@
initWithTargetedCVPixelBuffer_orientation_optionsSelector :: Selector
initWithTargetedCVPixelBuffer_orientation_optionsSelector = mkSelector "initWithTargetedCVPixelBuffer:orientation:options:"

-- | @Selector@ for @initWithTargetedCVPixelBuffer:orientation:options:completionHandler:@
initWithTargetedCVPixelBuffer_orientation_options_completionHandlerSelector :: Selector
initWithTargetedCVPixelBuffer_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCVPixelBuffer:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCGImage:options:@
initWithTargetedCGImage_optionsSelector :: Selector
initWithTargetedCGImage_optionsSelector = mkSelector "initWithTargetedCGImage:options:"

-- | @Selector@ for @initWithTargetedCGImage:options:completionHandler:@
initWithTargetedCGImage_options_completionHandlerSelector :: Selector
initWithTargetedCGImage_options_completionHandlerSelector = mkSelector "initWithTargetedCGImage:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCGImage:orientation:options:@
initWithTargetedCGImage_orientation_optionsSelector :: Selector
initWithTargetedCGImage_orientation_optionsSelector = mkSelector "initWithTargetedCGImage:orientation:options:"

-- | @Selector@ for @initWithTargetedCGImage:orientation:options:completionHandler:@
initWithTargetedCGImage_orientation_options_completionHandlerSelector :: Selector
initWithTargetedCGImage_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCGImage:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCIImage:options:@
initWithTargetedCIImage_optionsSelector :: Selector
initWithTargetedCIImage_optionsSelector = mkSelector "initWithTargetedCIImage:options:"

-- | @Selector@ for @initWithTargetedCIImage:options:completionHandler:@
initWithTargetedCIImage_options_completionHandlerSelector :: Selector
initWithTargetedCIImage_options_completionHandlerSelector = mkSelector "initWithTargetedCIImage:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCIImage:orientation:options:@
initWithTargetedCIImage_orientation_optionsSelector :: Selector
initWithTargetedCIImage_orientation_optionsSelector = mkSelector "initWithTargetedCIImage:orientation:options:"

-- | @Selector@ for @initWithTargetedCIImage:orientation:options:completionHandler:@
initWithTargetedCIImage_orientation_options_completionHandlerSelector :: Selector
initWithTargetedCIImage_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCIImage:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageURL:options:@
initWithTargetedImageURL_optionsSelector :: Selector
initWithTargetedImageURL_optionsSelector = mkSelector "initWithTargetedImageURL:options:"

-- | @Selector@ for @initWithTargetedImageURL:options:completionHandler:@
initWithTargetedImageURL_options_completionHandlerSelector :: Selector
initWithTargetedImageURL_options_completionHandlerSelector = mkSelector "initWithTargetedImageURL:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageURL:orientation:options:@
initWithTargetedImageURL_orientation_optionsSelector :: Selector
initWithTargetedImageURL_orientation_optionsSelector = mkSelector "initWithTargetedImageURL:orientation:options:"

-- | @Selector@ for @initWithTargetedImageURL:orientation:options:completionHandler:@
initWithTargetedImageURL_orientation_options_completionHandlerSelector :: Selector
initWithTargetedImageURL_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedImageURL:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageData:options:@
initWithTargetedImageData_optionsSelector :: Selector
initWithTargetedImageData_optionsSelector = mkSelector "initWithTargetedImageData:options:"

-- | @Selector@ for @initWithTargetedImageData:options:completionHandler:@
initWithTargetedImageData_options_completionHandlerSelector :: Selector
initWithTargetedImageData_options_completionHandlerSelector = mkSelector "initWithTargetedImageData:options:completionHandler:"

-- | @Selector@ for @initWithTargetedImageData:orientation:options:@
initWithTargetedImageData_orientation_optionsSelector :: Selector
initWithTargetedImageData_orientation_optionsSelector = mkSelector "initWithTargetedImageData:orientation:options:"

-- | @Selector@ for @initWithTargetedImageData:orientation:options:completionHandler:@
initWithTargetedImageData_orientation_options_completionHandlerSelector :: Selector
initWithTargetedImageData_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedImageData:orientation:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:options:@
initWithTargetedCMSampleBuffer_optionsSelector :: Selector
initWithTargetedCMSampleBuffer_optionsSelector = mkSelector "initWithTargetedCMSampleBuffer:options:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:options:completionHandler:@
initWithTargetedCMSampleBuffer_options_completionHandlerSelector :: Selector
initWithTargetedCMSampleBuffer_options_completionHandlerSelector = mkSelector "initWithTargetedCMSampleBuffer:options:completionHandler:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:orientation:options:@
initWithTargetedCMSampleBuffer_orientation_optionsSelector :: Selector
initWithTargetedCMSampleBuffer_orientation_optionsSelector = mkSelector "initWithTargetedCMSampleBuffer:orientation:options:"

-- | @Selector@ for @initWithTargetedCMSampleBuffer:orientation:options:completionHandler:@
initWithTargetedCMSampleBuffer_orientation_options_completionHandlerSelector :: Selector
initWithTargetedCMSampleBuffer_orientation_options_completionHandlerSelector = mkSelector "initWithTargetedCMSampleBuffer:orientation:options:completionHandler:"

