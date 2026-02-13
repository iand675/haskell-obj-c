{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Performs requests on a sequence of images.
--
-- The VNSequenceRequestHandler is created without any specific image source.  The -performRequests:on<ImageSource>:error: methods will retain the image source for no longer than the lifetime of the call.             The VNSequenceRequestHandler can choose to also cache state information related to the previously-processed image sources.
--
-- Generated bindings for @VNSequenceRequestHandler@.
module ObjC.Vision.VNSequenceRequestHandler
  ( VNSequenceRequestHandler
  , IsVNSequenceRequestHandler(..)
  , init_
  , performRequests_onCVPixelBuffer_error
  , performRequests_onCVPixelBuffer_orientation_error
  , performRequests_onCGImage_error
  , performRequests_onCGImage_orientation_error
  , performRequests_onCIImage_error
  , performRequests_onCIImage_orientation_error
  , performRequests_onImageURL_error
  , performRequests_onImageURL_orientation_error
  , performRequests_onImageData_error
  , performRequests_onImageData_orientation_error
  , performRequests_onCMSampleBuffer_error
  , performRequests_onCMSampleBuffer_orientation_error
  , initSelector
  , performRequests_onCGImage_errorSelector
  , performRequests_onCGImage_orientation_errorSelector
  , performRequests_onCIImage_errorSelector
  , performRequests_onCIImage_orientation_errorSelector
  , performRequests_onCMSampleBuffer_errorSelector
  , performRequests_onCMSampleBuffer_orientation_errorSelector
  , performRequests_onCVPixelBuffer_errorSelector
  , performRequests_onCVPixelBuffer_orientation_errorSelector
  , performRequests_onImageData_errorSelector
  , performRequests_onImageData_orientation_errorSelector
  , performRequests_onImageURL_errorSelector
  , performRequests_onImageURL_orientation_errorSelector


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

-- | Creates a new object.
--
-- ObjC selector: @- init@
init_ :: IsVNSequenceRequestHandler vnSequenceRequestHandler => vnSequenceRequestHandler -> IO (Id VNSequenceRequestHandler)
init_ vnSequenceRequestHandler =
  sendOwnedMessage vnSequenceRequestHandler initSelector

-- | Perform requests on an image in a CVPixelBuffer.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @pixelBuffer@ — The CVPixelBuffer containing the image to be processed.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCVPixelBuffer:error:@
performRequests_onCVPixelBuffer_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSError error_) => vnSequenceRequestHandler -> requests -> Ptr () -> error_ -> IO Bool
performRequests_onCVPixelBuffer_error vnSequenceRequestHandler requests pixelBuffer error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCVPixelBuffer_errorSelector (toNSArray requests) pixelBuffer (toNSError error_)

-- | Perform requests on an image in a CVPixelBuffer.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @pixelBuffer@ — The CVPixelBuffer containing the image to be processed.
--
-- @orientation@ — The orientation of the image as it is captured in the pixel buffer.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCVPixelBuffer:orientation:error:@
performRequests_onCVPixelBuffer_orientation_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSError error_) => vnSequenceRequestHandler -> requests -> Ptr () -> CInt -> error_ -> IO Bool
performRequests_onCVPixelBuffer_orientation_error vnSequenceRequestHandler requests pixelBuffer orientation error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCVPixelBuffer_orientation_errorSelector (toNSArray requests) pixelBuffer orientation (toNSError error_)

-- | Perform requests on an image in a CGImageRef.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @image@ — The CGImageRef containing the image to be processed.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCGImage:error:@
performRequests_onCGImage_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSError error_) => vnSequenceRequestHandler -> requests -> Ptr () -> error_ -> IO Bool
performRequests_onCGImage_error vnSequenceRequestHandler requests image error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCGImage_errorSelector (toNSArray requests) image (toNSError error_)

-- | Perform requests on an image in a CGImageRef.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @image@ — The CGImageRef containing the image to be processed.
--
-- @orientation@ — The orientation of the image.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCGImage:orientation:error:@
performRequests_onCGImage_orientation_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSError error_) => vnSequenceRequestHandler -> requests -> Ptr () -> CInt -> error_ -> IO Bool
performRequests_onCGImage_orientation_error vnSequenceRequestHandler requests image orientation error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCGImage_orientation_errorSelector (toNSArray requests) image orientation (toNSError error_)

-- | Perform requests on an image in a CIImage.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @image@ — The CIImage containing the image to be processed.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCIImage:error:@
performRequests_onCIImage_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsCIImage image, IsNSError error_) => vnSequenceRequestHandler -> requests -> image -> error_ -> IO Bool
performRequests_onCIImage_error vnSequenceRequestHandler requests image error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCIImage_errorSelector (toNSArray requests) (toCIImage image) (toNSError error_)

-- | Perform requests on an image in a CIImage.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @image@ — The CIImage containing the image to be processed.
--
-- @orientation@ — The orientation of the image.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCIImage:orientation:error:@
performRequests_onCIImage_orientation_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsCIImage image, IsNSError error_) => vnSequenceRequestHandler -> requests -> image -> CInt -> error_ -> IO Bool
performRequests_onCIImage_orientation_error vnSequenceRequestHandler requests image orientation error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCIImage_orientation_errorSelector (toNSArray requests) (toCIImage image) orientation (toNSError error_)

-- | Perform requests on an image referenced by an URL.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @imageURL@ — The URL of the image to be processed.  If this is not a file-based URL, the method will fail.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onImageURL:error:@
performRequests_onImageURL_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSURL imageURL, IsNSError error_) => vnSequenceRequestHandler -> requests -> imageURL -> error_ -> IO Bool
performRequests_onImageURL_error vnSequenceRequestHandler requests imageURL error_ =
  sendMessage vnSequenceRequestHandler performRequests_onImageURL_errorSelector (toNSArray requests) (toNSURL imageURL) (toNSError error_)

-- | Perform requests on an image referenced by an URL.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @imageURL@ — The URL of the image to be processed.  If this is not a file-based URL, the method will fail.
--
-- @orientation@ — The orientation of the image.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onImageURL:orientation:error:@
performRequests_onImageURL_orientation_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSURL imageURL, IsNSError error_) => vnSequenceRequestHandler -> requests -> imageURL -> CInt -> error_ -> IO Bool
performRequests_onImageURL_orientation_error vnSequenceRequestHandler requests imageURL orientation error_ =
  sendMessage vnSequenceRequestHandler performRequests_onImageURL_orientation_errorSelector (toNSArray requests) (toNSURL imageURL) orientation (toNSError error_)

-- | Perform requests on an image with its source format in memory.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @imageData@ — The data representing the source format of the image to be processed.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onImageData:error:@
performRequests_onImageData_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSData imageData, IsNSError error_) => vnSequenceRequestHandler -> requests -> imageData -> error_ -> IO Bool
performRequests_onImageData_error vnSequenceRequestHandler requests imageData error_ =
  sendMessage vnSequenceRequestHandler performRequests_onImageData_errorSelector (toNSArray requests) (toNSData imageData) (toNSError error_)

-- | Perform requests on an image with its source format in memory.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @imageData@ — The data representing the source format of the image to be processed.
--
-- @orientation@ — The orientation of the image.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onImageData:orientation:error:@
performRequests_onImageData_orientation_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSData imageData, IsNSError error_) => vnSequenceRequestHandler -> requests -> imageData -> CInt -> error_ -> IO Bool
performRequests_onImageData_orientation_error vnSequenceRequestHandler requests imageData orientation error_ =
  sendMessage vnSequenceRequestHandler performRequests_onImageData_orientation_errorSelector (toNSArray requests) (toNSData imageData) orientation (toNSError error_)

-- | Perform requests on the image buffer contained in the CMSampleBufferRef.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @sampleBuffer@ — A CMSampleBuffer containing an image that will be used for performing the requests. Not all types of sample buffers are supported. They need to contain a CVImageBuffer, be valid and ready.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCMSampleBuffer:error:@
performRequests_onCMSampleBuffer_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSError error_) => vnSequenceRequestHandler -> requests -> Ptr () -> error_ -> IO Bool
performRequests_onCMSampleBuffer_error vnSequenceRequestHandler requests sampleBuffer error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCMSampleBuffer_errorSelector (toNSArray requests) sampleBuffer (toNSError error_)

-- | Perform requests on the image buffer contained in the CMSampleBufferRef.
--
-- @requests@ — The VNRequests to be performed on the image.
--
-- @sampleBuffer@ — A CMSampleBuffer containing an image that will be used for performing the requests. Not all types of sample buffers are supported. They need to contain a CVImageBuffer, be valid and ready.
--
-- @orientation@ — The orientation of the image.
--
-- @error@ — On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify NULL for this parameter if you do not want the error information.
--
-- ObjC selector: @- performRequests:onCMSampleBuffer:orientation:error:@
performRequests_onCMSampleBuffer_orientation_error :: (IsVNSequenceRequestHandler vnSequenceRequestHandler, IsNSArray requests, IsNSError error_) => vnSequenceRequestHandler -> requests -> Ptr () -> CInt -> error_ -> IO Bool
performRequests_onCMSampleBuffer_orientation_error vnSequenceRequestHandler requests sampleBuffer orientation error_ =
  sendMessage vnSequenceRequestHandler performRequests_onCMSampleBuffer_orientation_errorSelector (toNSArray requests) sampleBuffer orientation (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNSequenceRequestHandler)
initSelector = mkSelector "init"

-- | @Selector@ for @performRequests:onCVPixelBuffer:error:@
performRequests_onCVPixelBuffer_errorSelector :: Selector '[Id NSArray, Ptr (), Id NSError] Bool
performRequests_onCVPixelBuffer_errorSelector = mkSelector "performRequests:onCVPixelBuffer:error:"

-- | @Selector@ for @performRequests:onCVPixelBuffer:orientation:error:@
performRequests_onCVPixelBuffer_orientation_errorSelector :: Selector '[Id NSArray, Ptr (), CInt, Id NSError] Bool
performRequests_onCVPixelBuffer_orientation_errorSelector = mkSelector "performRequests:onCVPixelBuffer:orientation:error:"

-- | @Selector@ for @performRequests:onCGImage:error:@
performRequests_onCGImage_errorSelector :: Selector '[Id NSArray, Ptr (), Id NSError] Bool
performRequests_onCGImage_errorSelector = mkSelector "performRequests:onCGImage:error:"

-- | @Selector@ for @performRequests:onCGImage:orientation:error:@
performRequests_onCGImage_orientation_errorSelector :: Selector '[Id NSArray, Ptr (), CInt, Id NSError] Bool
performRequests_onCGImage_orientation_errorSelector = mkSelector "performRequests:onCGImage:orientation:error:"

-- | @Selector@ for @performRequests:onCIImage:error:@
performRequests_onCIImage_errorSelector :: Selector '[Id NSArray, Id CIImage, Id NSError] Bool
performRequests_onCIImage_errorSelector = mkSelector "performRequests:onCIImage:error:"

-- | @Selector@ for @performRequests:onCIImage:orientation:error:@
performRequests_onCIImage_orientation_errorSelector :: Selector '[Id NSArray, Id CIImage, CInt, Id NSError] Bool
performRequests_onCIImage_orientation_errorSelector = mkSelector "performRequests:onCIImage:orientation:error:"

-- | @Selector@ for @performRequests:onImageURL:error:@
performRequests_onImageURL_errorSelector :: Selector '[Id NSArray, Id NSURL, Id NSError] Bool
performRequests_onImageURL_errorSelector = mkSelector "performRequests:onImageURL:error:"

-- | @Selector@ for @performRequests:onImageURL:orientation:error:@
performRequests_onImageURL_orientation_errorSelector :: Selector '[Id NSArray, Id NSURL, CInt, Id NSError] Bool
performRequests_onImageURL_orientation_errorSelector = mkSelector "performRequests:onImageURL:orientation:error:"

-- | @Selector@ for @performRequests:onImageData:error:@
performRequests_onImageData_errorSelector :: Selector '[Id NSArray, Id NSData, Id NSError] Bool
performRequests_onImageData_errorSelector = mkSelector "performRequests:onImageData:error:"

-- | @Selector@ for @performRequests:onImageData:orientation:error:@
performRequests_onImageData_orientation_errorSelector :: Selector '[Id NSArray, Id NSData, CInt, Id NSError] Bool
performRequests_onImageData_orientation_errorSelector = mkSelector "performRequests:onImageData:orientation:error:"

-- | @Selector@ for @performRequests:onCMSampleBuffer:error:@
performRequests_onCMSampleBuffer_errorSelector :: Selector '[Id NSArray, Ptr (), Id NSError] Bool
performRequests_onCMSampleBuffer_errorSelector = mkSelector "performRequests:onCMSampleBuffer:error:"

-- | @Selector@ for @performRequests:onCMSampleBuffer:orientation:error:@
performRequests_onCMSampleBuffer_orientation_errorSelector :: Selector '[Id NSArray, Ptr (), CInt, Id NSError] Bool
performRequests_onCMSampleBuffer_orientation_errorSelector = mkSelector "performRequests:onCMSampleBuffer:orientation:error:"

