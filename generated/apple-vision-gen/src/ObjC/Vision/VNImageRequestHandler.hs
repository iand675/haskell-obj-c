{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Performs requests on a single image.
--
-- The VNImageRequestHandler is created with an image that is used to be used for the requests a client might want to schedule. The VNImageRequestHandler retains, but never modifies, the image source for its entire lifetime. The client also must not modify the content of the image source once the VNImageRequestHandler is created otherwise the results are undefined.             The VNImageRequestHandler can choose to also cache intermediate representation of the image or other request-specific information for the purposes of runtime performance.
--
-- Generated bindings for @VNImageRequestHandler@.
module ObjC.Vision.VNImageRequestHandler
  ( VNImageRequestHandler
  , IsVNImageRequestHandler(..)
  , init_
  , initWithCVPixelBuffer_options
  , initWithCVPixelBuffer_orientation_options
  , initWithCVPixelBuffer_depthData_orientation_options
  , initWithCGImage_options
  , initWithCGImage_orientation_options
  , initWithCIImage_options
  , initWithCIImage_orientation_options
  , initWithURL_options
  , initWithURL_orientation_options
  , initWithData_options
  , initWithData_orientation_options
  , initWithCMSampleBuffer_options
  , initWithCMSampleBuffer_orientation_options
  , initWithCMSampleBuffer_depthData_orientation_options
  , performRequests_error
  , initSelector
  , initWithCGImage_optionsSelector
  , initWithCGImage_orientation_optionsSelector
  , initWithCIImage_optionsSelector
  , initWithCIImage_orientation_optionsSelector
  , initWithCMSampleBuffer_depthData_orientation_optionsSelector
  , initWithCMSampleBuffer_optionsSelector
  , initWithCMSampleBuffer_orientation_optionsSelector
  , initWithCVPixelBuffer_depthData_orientation_optionsSelector
  , initWithCVPixelBuffer_optionsSelector
  , initWithCVPixelBuffer_orientation_optionsSelector
  , initWithData_optionsSelector
  , initWithData_orientation_optionsSelector
  , initWithURL_optionsSelector
  , initWithURL_orientation_optionsSelector
  , performRequests_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNImageRequestHandler vnImageRequestHandler => vnImageRequestHandler -> IO (Id VNImageRequestHandler)
init_ vnImageRequestHandler =
  sendOwnedMessage vnImageRequestHandler initSelector

-- | initWithCVPixelBuffer:options creates a VNImageRequestHandler to be used for performing requests against the image passed in as buffer.
--
-- @pixelBuffer@ — A CVPixelBuffer containing the image to be used for performing the requests. The content of the buffer cannot be modified for the lifetime of the VNImageRequestHandler.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- ObjC selector: @- initWithCVPixelBuffer:options:@
initWithCVPixelBuffer_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> options -> IO (Id VNImageRequestHandler)
initWithCVPixelBuffer_options vnImageRequestHandler pixelBuffer options =
  sendOwnedMessage vnImageRequestHandler initWithCVPixelBuffer_optionsSelector pixelBuffer (toNSDictionary options)

-- | initWithCVPixelBuffer:options creates a VNImageRequestHandler to be used for performing requests against the image passed in as buffer.
--
-- @pixelBuffer@ — A CVPixelBuffer containing the image to be used for performing the requests. The content of the buffer cannot be modified for the lifetime of the VNImageRequestHandler.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- ObjC selector: @- initWithCVPixelBuffer:orientation:options:@
initWithCVPixelBuffer_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithCVPixelBuffer_orientation_options vnImageRequestHandler pixelBuffer orientation options =
  sendOwnedMessage vnImageRequestHandler initWithCVPixelBuffer_orientation_optionsSelector pixelBuffer orientation (toNSDictionary options)

-- | initWithCVPixelBuffer:depthData:orientation:options creates a VNImageRequestHandler to be used for performing requests against the image passed in as buffer with depth information.
--
-- @pixelBuffer@ — A CVPixelBuffer containing the image to be used for performing the requests. The content of the buffer cannot be modified for the lifetime of the VNImageRequestHandler.
--
-- @depthData@ — An AVDepthData instance associated with the pixelBuffer
--
-- @orientation@ — The orientation of the image and depth buffers based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information and should match for both buffers.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image
--
-- ObjC selector: @- initWithCVPixelBuffer:depthData:orientation:options:@
initWithCVPixelBuffer_depthData_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsAVDepthData depthData, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> depthData -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithCVPixelBuffer_depthData_orientation_options vnImageRequestHandler pixelBuffer depthData orientation options =
  sendOwnedMessage vnImageRequestHandler initWithCVPixelBuffer_depthData_orientation_optionsSelector pixelBuffer (toAVDepthData depthData) orientation (toNSDictionary options)

-- | initWithCGImage:options creates a VNImageRequestHandler to be used for performing requests against the image passed in as a CGImageRef.
--
-- @image@ — A CGImageRef containing the image to be used for performing the requests. The content of the image cannot be modified.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- ObjC selector: @- initWithCGImage:options:@
initWithCGImage_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> options -> IO (Id VNImageRequestHandler)
initWithCGImage_options vnImageRequestHandler image options =
  sendOwnedMessage vnImageRequestHandler initWithCGImage_optionsSelector image (toNSDictionary options)

-- | initWithCGImage:options creates a VNImageRequestHandler to be used for performing requests against the image passed in as a CGImageRef.
--
-- @image@ — A CGImageRef containing the image to be used for performing the requests. The content of the image cannot be modified.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- ObjC selector: @- initWithCGImage:orientation:options:@
initWithCGImage_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithCGImage_orientation_options vnImageRequestHandler image orientation options =
  sendOwnedMessage vnImageRequestHandler initWithCGImage_orientation_optionsSelector image orientation (toNSDictionary options)

-- | initWithCIImage:options creates a VNImageRequestHandler to be used for performing requests against the image passed in as a CIImage.
--
-- @image@ — A CIImage containing the image to be used for performing the requests. The content of the image cannot be modified.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: :  Request results may not be accurate in simulator due to CI's inability to render certain pixel formats in the simulator. The orientation of the original image should be applied for instance by using imageByApplyingOrientation or use the initWithCIImage:options:orientation API.
--
-- ObjC selector: @- initWithCIImage:options:@
initWithCIImage_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsCIImage image, IsNSDictionary options) => vnImageRequestHandler -> image -> options -> IO (Id VNImageRequestHandler)
initWithCIImage_options vnImageRequestHandler image options =
  sendOwnedMessage vnImageRequestHandler initWithCIImage_optionsSelector (toCIImage image) (toNSDictionary options)

-- | initWithCIImage:options:orientation creates a VNImageRequestHandler to be used for performing requests against the image passed in as a CIImage.
--
-- @image@ — A CIImage containing the image to be used for performing the requests. The content of the image cannot be modified.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: :  Request results may not be accurate in simulator due to CI's inability to render certain pixel formats in the simulator
--
-- ObjC selector: @- initWithCIImage:orientation:options:@
initWithCIImage_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsCIImage image, IsNSDictionary options) => vnImageRequestHandler -> image -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithCIImage_orientation_options vnImageRequestHandler image orientation options =
  sendOwnedMessage vnImageRequestHandler initWithCIImage_orientation_optionsSelector (toCIImage image) orientation (toNSDictionary options)

-- | initWithURL:options creates a VNImageRequestHandler to be used for performing requests against an image specified by it's URL
--
-- @imageURL@ — A URL pointing at an image to be used for performing the requests. The image has to be in a format that is supported by ImageIO. The content of the image cannot be modified.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: :  Request results may not be accurate in simulator due to CI's inability to render certain pixel formats in the simulator
--
-- ObjC selector: @- initWithURL:options:@
initWithURL_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSURL imageURL, IsNSDictionary options) => vnImageRequestHandler -> imageURL -> options -> IO (Id VNImageRequestHandler)
initWithURL_options vnImageRequestHandler imageURL options =
  sendOwnedMessage vnImageRequestHandler initWithURL_optionsSelector (toNSURL imageURL) (toNSDictionary options)

-- | initWithURL:options creates a VNImageRequestHandler to be used for performing requests against an image specified by it's URL
--
-- @imageURL@ — A URL pointing at an image to be used for performing the requests. The image has to be in a format that is supported by ImageIO. The content of the image cannot be modified.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: :  Request results may not be accurate in simulator due to CI's inability to render certain pixel formats in the simulator
--
-- ObjC selector: @- initWithURL:orientation:options:@
initWithURL_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSURL imageURL, IsNSDictionary options) => vnImageRequestHandler -> imageURL -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithURL_orientation_options vnImageRequestHandler imageURL orientation options =
  sendOwnedMessage vnImageRequestHandler initWithURL_orientation_optionsSelector (toNSURL imageURL) orientation (toNSDictionary options)

-- | initWithData:options creates a VNImageRequestHandler to be used for performing requests against an image contained in an NSData object.
--
-- @imageData@ — An NSData object containing the content of the image to be used for performing the requests. See CIImage imageWithData for supported format. The content of the image cannot be modified.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: :  Request results may not be accurate in simulator due to CI's inability to render certain pixel formats in the simulator
--
-- ObjC selector: @- initWithData:options:@
initWithData_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSData imageData, IsNSDictionary options) => vnImageRequestHandler -> imageData -> options -> IO (Id VNImageRequestHandler)
initWithData_options vnImageRequestHandler imageData options =
  sendOwnedMessage vnImageRequestHandler initWithData_optionsSelector (toNSData imageData) (toNSDictionary options)

-- | initWithData:options creates a VNImageRequestHandler to be used for performing requests against an image contained in an NSData object.
--
-- @imageData@ — An NSData object containing the content of the image to be used for performing the requests. See CIImage imageWithData for supported format. The content of the image cannot be modified.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: :  Request results may not be accurate in simulator due to CI's inability to render certain pixel formats in the simulator
--
-- ObjC selector: @- initWithData:orientation:options:@
initWithData_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSData imageData, IsNSDictionary options) => vnImageRequestHandler -> imageData -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithData_orientation_options vnImageRequestHandler imageData orientation options =
  sendOwnedMessage vnImageRequestHandler initWithData_orientation_optionsSelector (toNSData imageData) orientation (toNSDictionary options)

-- | Creates a VNImageRequestHandler to be used for performing requests against the image buffer contained in the CMSampleBufferRef
--
-- @sampleBuffer@ — A CMSampleBuffer containing the imageBuffer that will be used for performing the requests. Not all types of sample buffers are supported. They need to contain a CVImageBuffer, be valid and ready.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: CMSampleBuffers can contain metadata like camera intrinsics that will be used by algorithms supporting it unless overwritten by the options.
--
-- ObjC selector: @- initWithCMSampleBuffer:options:@
initWithCMSampleBuffer_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> options -> IO (Id VNImageRequestHandler)
initWithCMSampleBuffer_options vnImageRequestHandler sampleBuffer options =
  sendOwnedMessage vnImageRequestHandler initWithCMSampleBuffer_optionsSelector sampleBuffer (toNSDictionary options)

-- | Creates a VNImageRequestHandler to be used for performing requests against the image buffer contained in the CMSampleBufferRef
--
-- @sampleBuffer@ — A CMSampleBuffer containing the imageBuffer that will be used for performing the requests. Not all types of sample buffers are supported. They need to contain a CVImageBuffer, be valid and ready.
--
-- @orientation@ — The orientation of the image/buffer based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image like VNImageOptionCameraIntrinsics
--
-- Note: CMSampleBuffers can contain metadata like camera intrinsics that will be used by algorithms supporting it unless overwritten by the options.
--
-- Note: :  Because CoreImage is unable to render certain pixel formats in the iOS simulator, request results may not be accurate in those cases.
--
-- ObjC selector: @- initWithCMSampleBuffer:orientation:options:@
initWithCMSampleBuffer_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithCMSampleBuffer_orientation_options vnImageRequestHandler sampleBuffer orientation options =
  sendOwnedMessage vnImageRequestHandler initWithCMSampleBuffer_orientation_optionsSelector sampleBuffer orientation (toNSDictionary options)

-- | Creates a VNImageRequestHandler to be used for performing requests against the image buffer contained in the CMSampleBufferRef
--
-- @sampleBuffer@ — A CMSampleBuffer containing the imageBuffer that will be used for performing the requests. Not all types of sample buffers are supported. They need to contain a CVImageBuffer, be valid and ready.
--
-- @depthData@ — An AVDepthData instance associated with the pixelBuffer
--
-- @orientation@ — The orientation of the image and depth buffers based on the EXIF specification. For details see kCGImagePropertyOrientation. The value has to be an integer from 1 to 8. This supersedes every other orientation information and should match for both buffers.
--
-- @options@ — A dictionary with options specifying auxiliary information for the buffer/image
--
-- Note: CMSampleBuffers can contain metadata like camera intrinsics that will be used by algorithms supporting it unless overwritten by the options.
--
-- Note: :  Because CoreImage is unable to render certain pixel formats in the iOS simulator, request results may not be accurate in those cases.
--
-- ObjC selector: @- initWithCMSampleBuffer:depthData:orientation:options:@
initWithCMSampleBuffer_depthData_orientation_options :: (IsVNImageRequestHandler vnImageRequestHandler, IsAVDepthData depthData, IsNSDictionary options) => vnImageRequestHandler -> Ptr () -> depthData -> CInt -> options -> IO (Id VNImageRequestHandler)
initWithCMSampleBuffer_depthData_orientation_options vnImageRequestHandler sampleBuffer depthData orientation options =
  sendOwnedMessage vnImageRequestHandler initWithCMSampleBuffer_depthData_orientation_optionsSelector sampleBuffer (toAVDepthData depthData) orientation (toNSDictionary options)

-- | performRequests schedules one or more VNRequests to be performed. The function returns once all requests have been finished.
--
-- The results of the VNRequests as well any possible errors of the individual requests are reported in the VNRequests results and error properties.
--
-- @requests@ — An NSArray of VNRequests that are to be performed.
--
-- @error@ — Returns an error that happened during scheduling of the requests. Check individual requests results and errors for their respective success and failures. This parameter is optional.
--
-- Returns: Returns true if all requests were scheduled and performed. Check individual requests results and errors for their respective success and failures.
--
-- ObjC selector: @- performRequests:error:@
performRequests_error :: (IsVNImageRequestHandler vnImageRequestHandler, IsNSArray requests, IsNSError error_) => vnImageRequestHandler -> requests -> error_ -> IO Bool
performRequests_error vnImageRequestHandler requests error_ =
  sendMessage vnImageRequestHandler performRequests_errorSelector (toNSArray requests) (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNImageRequestHandler)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCVPixelBuffer:options:@
initWithCVPixelBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id VNImageRequestHandler)
initWithCVPixelBuffer_optionsSelector = mkSelector "initWithCVPixelBuffer:options:"

-- | @Selector@ for @initWithCVPixelBuffer:orientation:options:@
initWithCVPixelBuffer_orientation_optionsSelector :: Selector '[Ptr (), CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithCVPixelBuffer_orientation_optionsSelector = mkSelector "initWithCVPixelBuffer:orientation:options:"

-- | @Selector@ for @initWithCVPixelBuffer:depthData:orientation:options:@
initWithCVPixelBuffer_depthData_orientation_optionsSelector :: Selector '[Ptr (), Id AVDepthData, CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithCVPixelBuffer_depthData_orientation_optionsSelector = mkSelector "initWithCVPixelBuffer:depthData:orientation:options:"

-- | @Selector@ for @initWithCGImage:options:@
initWithCGImage_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id VNImageRequestHandler)
initWithCGImage_optionsSelector = mkSelector "initWithCGImage:options:"

-- | @Selector@ for @initWithCGImage:orientation:options:@
initWithCGImage_orientation_optionsSelector :: Selector '[Ptr (), CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithCGImage_orientation_optionsSelector = mkSelector "initWithCGImage:orientation:options:"

-- | @Selector@ for @initWithCIImage:options:@
initWithCIImage_optionsSelector :: Selector '[Id CIImage, Id NSDictionary] (Id VNImageRequestHandler)
initWithCIImage_optionsSelector = mkSelector "initWithCIImage:options:"

-- | @Selector@ for @initWithCIImage:orientation:options:@
initWithCIImage_orientation_optionsSelector :: Selector '[Id CIImage, CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithCIImage_orientation_optionsSelector = mkSelector "initWithCIImage:orientation:options:"

-- | @Selector@ for @initWithURL:options:@
initWithURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id VNImageRequestHandler)
initWithURL_optionsSelector = mkSelector "initWithURL:options:"

-- | @Selector@ for @initWithURL:orientation:options:@
initWithURL_orientation_optionsSelector :: Selector '[Id NSURL, CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithURL_orientation_optionsSelector = mkSelector "initWithURL:orientation:options:"

-- | @Selector@ for @initWithData:options:@
initWithData_optionsSelector :: Selector '[Id NSData, Id NSDictionary] (Id VNImageRequestHandler)
initWithData_optionsSelector = mkSelector "initWithData:options:"

-- | @Selector@ for @initWithData:orientation:options:@
initWithData_orientation_optionsSelector :: Selector '[Id NSData, CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithData_orientation_optionsSelector = mkSelector "initWithData:orientation:options:"

-- | @Selector@ for @initWithCMSampleBuffer:options:@
initWithCMSampleBuffer_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id VNImageRequestHandler)
initWithCMSampleBuffer_optionsSelector = mkSelector "initWithCMSampleBuffer:options:"

-- | @Selector@ for @initWithCMSampleBuffer:orientation:options:@
initWithCMSampleBuffer_orientation_optionsSelector :: Selector '[Ptr (), CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithCMSampleBuffer_orientation_optionsSelector = mkSelector "initWithCMSampleBuffer:orientation:options:"

-- | @Selector@ for @initWithCMSampleBuffer:depthData:orientation:options:@
initWithCMSampleBuffer_depthData_orientation_optionsSelector :: Selector '[Ptr (), Id AVDepthData, CInt, Id NSDictionary] (Id VNImageRequestHandler)
initWithCMSampleBuffer_depthData_orientation_optionsSelector = mkSelector "initWithCMSampleBuffer:depthData:orientation:options:"

-- | @Selector@ for @performRequests:error:@
performRequests_errorSelector :: Selector '[Id NSArray, Id NSError] Bool
performRequests_errorSelector = mkSelector "performRequests:error:"

