{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCapturePhotoBracketSettings
--
-- A concrete subclass of AVCapturePhotoSettings that describes a bracketed capture.
--
-- In addition to the properties expressed in the base class, an AVCapturePhotoBracketSettings contains an array of AVCaptureBracketedStillImageSettings objects, where each describes one individual photo in the bracket. bracketedSettings.count must be <= AVCapturePhotoOutput's -maxBracketedCapturePhotoCount. Capturing a photo bracket may require the allocation of additional resources.
--
-- When you request a bracketed capture, your AVCapturePhotoCaptureDelegate's -captureOutput:didFinishProcessing{Photo | RawPhoto}... callbacks are called back bracketSettings.count times and provided with the corresponding AVCaptureBracketedStillImageSettings object from your request.
--
-- Generated bindings for @AVCapturePhotoBracketSettings@.
module ObjC.AVFoundation.AVCapturePhotoBracketSettings
  ( AVCapturePhotoBracketSettings
  , IsAVCapturePhotoBracketSettings(..)
  , photoBracketSettingsWithRawPixelFormatType_processedFormat_bracketedSettings
  , photoBracketSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType_bracketedSettings
  , bracketedSettings
  , lensStabilizationEnabled
  , setLensStabilizationEnabled
  , photoBracketSettingsWithRawPixelFormatType_processedFormat_bracketedSettingsSelector
  , photoBracketSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType_bracketedSettingsSelector
  , bracketedSettingsSelector
  , lensStabilizationEnabledSelector
  , setLensStabilizationEnabledSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | photoBracketSettingsWithRawPixelFormatType:processedFormat:bracketedSettings:
--
-- Creates an instance of AVCapturePhotoBracketSettings.
--
-- @rawPixelFormatType@ — One of the OSTypes contained in AVCapturePhotoOutput's -availableRawPhotoPixelFormatTypes array. May be set to 0 if you do not desire RAW capture.
--
-- @processedFormat@ — A dictionary of Core Video pixel buffer attributes or AVVideoSettings, analogous to AVCaptureStillImageOutput's outputSettings property. If you wish an uncompressed format, your dictionary must contain kCVPixelBufferPixelFormatTypeKey, and the format specified must be present in AVCapturePhotoOutput's -availablePhotoPixelFormatTypes array. kCVPixelBufferPixelFormatTypeKey is the only supported key when expressing uncompressed output. If you wish a compressed format, your dictionary must contain AVVideoCodecKey and the codec specified must be present in AVCapturePhotoOutput's -availablePhotoCodecTypes array. If you are specifying a compressed format, the AVVideoCompressionPropertiesKey is also supported, with a payload dictionary containing a single AVVideoQualityKey. If you only wish to capture RAW, you may pass a non-zero rawPixelFormatType and a nil processedFormat dictionary. If you pass a rawPixelFormatType of 0 AND a nil processedFormat dictionary, the default output of AVVideoCodecTypeJPEG will be delivered.
--
-- @bracketedSettings@ — An array of AVCaptureBracketedStillImageSettings objects (defined in AVCaptureStillImageOutput.h). All must be of the same type, either AVCaptureManualExposureBracketedStillImageSettings or AVCaptureAutoExposureBracketedStillImageSettings. bracketedSettings.count must be <= AVCapturePhotoOutput's -maxBracketedCapturePhotoCount.
--
-- Returns: An instance of AVCapturePhotoBracketSettings.
--
-- An NSInvalidArgumentException is thrown if bracketedSettings is nil, contains zero elements, or mixes and matches different subclasses of AVCaptureBracketedStillImageSettings.
--
-- AVCapturePhotoBracketSettings do not support flashMode, autoStillImageStabilizationEnabled, livePhotoMovieFileURL or livePhotoMovieMetadata.
--
-- ObjC selector: @+ photoBracketSettingsWithRawPixelFormatType:processedFormat:bracketedSettings:@
photoBracketSettingsWithRawPixelFormatType_processedFormat_bracketedSettings :: (IsNSDictionary processedFormat, IsNSArray bracketedSettings) => CUInt -> processedFormat -> bracketedSettings -> IO (Id AVCapturePhotoBracketSettings)
photoBracketSettingsWithRawPixelFormatType_processedFormat_bracketedSettings rawPixelFormatType processedFormat bracketedSettings =
  do
    cls' <- getRequiredClass "AVCapturePhotoBracketSettings"
    withObjCPtr processedFormat $ \raw_processedFormat ->
      withObjCPtr bracketedSettings $ \raw_bracketedSettings ->
        sendClassMsg cls' (mkSelector "photoBracketSettingsWithRawPixelFormatType:processedFormat:bracketedSettings:") (retPtr retVoid) [argCUInt (fromIntegral rawPixelFormatType), argPtr (castPtr raw_processedFormat :: Ptr ()), argPtr (castPtr raw_bracketedSettings :: Ptr ())] >>= retainedObject . castPtr

-- | photoBracketSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:bracketedSettings:
--
-- Creates an instance of AVCapturePhotoBracketSettings.
--
-- @rawPixelFormatType@ — One of the OSTypes contained in AVCapturePhotoOutput's -availableRawPhotoPixelFormatTypes array. May be set to 0 if you do not desire RAW capture.
--
-- @rawFileType@ — The file container for which the RAW image should be formatted to be written. Pass nil if you have no preferred file container. A default container will be chosen for you.
--
-- @processedFormat@ — A dictionary of Core Video pixel buffer attributes or AVVideoSettings, analogous to AVCaptureStillImageOutput's outputSettings property. If you wish an uncompressed format, your dictionary must contain kCVPixelBufferPixelFormatTypeKey, and the format specified must be present in AVCapturePhotoOutput's -availablePhotoPixelFormatTypes array. kCVPixelBufferPixelFormatTypeKey is the only supported key when expressing uncompressed output. If you wish a compressed format, your dictionary must contain AVVideoCodecKey and the codec specified must be present in AVCapturePhotoOutput's -availablePhotoCodecTypes array. If you are specifying a compressed format, the AVVideoCompressionPropertiesKey is also supported, with a payload dictionary containing a single AVVideoQualityKey. If you only wish to capture RAW, you may pass a non-zero rawPixelFormatType and a nil processedFormat dictionary. If you pass a rawPixelFormatType of 0 AND a nil processedFormat dictionary, the default output of AVVideoCodecTypeJPEG will be delivered.
--
-- @processedFileType@ — The file container for which the processed image should be formatted to be written. Pass nil if you have no preferred file container. A default container will be chosen for you.
--
-- @bracketedSettings@ — An array of AVCaptureBracketedStillImageSettings objects (defined in AVCaptureStillImageOutput.h). All must be of the same type, either AVCaptureManualExposureBracketedStillImageSettings or AVCaptureAutoExposureBracketedStillImageSettings. bracketedSettings.count must be <= AVCapturePhotoOutput's -maxBracketedCapturePhotoCount.
--
-- Returns: An instance of AVCapturePhotoBracketSettings.
--
-- An NSInvalidArgumentException is thrown if bracketedSettings is nil, contains zero elements, or mixes and matches different subclasses of AVCaptureBracketedStillImageSettings.
--
-- AVCapturePhotoBracketSettings do not support flashMode, autoStillImageStabilizationEnabled, livePhotoMovieFileURL or livePhotoMovieMetadata.
--
-- ObjC selector: @+ photoBracketSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:bracketedSettings:@
photoBracketSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType_bracketedSettings :: (IsNSString rawFileType, IsNSDictionary processedFormat, IsNSString processedFileType, IsNSArray bracketedSettings) => CUInt -> rawFileType -> processedFormat -> processedFileType -> bracketedSettings -> IO (Id AVCapturePhotoBracketSettings)
photoBracketSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType_bracketedSettings rawPixelFormatType rawFileType processedFormat processedFileType bracketedSettings =
  do
    cls' <- getRequiredClass "AVCapturePhotoBracketSettings"
    withObjCPtr rawFileType $ \raw_rawFileType ->
      withObjCPtr processedFormat $ \raw_processedFormat ->
        withObjCPtr processedFileType $ \raw_processedFileType ->
          withObjCPtr bracketedSettings $ \raw_bracketedSettings ->
            sendClassMsg cls' (mkSelector "photoBracketSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:bracketedSettings:") (retPtr retVoid) [argCUInt (fromIntegral rawPixelFormatType), argPtr (castPtr raw_rawFileType :: Ptr ()), argPtr (castPtr raw_processedFormat :: Ptr ()), argPtr (castPtr raw_processedFileType :: Ptr ()), argPtr (castPtr raw_bracketedSettings :: Ptr ())] >>= retainedObject . castPtr

-- | bracketedSettings
--
-- An array of AVCaptureBracketedStillImageSettings objects you passed in -initWithFormat:rawPixelFormatType:bracketedSettings:
--
-- This read-only property never returns nil.
--
-- ObjC selector: @- bracketedSettings@
bracketedSettings :: IsAVCapturePhotoBracketSettings avCapturePhotoBracketSettings => avCapturePhotoBracketSettings -> IO (Id NSArray)
bracketedSettings avCapturePhotoBracketSettings  =
  sendMsg avCapturePhotoBracketSettings (mkSelector "bracketedSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lensStabilizationEnabled
--
-- Specifies whether lens (optical) stabilization should be employed during the bracketed capture.
--
-- Default value is NO. This property may only be set to YES if AVCapturePhotoOutput's isLensStabilizationDuringBracketedCaptureSupported is YES. When set to YES, AVCapturePhotoOutput holds the lens steady for the duration of the bracket to counter hand shake and produce a sharper bracket of images.
--
-- ObjC selector: @- lensStabilizationEnabled@
lensStabilizationEnabled :: IsAVCapturePhotoBracketSettings avCapturePhotoBracketSettings => avCapturePhotoBracketSettings -> IO Bool
lensStabilizationEnabled avCapturePhotoBracketSettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCapturePhotoBracketSettings (mkSelector "lensStabilizationEnabled") retCULong []

-- | lensStabilizationEnabled
--
-- Specifies whether lens (optical) stabilization should be employed during the bracketed capture.
--
-- Default value is NO. This property may only be set to YES if AVCapturePhotoOutput's isLensStabilizationDuringBracketedCaptureSupported is YES. When set to YES, AVCapturePhotoOutput holds the lens steady for the duration of the bracket to counter hand shake and produce a sharper bracket of images.
--
-- ObjC selector: @- setLensStabilizationEnabled:@
setLensStabilizationEnabled :: IsAVCapturePhotoBracketSettings avCapturePhotoBracketSettings => avCapturePhotoBracketSettings -> Bool -> IO ()
setLensStabilizationEnabled avCapturePhotoBracketSettings  value =
  sendMsg avCapturePhotoBracketSettings (mkSelector "setLensStabilizationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @photoBracketSettingsWithRawPixelFormatType:processedFormat:bracketedSettings:@
photoBracketSettingsWithRawPixelFormatType_processedFormat_bracketedSettingsSelector :: Selector
photoBracketSettingsWithRawPixelFormatType_processedFormat_bracketedSettingsSelector = mkSelector "photoBracketSettingsWithRawPixelFormatType:processedFormat:bracketedSettings:"

-- | @Selector@ for @photoBracketSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:bracketedSettings:@
photoBracketSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType_bracketedSettingsSelector :: Selector
photoBracketSettingsWithRawPixelFormatType_rawFileType_processedFormat_processedFileType_bracketedSettingsSelector = mkSelector "photoBracketSettingsWithRawPixelFormatType:rawFileType:processedFormat:processedFileType:bracketedSettings:"

-- | @Selector@ for @bracketedSettings@
bracketedSettingsSelector :: Selector
bracketedSettingsSelector = mkSelector "bracketedSettings"

-- | @Selector@ for @lensStabilizationEnabled@
lensStabilizationEnabledSelector :: Selector
lensStabilizationEnabledSelector = mkSelector "lensStabilizationEnabled"

-- | @Selector@ for @setLensStabilizationEnabled:@
setLensStabilizationEnabledSelector :: Selector
setLensStabilizationEnabledSelector = mkSelector "setLensStabilizationEnabled:"

