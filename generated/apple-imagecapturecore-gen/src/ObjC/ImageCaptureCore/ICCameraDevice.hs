{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICCameraDevice
--
-- ICCameraDevice is a concrete subclass of ICDevice class. ICDeviceBrowser creates instances of this class.
--
-- Generated bindings for @ICCameraDevice@.
module ObjC.ImageCaptureCore.ICCameraDevice
  ( ICCameraDevice
  , IsICCameraDevice(..)
  , filesOfType
  , requestReadDataFromFile_atOffset_length_readDelegate_didReadDataSelector_contextInfo
  , requestDownloadFile_options_downloadDelegate_didDownloadSelector_contextInfo
  , cancelDownload
  , requestDeleteFiles
  , cancelDelete
  , requestSyncClock
  , requestUploadFile_options_uploadDelegate_didUploadSelector_contextInfo
  , requestTakePicture
  , requestEnableTethering
  , requestDisableTethering
  , requestSendPTPCommand_outData_sendCommandDelegate_didSendCommandSelector_contextInfo
  , requestSendPTPCommand_outData_completion
  , contentCatalogPercentCompleted
  , contents
  , mediaFiles
  , ejectable
  , locked
  , accessRestrictedAppleDevice
  , iCloudPhotosEnabled
  , mountPoint
  , mediaPresentation
  , setMediaPresentation
  , timeOffset
  , batteryLevelAvailable
  , batteryLevel
  , tetheredCaptureEnabled
  , ptpEventHandler
  , setPtpEventHandler
  , filesOfTypeSelector
  , requestReadDataFromFile_atOffset_length_readDelegate_didReadDataSelector_contextInfoSelector
  , requestDownloadFile_options_downloadDelegate_didDownloadSelector_contextInfoSelector
  , cancelDownloadSelector
  , requestDeleteFilesSelector
  , cancelDeleteSelector
  , requestSyncClockSelector
  , requestUploadFile_options_uploadDelegate_didUploadSelector_contextInfoSelector
  , requestTakePictureSelector
  , requestEnableTetheringSelector
  , requestDisableTetheringSelector
  , requestSendPTPCommand_outData_sendCommandDelegate_didSendCommandSelector_contextInfoSelector
  , requestSendPTPCommand_outData_completionSelector
  , contentCatalogPercentCompletedSelector
  , contentsSelector
  , mediaFilesSelector
  , ejectableSelector
  , lockedSelector
  , accessRestrictedAppleDeviceSelector
  , iCloudPhotosEnabledSelector
  , mountPointSelector
  , mediaPresentationSelector
  , setMediaPresentationSelector
  , timeOffsetSelector
  , batteryLevelAvailableSelector
  , batteryLevelSelector
  , tetheredCaptureEnabledSelector
  , ptpEventHandlerSelector
  , setPtpEventHandlerSelector

  -- * Enum types
  , ICMediaPresentation(ICMediaPresentation)
  , pattern ICMediaPresentationConvertedAssets
  , pattern ICMediaPresentationOriginalAssets

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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | filesOfType:
--
-- This method returns an array of files on the camera of type fileType.
--
-- The fileType string is one of the following Uniform Type Identifier strings: kUTTypeImage, kUTTypeMovie, kUTTypeAudio, or kUTTypeData.
--
-- ObjC selector: @- filesOfType:@
filesOfType :: (IsICCameraDevice icCameraDevice, IsNSString fileUTType) => icCameraDevice -> fileUTType -> IO (Id NSArray)
filesOfType icCameraDevice  fileUTType =
  withObjCPtr fileUTType $ \raw_fileUTType ->
      sendMsg icCameraDevice (mkSelector "filesOfType:") (retPtr retVoid) [argPtr (castPtr raw_fileUTType :: Ptr ())] >>= retainedObject . castPtr

-- | requestReadDataFromFile:atOffset:length:readDelegate:didReadDataSelector:contextInfo:
--
-- This method asynchronously reads data of a specified length from a specified offset.
--
-- The readDelegate passed must not be nil. When this request is completed, the didReadDataSelector of the readDelegate object is called. The didReadDataSelector should have the same signature as: - (void)didReadData:(NSData*)data fromFile:(ICCameraFile*)file error:(NSError*)error contextInfo:(void*)contextInfo. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestReadDataFromFile:atOffset:length:readDelegate:didReadDataSelector:contextInfo:@
requestReadDataFromFile_atOffset_length_readDelegate_didReadDataSelector_contextInfo :: (IsICCameraDevice icCameraDevice, IsICCameraFile file) => icCameraDevice -> file -> CLong -> CLong -> RawId -> Selector -> Ptr () -> IO ()
requestReadDataFromFile_atOffset_length_readDelegate_didReadDataSelector_contextInfo icCameraDevice  file offset length_ readDelegate selector contextInfo =
  withObjCPtr file $ \raw_file ->
      sendMsg icCameraDevice (mkSelector "requestReadDataFromFile:atOffset:length:readDelegate:didReadDataSelector:contextInfo:") retVoid [argPtr (castPtr raw_file :: Ptr ()), argCLong offset, argCLong length_, argPtr (castPtr (unRawId readDelegate) :: Ptr ()), argPtr (unSelector selector), argPtr contextInfo]

-- | requestDownloadFile:options:downloadDelegate:didDownloadSelector:contextInfo:
--
-- Download a file from the camera. Please refer to the top of this header for information about the options.
--
-- The downloadDelegate passed must not be nil. When this request is completed, the didDownloadSelector of the downloadDelegate object is called.The didDownloadSelector should have the same signature as: - (void)didDownloadFile:(ICCameraFile*)file error:(NSError*)error options:(NSDictionary*)options contextInfo:(void*)contextInfo. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestDownloadFile:options:downloadDelegate:didDownloadSelector:contextInfo:@
requestDownloadFile_options_downloadDelegate_didDownloadSelector_contextInfo :: (IsICCameraDevice icCameraDevice, IsICCameraFile file, IsNSDictionary options) => icCameraDevice -> file -> options -> RawId -> Selector -> Ptr () -> IO ()
requestDownloadFile_options_downloadDelegate_didDownloadSelector_contextInfo icCameraDevice  file options downloadDelegate selector contextInfo =
  withObjCPtr file $ \raw_file ->
    withObjCPtr options $ \raw_options ->
        sendMsg icCameraDevice (mkSelector "requestDownloadFile:options:downloadDelegate:didDownloadSelector:contextInfo:") retVoid [argPtr (castPtr raw_file :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr (unRawId downloadDelegate) :: Ptr ()), argPtr (unSelector selector), argPtr contextInfo]

-- | cancelDownload
--
-- Cancels the current download operation if supported
--
-- ObjC selector: @- cancelDownload@
cancelDownload :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO ()
cancelDownload icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "cancelDownload") retVoid []

-- | requestDeleteFiles
--
-- Deletes files.
--
-- ObjC selector: @- requestDeleteFiles:@
requestDeleteFiles :: (IsICCameraDevice icCameraDevice, IsNSArray files) => icCameraDevice -> files -> IO ()
requestDeleteFiles icCameraDevice  files =
  withObjCPtr files $ \raw_files ->
      sendMsg icCameraDevice (mkSelector "requestDeleteFiles:") retVoid [argPtr (castPtr raw_files :: Ptr ())]

-- | cancelDelete
--
-- Cancels the current delete operation started by sending a 'requestDeleteFiles:'. This will only cancel operations in flight when a batch of files have been requested for deletion.
--
-- ObjC selector: @- cancelDelete@
cancelDelete :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO ()
cancelDelete icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "cancelDelete") retVoid []

-- | requestSyncClock
--
-- Synchronize camera's clock with the computer's clock. You should send this request only if the camera has the 'ICCameraDeviceCanSyncClock' capability.
--
-- ObjC selector: @- requestSyncClock@
requestSyncClock :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO ()
requestSyncClock icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "requestSyncClock") retVoid []

-- | requestUploadFile:options:uploadDelegate:didUploadSelector:contextInfo:
--
-- Upload a file at fileURL to the camera. The options dictionary is not used in this version.
--
-- The uploadDelegate passed must not be nil. When this request is completed, the didUploadSelector of the uploadDelegate object is called. The didUploadSelector should have the same signature as: - (void)didUploadFile:(NSURL*)fileURL error:(NSError*)error contextInfo:(void*)contextInfo. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestUploadFile:options:uploadDelegate:didUploadSelector:contextInfo:@
requestUploadFile_options_uploadDelegate_didUploadSelector_contextInfo :: (IsICCameraDevice icCameraDevice, IsNSURL fileURL, IsNSDictionary options) => icCameraDevice -> fileURL -> options -> RawId -> Selector -> Ptr () -> IO ()
requestUploadFile_options_uploadDelegate_didUploadSelector_contextInfo icCameraDevice  fileURL options uploadDelegate selector contextInfo =
  withObjCPtr fileURL $ \raw_fileURL ->
    withObjCPtr options $ \raw_options ->
        sendMsg icCameraDevice (mkSelector "requestUploadFile:options:uploadDelegate:didUploadSelector:contextInfo:") retVoid [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr (unRawId uploadDelegate) :: Ptr ()), argPtr (unSelector selector), argPtr contextInfo]

-- | requestTakePicture
--
-- Capture a new image using the camera, the camera capabilities include 'ICCameraDeviceCanTakePicture'.
--
-- ObjC selector: @- requestTakePicture@
requestTakePicture :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO ()
requestTakePicture icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "requestTakePicture") retVoid []

-- | requestEnableTethering
--
-- Send this message to enable tethered capture on the camera device if the camera has the 'ICCameraDeviceCanTakePicture' capability.
--
-- ObjC selector: @- requestEnableTethering@
requestEnableTethering :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO ()
requestEnableTethering icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "requestEnableTethering") retVoid []

-- | requestDisableTethering
--
-- Send this message to disable tethered capture on the camera device if the camera has the 'ICCameraDeviceCanTakePicture' capability and if your process has already sent a 'requestEnableTethering' to it.
--
-- ObjC selector: @- requestDisableTethering@
requestDisableTethering :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO ()
requestDisableTethering icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "requestDisableTethering") retVoid []

-- | requestSendPTPCommand:outData:sendCommandDelegate:sendCommandDelegate:contextInfo:
--
-- This method asynchronously sends a PTP command to a camera.
--
-- This should be sent only if the 'capabilities' property contains 'ICCameraDeviceCanAcceptPTPCommands'. All PTP cameras have this capability. The response to this command will be delivered using didSendCommandSelector of sendCommandDelegate. The didSendCommandSelector should have the same signature as: - (void)didSendPTPCommand:(NSData*)command inData:(NSData*)data response:(NSData*)response error:(NSError*)error contextInfo:(void*)contextInfo. The content of error returned should be examined to determine if the request completed successfully.
--
-- ObjC selector: @- requestSendPTPCommand:outData:sendCommandDelegate:didSendCommandSelector:contextInfo:@
requestSendPTPCommand_outData_sendCommandDelegate_didSendCommandSelector_contextInfo :: (IsICCameraDevice icCameraDevice, IsNSData command, IsNSData data_) => icCameraDevice -> command -> data_ -> RawId -> Selector -> Ptr () -> IO ()
requestSendPTPCommand_outData_sendCommandDelegate_didSendCommandSelector_contextInfo icCameraDevice  command data_ sendCommandDelegate selector contextInfo =
  withObjCPtr command $ \raw_command ->
    withObjCPtr data_ $ \raw_data_ ->
        sendMsg icCameraDevice (mkSelector "requestSendPTPCommand:outData:sendCommandDelegate:didSendCommandSelector:contextInfo:") retVoid [argPtr (castPtr raw_command :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId sendCommandDelegate) :: Ptr ()), argPtr (unSelector selector), argPtr contextInfo]

-- | requestSendPTPCommand:outData:completion
--
-- This method asynchronously sends a PTP command to a camera.
--
-- The response, data, and any error message will be returned the block.
--
-- ObjC selector: @- requestSendPTPCommand:outData:completion:@
requestSendPTPCommand_outData_completion :: (IsICCameraDevice icCameraDevice, IsNSData ptpCommand, IsNSData ptpData) => icCameraDevice -> ptpCommand -> ptpData -> Ptr () -> IO ()
requestSendPTPCommand_outData_completion icCameraDevice  ptpCommand ptpData completion =
  withObjCPtr ptpCommand $ \raw_ptpCommand ->
    withObjCPtr ptpData $ \raw_ptpData ->
        sendMsg icCameraDevice (mkSelector "requestSendPTPCommand:outData:completion:") retVoid [argPtr (castPtr raw_ptpCommand :: Ptr ()), argPtr (castPtr raw_ptpData :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | contentCatalogPercentCompleted
--
-- ￼Indicates the percentage of content cataloging completed on the device. Its value ranges from 0 to 100.
--
-- ObjC selector: @- contentCatalogPercentCompleted@
contentCatalogPercentCompleted :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO CULong
contentCatalogPercentCompleted icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "contentCatalogPercentCompleted") retCULong []

-- | contents
--
-- ￼Contents of the camera. The structure of the elements in this array will reflect the folder structure of the storage reported by the camera. Each item in this array will correspond to a storage on the camera.
--
-- ObjC selector: @- contents@
contents :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO (Id NSArray)
contents icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "contents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mediaFiles
--
-- ￼The property mediaFiles represents all image, movie and audio files on the camera. These files are returned as a single array without regard to the folder hierarchy used to store these files on the camera.
--
-- ObjC selector: @- mediaFiles@
mediaFiles :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO (Id NSArray)
mediaFiles icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "mediaFiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ejectable
--
-- ￼Indicates whether the device can be 'soft' removed or disconnected.
--
-- ObjC selector: @- ejectable@
ejectable :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO Bool
ejectable icCameraDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraDevice (mkSelector "ejectable") retCULong []

-- | locked
--
-- ￼Indicates whether the device is locked.  A locked device does not allow for deletion of any asset.
--
-- ObjC selector: @- locked@
locked :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO Bool
locked icCameraDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraDevice (mkSelector "locked") retCULong []

-- | accessRestrictedAppleDevice
--
-- Set to YES if the device is made by Apple and is pass-coded locked and connected to an untrusted host.
--
-- ObjC selector: @- accessRestrictedAppleDevice@
accessRestrictedAppleDevice :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO Bool
accessRestrictedAppleDevice icCameraDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraDevice (mkSelector "accessRestrictedAppleDevice") retCULong []

-- | iCloudPhotosEnabled
--
-- Set to YES if the device is made by Apple and is pass-coded locked and connected to an untrusted host.
--
-- ObjC selector: @- iCloudPhotosEnabled@
iCloudPhotosEnabled :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO Bool
iCloudPhotosEnabled icCameraDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraDevice (mkSelector "iCloudPhotosEnabled") retCULong []

-- | mountPoint
--
-- Filesystem mount point for a device with transportType of ICTransportTypeMassStorage. This will be NULL for all other devices.
--
-- ObjC selector: @- mountPoint@
mountPoint :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO RawId
mountPoint icCameraDevice  =
    fmap (RawId . castPtr) $ sendMsg icCameraDevice (mkSelector "mountPoint") (retPtr retVoid) []

-- | mediaPresentation
--
-- The media presentation describes the visible assets from a device that may contain multiple formats of each media asset.  The asigngments are of the type ICMediaPresentation enumeration.  This property is available only if the capability ICCameraDeviceSupportsHEIF is  present.
--
-- A device supporting this capability can specify the  following presentations:
--
-- ICMediaPresentationConverted - The default behavior for applications retrieving images from a device supporting HEIF is to show only converted JPG from HEIF originals, and only H264 encoded video assets from HEVC.  ICMediaPresentationOriginal - This presentation will show only original images from a device supporting HEIF and HEVC.  Burned in renders are always exported in JPG, as are burned in effects for MOV clips.
--
-- ObjC selector: @- mediaPresentation@
mediaPresentation :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO ICMediaPresentation
mediaPresentation icCameraDevice  =
    fmap (coerce :: CULong -> ICMediaPresentation) $ sendMsg icCameraDevice (mkSelector "mediaPresentation") retCULong []

-- | mediaPresentation
--
-- The media presentation describes the visible assets from a device that may contain multiple formats of each media asset.  The asigngments are of the type ICMediaPresentation enumeration.  This property is available only if the capability ICCameraDeviceSupportsHEIF is  present.
--
-- A device supporting this capability can specify the  following presentations:
--
-- ICMediaPresentationConverted - The default behavior for applications retrieving images from a device supporting HEIF is to show only converted JPG from HEIF originals, and only H264 encoded video assets from HEVC.  ICMediaPresentationOriginal - This presentation will show only original images from a device supporting HEIF and HEVC.  Burned in renders are always exported in JPG, as are burned in effects for MOV clips.
--
-- ObjC selector: @- setMediaPresentation:@
setMediaPresentation :: IsICCameraDevice icCameraDevice => icCameraDevice -> ICMediaPresentation -> IO ()
setMediaPresentation icCameraDevice  value =
    sendMsg icCameraDevice (mkSelector "setMediaPresentation:") retVoid [argCULong (coerce value)]

-- | timeOffset
--
-- Indicates the time offset, in seconds, between the camera's clock and the computer's clock￼. This value is positive if the camera's clock is ahead of the computer's clock. This property should be ignored if the camera's capabilities property does not contain ICCameraDeviceCanSyncClock.
--
-- ObjC selector: @- timeOffset@
timeOffset :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO CDouble
timeOffset icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "timeOffset") retCDouble []

-- | batteryLevelAvailable
--
-- Indicates if the device has reported battery charge level￼.
--
-- ObjC selector: @- batteryLevelAvailable@
batteryLevelAvailable :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO Bool
batteryLevelAvailable icCameraDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraDevice (mkSelector "batteryLevelAvailable") retCULong []

-- | batteryLevel
--
-- ￼Indicates the battery charge level. Its value ranges from 0 to 100.
--
-- ObjC selector: @- batteryLevel@
batteryLevel :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO CULong
batteryLevel icCameraDevice  =
    sendMsg icCameraDevice (mkSelector "batteryLevel") retCULong []

-- | tetheredCaptureEnabled
--
-- This property is always set to YES when the device has the capability 'ICCameraDeviceCanTakePicture'
--
-- requestEnableTethering/requestDisableTethering is no longer required to setup and destroy the standard  take picture functionality of supported cameras.
--
-- ObjC selector: @- tetheredCaptureEnabled@
tetheredCaptureEnabled :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO Bool
tetheredCaptureEnabled icCameraDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraDevice (mkSelector "tetheredCaptureEnabled") retCULong []

-- | ptpEventHandler
--
-- As an alternative to setting up an object to handle PTP event packets, a handler can be set.  The handler will always be called in place of the delegate if non-nil.  If the handler is not present, the delegate will be called if present. It is guaranteed only one of the methods will be called if both are implemented.
--
-- ObjC selector: @- ptpEventHandler@
ptpEventHandler :: IsICCameraDevice icCameraDevice => icCameraDevice -> IO (Ptr ())
ptpEventHandler icCameraDevice  =
    fmap castPtr $ sendMsg icCameraDevice (mkSelector "ptpEventHandler") (retPtr retVoid) []

-- | ptpEventHandler
--
-- As an alternative to setting up an object to handle PTP event packets, a handler can be set.  The handler will always be called in place of the delegate if non-nil.  If the handler is not present, the delegate will be called if present. It is guaranteed only one of the methods will be called if both are implemented.
--
-- ObjC selector: @- setPtpEventHandler:@
setPtpEventHandler :: IsICCameraDevice icCameraDevice => icCameraDevice -> Ptr () -> IO ()
setPtpEventHandler icCameraDevice  value =
    sendMsg icCameraDevice (mkSelector "setPtpEventHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filesOfType:@
filesOfTypeSelector :: Selector
filesOfTypeSelector = mkSelector "filesOfType:"

-- | @Selector@ for @requestReadDataFromFile:atOffset:length:readDelegate:didReadDataSelector:contextInfo:@
requestReadDataFromFile_atOffset_length_readDelegate_didReadDataSelector_contextInfoSelector :: Selector
requestReadDataFromFile_atOffset_length_readDelegate_didReadDataSelector_contextInfoSelector = mkSelector "requestReadDataFromFile:atOffset:length:readDelegate:didReadDataSelector:contextInfo:"

-- | @Selector@ for @requestDownloadFile:options:downloadDelegate:didDownloadSelector:contextInfo:@
requestDownloadFile_options_downloadDelegate_didDownloadSelector_contextInfoSelector :: Selector
requestDownloadFile_options_downloadDelegate_didDownloadSelector_contextInfoSelector = mkSelector "requestDownloadFile:options:downloadDelegate:didDownloadSelector:contextInfo:"

-- | @Selector@ for @cancelDownload@
cancelDownloadSelector :: Selector
cancelDownloadSelector = mkSelector "cancelDownload"

-- | @Selector@ for @requestDeleteFiles:@
requestDeleteFilesSelector :: Selector
requestDeleteFilesSelector = mkSelector "requestDeleteFiles:"

-- | @Selector@ for @cancelDelete@
cancelDeleteSelector :: Selector
cancelDeleteSelector = mkSelector "cancelDelete"

-- | @Selector@ for @requestSyncClock@
requestSyncClockSelector :: Selector
requestSyncClockSelector = mkSelector "requestSyncClock"

-- | @Selector@ for @requestUploadFile:options:uploadDelegate:didUploadSelector:contextInfo:@
requestUploadFile_options_uploadDelegate_didUploadSelector_contextInfoSelector :: Selector
requestUploadFile_options_uploadDelegate_didUploadSelector_contextInfoSelector = mkSelector "requestUploadFile:options:uploadDelegate:didUploadSelector:contextInfo:"

-- | @Selector@ for @requestTakePicture@
requestTakePictureSelector :: Selector
requestTakePictureSelector = mkSelector "requestTakePicture"

-- | @Selector@ for @requestEnableTethering@
requestEnableTetheringSelector :: Selector
requestEnableTetheringSelector = mkSelector "requestEnableTethering"

-- | @Selector@ for @requestDisableTethering@
requestDisableTetheringSelector :: Selector
requestDisableTetheringSelector = mkSelector "requestDisableTethering"

-- | @Selector@ for @requestSendPTPCommand:outData:sendCommandDelegate:didSendCommandSelector:contextInfo:@
requestSendPTPCommand_outData_sendCommandDelegate_didSendCommandSelector_contextInfoSelector :: Selector
requestSendPTPCommand_outData_sendCommandDelegate_didSendCommandSelector_contextInfoSelector = mkSelector "requestSendPTPCommand:outData:sendCommandDelegate:didSendCommandSelector:contextInfo:"

-- | @Selector@ for @requestSendPTPCommand:outData:completion:@
requestSendPTPCommand_outData_completionSelector :: Selector
requestSendPTPCommand_outData_completionSelector = mkSelector "requestSendPTPCommand:outData:completion:"

-- | @Selector@ for @contentCatalogPercentCompleted@
contentCatalogPercentCompletedSelector :: Selector
contentCatalogPercentCompletedSelector = mkSelector "contentCatalogPercentCompleted"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @mediaFiles@
mediaFilesSelector :: Selector
mediaFilesSelector = mkSelector "mediaFiles"

-- | @Selector@ for @ejectable@
ejectableSelector :: Selector
ejectableSelector = mkSelector "ejectable"

-- | @Selector@ for @locked@
lockedSelector :: Selector
lockedSelector = mkSelector "locked"

-- | @Selector@ for @accessRestrictedAppleDevice@
accessRestrictedAppleDeviceSelector :: Selector
accessRestrictedAppleDeviceSelector = mkSelector "accessRestrictedAppleDevice"

-- | @Selector@ for @iCloudPhotosEnabled@
iCloudPhotosEnabledSelector :: Selector
iCloudPhotosEnabledSelector = mkSelector "iCloudPhotosEnabled"

-- | @Selector@ for @mountPoint@
mountPointSelector :: Selector
mountPointSelector = mkSelector "mountPoint"

-- | @Selector@ for @mediaPresentation@
mediaPresentationSelector :: Selector
mediaPresentationSelector = mkSelector "mediaPresentation"

-- | @Selector@ for @setMediaPresentation:@
setMediaPresentationSelector :: Selector
setMediaPresentationSelector = mkSelector "setMediaPresentation:"

-- | @Selector@ for @timeOffset@
timeOffsetSelector :: Selector
timeOffsetSelector = mkSelector "timeOffset"

-- | @Selector@ for @batteryLevelAvailable@
batteryLevelAvailableSelector :: Selector
batteryLevelAvailableSelector = mkSelector "batteryLevelAvailable"

-- | @Selector@ for @batteryLevel@
batteryLevelSelector :: Selector
batteryLevelSelector = mkSelector "batteryLevel"

-- | @Selector@ for @tetheredCaptureEnabled@
tetheredCaptureEnabledSelector :: Selector
tetheredCaptureEnabledSelector = mkSelector "tetheredCaptureEnabled"

-- | @Selector@ for @ptpEventHandler@
ptpEventHandlerSelector :: Selector
ptpEventHandlerSelector = mkSelector "ptpEventHandler"

-- | @Selector@ for @setPtpEventHandler:@
setPtpEventHandlerSelector :: Selector
setPtpEventHandlerSelector = mkSelector "setPtpEventHandler:"

