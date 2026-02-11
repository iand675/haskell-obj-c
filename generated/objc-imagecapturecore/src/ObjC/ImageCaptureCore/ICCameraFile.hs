{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICCameraFile
--
-- This class represents a file on an ICCameraDevice object.
--
-- Generated bindings for @ICCameraFile@.
module ObjC.ImageCaptureCore.ICCameraFile
  ( ICCameraFile
  , IsICCameraFile(..)
  , fingerprintForFileAtURL
  , requestThumbnailDataWithOptions_completion
  , requestMetadataDictionaryWithOptions_completion
  , requestDownloadWithOptions_completion
  , requestReadDataAtOffset_length_completion
  , requestSecurityScopedURLWithCompletion
  , requestFingerprintWithCompletion
  , width
  , height
  , originalFilename
  , createdFilename
  , fileSize
  , orientation
  , setOrientation
  , duration
  , highFramerate
  , timeLapse
  , firstPicked
  , originatingAssetID
  , groupUUID
  , gpsString
  , relatedUUID
  , burstUUID
  , burstFavorite
  , burstPicked
  , sidecarFiles
  , pairedRawImage
  , fileCreationDate
  , fileModificationDate
  , exifCreationDate
  , exifModificationDate
  , fingerprint
  , fingerprintForFileAtURLSelector
  , requestThumbnailDataWithOptions_completionSelector
  , requestMetadataDictionaryWithOptions_completionSelector
  , requestDownloadWithOptions_completionSelector
  , requestReadDataAtOffset_length_completionSelector
  , requestSecurityScopedURLWithCompletionSelector
  , requestFingerprintWithCompletionSelector
  , widthSelector
  , heightSelector
  , originalFilenameSelector
  , createdFilenameSelector
  , fileSizeSelector
  , orientationSelector
  , setOrientationSelector
  , durationSelector
  , highFramerateSelector
  , timeLapseSelector
  , firstPickedSelector
  , originatingAssetIDSelector
  , groupUUIDSelector
  , gpsStringSelector
  , relatedUUIDSelector
  , burstUUIDSelector
  , burstFavoriteSelector
  , burstPickedSelector
  , sidecarFilesSelector
  , pairedRawImageSelector
  , fileCreationDateSelector
  , fileModificationDateSelector
  , exifCreationDateSelector
  , exifModificationDateSelector
  , fingerprintSelector

  -- * Enum types
  , ICEXIFOrientationType(ICEXIFOrientationType)
  , pattern ICEXIFOrientation1
  , pattern ICEXIFOrientation2
  , pattern ICEXIFOrientation3
  , pattern ICEXIFOrientation4
  , pattern ICEXIFOrientation5
  , pattern ICEXIFOrientation6
  , pattern ICEXIFOrientation7
  , pattern ICEXIFOrientation8

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

-- | fingerprintForFileAtURL
--
-- Generates a fingerprint given a URL date, or nil.
--
-- ObjC selector: @+ fingerprintForFileAtURL:@
fingerprintForFileAtURL :: IsNSURL url => url -> IO (Id NSString)
fingerprintForFileAtURL url =
  do
    cls' <- getRequiredClass "ICCameraFile"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "fingerprintForFileAtURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | requestThumbnailDataWithOptions:completion
--
-- ￼Perform a thumbnail request and execute the block callback in place of the delegate.
--
-- @options@ — Options dictionary
--
-- - 'kCGImageSourceThumbnailMaxPixelSize' - Request a width different from the embedded EXIF thumbnail
--
-- @completion@ — Completion block called with an NSData* object representing the JPG, and an NSError* for status.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestThumbnailDataWithOptions:completion:@
requestThumbnailDataWithOptions_completion :: (IsICCameraFile icCameraFile, IsNSDictionary options) => icCameraFile -> options -> Ptr () -> IO ()
requestThumbnailDataWithOptions_completion icCameraFile  options completion =
withObjCPtr options $ \raw_options ->
    sendMsg icCameraFile (mkSelector "requestThumbnailDataWithOptions:completion:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | requestMetadataDictionaryWithOptions:completion
--
-- ￼Perform a metadata request and execute the block callback in place of the delegate.
--
-- @options@ — Options dictionary
--
-- @completion@ — Completion block called with an NSDictionary* object containing the metadata, and an NSError* for status.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestMetadataDictionaryWithOptions:completion:@
requestMetadataDictionaryWithOptions_completion :: (IsICCameraFile icCameraFile, IsNSDictionary options) => icCameraFile -> options -> Ptr () -> IO ()
requestMetadataDictionaryWithOptions_completion icCameraFile  options completion =
withObjCPtr options $ \raw_options ->
    sendMsg icCameraFile (mkSelector "requestMetadataDictionaryWithOptions:completion:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | requestDownloadWithOptions:progressDelegate:completion
--
-- ￼Perform a download request and execute the block callback in place of the delegate.
--
-- @options@ — Dictionary Keys:
--
-- - @ICDownloadsDirectoryURL@   - @ICSaveAsFilename@   - @ICOverwriteExistingFile@   - @ICDeleteAfterDownload@   - @ICAdjustCreationDate@
--
-- @completion@ — Completion block to executed after request has returned,
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestDownloadWithOptions:completion:@
requestDownloadWithOptions_completion :: (IsICCameraFile icCameraFile, IsNSDictionary options) => icCameraFile -> options -> Ptr () -> IO (Id NSProgress)
requestDownloadWithOptions_completion icCameraFile  options completion =
withObjCPtr options $ \raw_options ->
    sendMsg icCameraFile (mkSelector "requestDownloadWithOptions:completion:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completion :: Ptr ())] >>= retainedObject . castPtr

-- | requestReadDataAtOffset:length:completion
--
-- This method asynchronously reads data of a specified length from a specified offset.
--
-- @offset@ — The offset into the file to start reading from
--
-- @length@ — The length of data to be read.
--
-- @completion@ — Completion block called with an NSData* object representing the data, and an NSError* for status.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestReadDataAtOffset:length:completion:@
requestReadDataAtOffset_length_completion :: IsICCameraFile icCameraFile => icCameraFile -> CLong -> CLong -> Ptr () -> IO ()
requestReadDataAtOffset_length_completion icCameraFile  offset length_ completion =
  sendMsg icCameraFile (mkSelector "requestReadDataAtOffset:length:completion:") retVoid [argCLong (fromIntegral offset), argCLong (fromIntegral length_), argPtr (castPtr completion :: Ptr ())]

-- | requestSecurityScopedURLWithCompletion
--
-- ￼Requests a security scoped NSURL* for a media file on a mass storage volume. The returned NSURL* requires the use of startAccessingSecurityScopedResource, and stopAccessingSecurityScopedResource for access.
--
-- @completion@ — Completion block called with an NSURL*, and an NSError* for status.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestSecurityScopedURLWithCompletion:@
requestSecurityScopedURLWithCompletion :: IsICCameraFile icCameraFile => icCameraFile -> Ptr () -> IO ()
requestSecurityScopedURLWithCompletion icCameraFile  completion =
  sendMsg icCameraFile (mkSelector "requestSecurityScopedURLWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | requestFingerprintWithCompletion
--
-- ￼Requests a fingerprint be generated for camera file.
--
-- @completion@ — Completion block called with an NSString*, and an NSError* for status.
--
-- Note: The completion block will execute on an any available queue, often this will not be the main queue.
--
-- ObjC selector: @- requestFingerprintWithCompletion:@
requestFingerprintWithCompletion :: IsICCameraFile icCameraFile => icCameraFile -> Ptr () -> IO ()
requestFingerprintWithCompletion icCameraFile  completion =
  sendMsg icCameraFile (mkSelector "requestFingerprintWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | width
--
-- Width of an image or movie frame.
--
-- ObjC selector: @- width@
width :: IsICCameraFile icCameraFile => icCameraFile -> IO CLong
width icCameraFile  =
  sendMsg icCameraFile (mkSelector "width") retCLong []

-- | height
--
-- Height of an image or movie frame.
--
-- ObjC selector: @- height@
height :: IsICCameraFile icCameraFile => icCameraFile -> IO CLong
height icCameraFile  =
  sendMsg icCameraFile (mkSelector "height") retCLong []

-- | originalFilename
--
-- Original filename on disk
--
-- ObjC selector: @- originalFilename@
originalFilename :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
originalFilename icCameraFile  =
  sendMsg icCameraFile (mkSelector "originalFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | createdFilename
--
-- Created filename
--
-- ObjC selector: @- createdFilename@
createdFilename :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
createdFilename icCameraFile  =
  sendMsg icCameraFile (mkSelector "createdFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fileSize
--
-- ￼Size of file in bytes.
--
-- ObjC selector: @- fileSize@
fileSize :: IsICCameraFile icCameraFile => icCameraFile -> IO CLong
fileSize icCameraFile  =
  sendMsg icCameraFile (mkSelector "fileSize") retCLong []

-- | orientation
--
-- ￼Desired orientation of image to use when it is downloaded.
--
-- This property is set to ICEXIFOrientation1 initially. If the format of this file supports EXIF orientation tag, then this property will be updated to match the value of that tag, when the thumbnail or metadata for this file is received.
--
-- ObjC selector: @- orientation@
orientation :: IsICCameraFile icCameraFile => icCameraFile -> IO ICEXIFOrientationType
orientation icCameraFile  =
  fmap (coerce :: CULong -> ICEXIFOrientationType) $ sendMsg icCameraFile (mkSelector "orientation") retCULong []

-- | orientation
--
-- ￼Desired orientation of image to use when it is downloaded.
--
-- This property is set to ICEXIFOrientation1 initially. If the format of this file supports EXIF orientation tag, then this property will be updated to match the value of that tag, when the thumbnail or metadata for this file is received.
--
-- ObjC selector: @- setOrientation:@
setOrientation :: IsICCameraFile icCameraFile => icCameraFile -> ICEXIFOrientationType -> IO ()
setOrientation icCameraFile  value =
  sendMsg icCameraFile (mkSelector "setOrientation:") retVoid [argCULong (coerce value)]

-- | duration
--
-- ￼Duration of audio/video file in seconds.
--
-- ObjC selector: @- duration@
duration :: IsICCameraFile icCameraFile => icCameraFile -> IO CDouble
duration icCameraFile  =
  sendMsg icCameraFile (mkSelector "duration") retCDouble []

-- | highFramerate
--
-- True if file is a slo-mo or high framerate video file, nil otherwise.
--
-- ObjC selector: @- highFramerate@
highFramerate :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
highFramerate icCameraFile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraFile (mkSelector "highFramerate") retCULong []

-- | timeLapse
--
-- True if file is a time-lapse video file, nil otherwise.
--
-- ObjC selector: @- timeLapse@
timeLapse :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
timeLapse icCameraFile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraFile (mkSelector "timeLapse") retCULong []

-- | firstPicked
--
-- True if file is a firstPicked nil otherwise.
--
-- ObjC selector: @- firstPicked@
firstPicked :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
firstPicked icCameraFile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraFile (mkSelector "firstPicked") retCULong []

-- | originatingAssetID
--
-- originatingAssetID of file if present, nil if not a HEIF or HVEC.
--
-- ObjC selector: @- originatingAssetID@
originatingAssetID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
originatingAssetID icCameraFile  =
  sendMsg icCameraFile (mkSelector "originatingAssetID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | groupUUID
--
-- groupUUID of file if present, nil if file has no groupUUID.
--
-- ObjC selector: @- groupUUID@
groupUUID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
groupUUID icCameraFile  =
  sendMsg icCameraFile (mkSelector "groupUUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gpsString
--
-- GPS String in standard format.
--
-- ObjC selector: @- gpsString@
gpsString :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
gpsString icCameraFile  =
  sendMsg icCameraFile (mkSelector "gpsString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | relatedUUID
--
-- Internal related UUID for dbg/aae/etc.
--
-- ObjC selector: @- relatedUUID@
relatedUUID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
relatedUUID icCameraFile  =
  sendMsg icCameraFile (mkSelector "relatedUUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | burstUUID
--
-- burstUUID of file if present, nil if not in a burst.
--
-- ObjC selector: @- burstUUID@
burstUUID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
burstUUID icCameraFile  =
  sendMsg icCameraFile (mkSelector "burstUUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | burstFavorite
--
-- True if burst favorite, ignored if not in a burst or not a burst favorite.
--
-- ObjC selector: @- burstFavorite@
burstFavorite :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
burstFavorite icCameraFile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraFile (mkSelector "burstFavorite") retCULong []

-- | burstPicked
--
-- True if burst user picked, ignored if not in a burst or not a burst user picked.
--
-- ObjC selector: @- burstPicked@
burstPicked :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
burstPicked icCameraFile  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg icCameraFile (mkSelector "burstPicked") retCULong []

-- | sidecarFiles
--
-- This property is NULL if there are no sidecar files associated with this file. Otherwise it is an array of ICCameraFile instances of sidecar files associated with this file. An example of a sidecar file is a file with the same base name as this file and having an extension XMP.
--
-- ObjC selector: @- sidecarFiles@
sidecarFiles :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSArray)
sidecarFiles icCameraFile  =
  sendMsg icCameraFile (mkSelector "sidecarFiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pairedRawImage
--
-- A single item subset of the sidecarFiles array, which contains the logical RAW compliment of a JPG or other format image.
--
-- ObjC selector: @- pairedRawImage@
pairedRawImage :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id ICCameraFile)
pairedRawImage icCameraFile  =
  sendMsg icCameraFile (mkSelector "pairedRawImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fileCreationDate
--
-- Properties will either represent the actual file creation date, or nil.
--
-- ObjC selector: @- fileCreationDate@
fileCreationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
fileCreationDate icCameraFile  =
  sendMsg icCameraFile (mkSelector "fileCreationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fileModificationDate
--
-- Properties will either represent the actual file modification date, or nil.
--
-- ObjC selector: @- fileModificationDate@
fileModificationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
fileModificationDate icCameraFile  =
  sendMsg icCameraFile (mkSelector "fileModificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exifCreationDate
--
-- Properties will either represent the exif creation date, or nil.
--
-- ObjC selector: @- exifCreationDate@
exifCreationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
exifCreationDate icCameraFile  =
  sendMsg icCameraFile (mkSelector "exifCreationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exifModificationDate
--
-- Properties will either represent the exif modification date, or nil.
--
-- ObjC selector: @- exifModificationDate@
exifModificationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
exifModificationDate icCameraFile  =
  sendMsg icCameraFile (mkSelector "exifModificationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fingerprint
--
-- A fingerprint generated from the camera file data date, or nil.
--
-- ObjC selector: @- fingerprint@
fingerprint :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
fingerprint icCameraFile  =
  sendMsg icCameraFile (mkSelector "fingerprint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fingerprintForFileAtURL:@
fingerprintForFileAtURLSelector :: Selector
fingerprintForFileAtURLSelector = mkSelector "fingerprintForFileAtURL:"

-- | @Selector@ for @requestThumbnailDataWithOptions:completion:@
requestThumbnailDataWithOptions_completionSelector :: Selector
requestThumbnailDataWithOptions_completionSelector = mkSelector "requestThumbnailDataWithOptions:completion:"

-- | @Selector@ for @requestMetadataDictionaryWithOptions:completion:@
requestMetadataDictionaryWithOptions_completionSelector :: Selector
requestMetadataDictionaryWithOptions_completionSelector = mkSelector "requestMetadataDictionaryWithOptions:completion:"

-- | @Selector@ for @requestDownloadWithOptions:completion:@
requestDownloadWithOptions_completionSelector :: Selector
requestDownloadWithOptions_completionSelector = mkSelector "requestDownloadWithOptions:completion:"

-- | @Selector@ for @requestReadDataAtOffset:length:completion:@
requestReadDataAtOffset_length_completionSelector :: Selector
requestReadDataAtOffset_length_completionSelector = mkSelector "requestReadDataAtOffset:length:completion:"

-- | @Selector@ for @requestSecurityScopedURLWithCompletion:@
requestSecurityScopedURLWithCompletionSelector :: Selector
requestSecurityScopedURLWithCompletionSelector = mkSelector "requestSecurityScopedURLWithCompletion:"

-- | @Selector@ for @requestFingerprintWithCompletion:@
requestFingerprintWithCompletionSelector :: Selector
requestFingerprintWithCompletionSelector = mkSelector "requestFingerprintWithCompletion:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @originalFilename@
originalFilenameSelector :: Selector
originalFilenameSelector = mkSelector "originalFilename"

-- | @Selector@ for @createdFilename@
createdFilenameSelector :: Selector
createdFilenameSelector = mkSelector "createdFilename"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @highFramerate@
highFramerateSelector :: Selector
highFramerateSelector = mkSelector "highFramerate"

-- | @Selector@ for @timeLapse@
timeLapseSelector :: Selector
timeLapseSelector = mkSelector "timeLapse"

-- | @Selector@ for @firstPicked@
firstPickedSelector :: Selector
firstPickedSelector = mkSelector "firstPicked"

-- | @Selector@ for @originatingAssetID@
originatingAssetIDSelector :: Selector
originatingAssetIDSelector = mkSelector "originatingAssetID"

-- | @Selector@ for @groupUUID@
groupUUIDSelector :: Selector
groupUUIDSelector = mkSelector "groupUUID"

-- | @Selector@ for @gpsString@
gpsStringSelector :: Selector
gpsStringSelector = mkSelector "gpsString"

-- | @Selector@ for @relatedUUID@
relatedUUIDSelector :: Selector
relatedUUIDSelector = mkSelector "relatedUUID"

-- | @Selector@ for @burstUUID@
burstUUIDSelector :: Selector
burstUUIDSelector = mkSelector "burstUUID"

-- | @Selector@ for @burstFavorite@
burstFavoriteSelector :: Selector
burstFavoriteSelector = mkSelector "burstFavorite"

-- | @Selector@ for @burstPicked@
burstPickedSelector :: Selector
burstPickedSelector = mkSelector "burstPicked"

-- | @Selector@ for @sidecarFiles@
sidecarFilesSelector :: Selector
sidecarFilesSelector = mkSelector "sidecarFiles"

-- | @Selector@ for @pairedRawImage@
pairedRawImageSelector :: Selector
pairedRawImageSelector = mkSelector "pairedRawImage"

-- | @Selector@ for @fileCreationDate@
fileCreationDateSelector :: Selector
fileCreationDateSelector = mkSelector "fileCreationDate"

-- | @Selector@ for @fileModificationDate@
fileModificationDateSelector :: Selector
fileModificationDateSelector = mkSelector "fileModificationDate"

-- | @Selector@ for @exifCreationDate@
exifCreationDateSelector :: Selector
exifCreationDateSelector = mkSelector "exifCreationDate"

-- | @Selector@ for @exifModificationDate@
exifModificationDateSelector :: Selector
exifModificationDateSelector = mkSelector "exifModificationDate"

-- | @Selector@ for @fingerprint@
fingerprintSelector :: Selector
fingerprintSelector = mkSelector "fingerprint"

