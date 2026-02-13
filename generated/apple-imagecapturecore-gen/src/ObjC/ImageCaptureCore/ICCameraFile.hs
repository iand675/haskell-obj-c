{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , burstFavoriteSelector
  , burstPickedSelector
  , burstUUIDSelector
  , createdFilenameSelector
  , durationSelector
  , exifCreationDateSelector
  , exifModificationDateSelector
  , fileCreationDateSelector
  , fileModificationDateSelector
  , fileSizeSelector
  , fingerprintForFileAtURLSelector
  , fingerprintSelector
  , firstPickedSelector
  , gpsStringSelector
  , groupUUIDSelector
  , heightSelector
  , highFramerateSelector
  , orientationSelector
  , originalFilenameSelector
  , originatingAssetIDSelector
  , pairedRawImageSelector
  , relatedUUIDSelector
  , requestDownloadWithOptions_completionSelector
  , requestFingerprintWithCompletionSelector
  , requestMetadataDictionaryWithOptions_completionSelector
  , requestReadDataAtOffset_length_completionSelector
  , requestSecurityScopedURLWithCompletionSelector
  , requestThumbnailDataWithOptions_completionSelector
  , setOrientationSelector
  , sidecarFilesSelector
  , timeLapseSelector
  , widthSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' fingerprintForFileAtURLSelector (toNSURL url)

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
requestThumbnailDataWithOptions_completion icCameraFile options completion =
  sendMessage icCameraFile requestThumbnailDataWithOptions_completionSelector (toNSDictionary options) completion

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
requestMetadataDictionaryWithOptions_completion icCameraFile options completion =
  sendMessage icCameraFile requestMetadataDictionaryWithOptions_completionSelector (toNSDictionary options) completion

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
requestDownloadWithOptions_completion icCameraFile options completion =
  sendMessage icCameraFile requestDownloadWithOptions_completionSelector (toNSDictionary options) completion

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
requestReadDataAtOffset_length_completion icCameraFile offset length_ completion =
  sendMessage icCameraFile requestReadDataAtOffset_length_completionSelector offset length_ completion

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
requestSecurityScopedURLWithCompletion icCameraFile completion =
  sendMessage icCameraFile requestSecurityScopedURLWithCompletionSelector completion

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
requestFingerprintWithCompletion icCameraFile completion =
  sendMessage icCameraFile requestFingerprintWithCompletionSelector completion

-- | width
--
-- Width of an image or movie frame.
--
-- ObjC selector: @- width@
width :: IsICCameraFile icCameraFile => icCameraFile -> IO CLong
width icCameraFile =
  sendMessage icCameraFile widthSelector

-- | height
--
-- Height of an image or movie frame.
--
-- ObjC selector: @- height@
height :: IsICCameraFile icCameraFile => icCameraFile -> IO CLong
height icCameraFile =
  sendMessage icCameraFile heightSelector

-- | originalFilename
--
-- Original filename on disk
--
-- ObjC selector: @- originalFilename@
originalFilename :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
originalFilename icCameraFile =
  sendMessage icCameraFile originalFilenameSelector

-- | createdFilename
--
-- Created filename
--
-- ObjC selector: @- createdFilename@
createdFilename :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
createdFilename icCameraFile =
  sendMessage icCameraFile createdFilenameSelector

-- | fileSize
--
-- ￼Size of file in bytes.
--
-- ObjC selector: @- fileSize@
fileSize :: IsICCameraFile icCameraFile => icCameraFile -> IO CLong
fileSize icCameraFile =
  sendMessage icCameraFile fileSizeSelector

-- | orientation
--
-- ￼Desired orientation of image to use when it is downloaded.
--
-- This property is set to ICEXIFOrientation1 initially. If the format of this file supports EXIF orientation tag, then this property will be updated to match the value of that tag, when the thumbnail or metadata for this file is received.
--
-- ObjC selector: @- orientation@
orientation :: IsICCameraFile icCameraFile => icCameraFile -> IO ICEXIFOrientationType
orientation icCameraFile =
  sendMessage icCameraFile orientationSelector

-- | orientation
--
-- ￼Desired orientation of image to use when it is downloaded.
--
-- This property is set to ICEXIFOrientation1 initially. If the format of this file supports EXIF orientation tag, then this property will be updated to match the value of that tag, when the thumbnail or metadata for this file is received.
--
-- ObjC selector: @- setOrientation:@
setOrientation :: IsICCameraFile icCameraFile => icCameraFile -> ICEXIFOrientationType -> IO ()
setOrientation icCameraFile value =
  sendMessage icCameraFile setOrientationSelector value

-- | duration
--
-- ￼Duration of audio/video file in seconds.
--
-- ObjC selector: @- duration@
duration :: IsICCameraFile icCameraFile => icCameraFile -> IO CDouble
duration icCameraFile =
  sendMessage icCameraFile durationSelector

-- | highFramerate
--
-- True if file is a slo-mo or high framerate video file, nil otherwise.
--
-- ObjC selector: @- highFramerate@
highFramerate :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
highFramerate icCameraFile =
  sendMessage icCameraFile highFramerateSelector

-- | timeLapse
--
-- True if file is a time-lapse video file, nil otherwise.
--
-- ObjC selector: @- timeLapse@
timeLapse :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
timeLapse icCameraFile =
  sendMessage icCameraFile timeLapseSelector

-- | firstPicked
--
-- True if file is a firstPicked nil otherwise.
--
-- ObjC selector: @- firstPicked@
firstPicked :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
firstPicked icCameraFile =
  sendMessage icCameraFile firstPickedSelector

-- | originatingAssetID
--
-- originatingAssetID of file if present, nil if not a HEIF or HVEC.
--
-- ObjC selector: @- originatingAssetID@
originatingAssetID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
originatingAssetID icCameraFile =
  sendMessage icCameraFile originatingAssetIDSelector

-- | groupUUID
--
-- groupUUID of file if present, nil if file has no groupUUID.
--
-- ObjC selector: @- groupUUID@
groupUUID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
groupUUID icCameraFile =
  sendMessage icCameraFile groupUUIDSelector

-- | gpsString
--
-- GPS String in standard format.
--
-- ObjC selector: @- gpsString@
gpsString :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
gpsString icCameraFile =
  sendMessage icCameraFile gpsStringSelector

-- | relatedUUID
--
-- Internal related UUID for dbg/aae/etc.
--
-- ObjC selector: @- relatedUUID@
relatedUUID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
relatedUUID icCameraFile =
  sendMessage icCameraFile relatedUUIDSelector

-- | burstUUID
--
-- burstUUID of file if present, nil if not in a burst.
--
-- ObjC selector: @- burstUUID@
burstUUID :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
burstUUID icCameraFile =
  sendMessage icCameraFile burstUUIDSelector

-- | burstFavorite
--
-- True if burst favorite, ignored if not in a burst or not a burst favorite.
--
-- ObjC selector: @- burstFavorite@
burstFavorite :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
burstFavorite icCameraFile =
  sendMessage icCameraFile burstFavoriteSelector

-- | burstPicked
--
-- True if burst user picked, ignored if not in a burst or not a burst user picked.
--
-- ObjC selector: @- burstPicked@
burstPicked :: IsICCameraFile icCameraFile => icCameraFile -> IO Bool
burstPicked icCameraFile =
  sendMessage icCameraFile burstPickedSelector

-- | sidecarFiles
--
-- This property is NULL if there are no sidecar files associated with this file. Otherwise it is an array of ICCameraFile instances of sidecar files associated with this file. An example of a sidecar file is a file with the same base name as this file and having an extension XMP.
--
-- ObjC selector: @- sidecarFiles@
sidecarFiles :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSArray)
sidecarFiles icCameraFile =
  sendMessage icCameraFile sidecarFilesSelector

-- | pairedRawImage
--
-- A single item subset of the sidecarFiles array, which contains the logical RAW compliment of a JPG or other format image.
--
-- ObjC selector: @- pairedRawImage@
pairedRawImage :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id ICCameraFile)
pairedRawImage icCameraFile =
  sendMessage icCameraFile pairedRawImageSelector

-- | fileCreationDate
--
-- Properties will either represent the actual file creation date, or nil.
--
-- ObjC selector: @- fileCreationDate@
fileCreationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
fileCreationDate icCameraFile =
  sendMessage icCameraFile fileCreationDateSelector

-- | fileModificationDate
--
-- Properties will either represent the actual file modification date, or nil.
--
-- ObjC selector: @- fileModificationDate@
fileModificationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
fileModificationDate icCameraFile =
  sendMessage icCameraFile fileModificationDateSelector

-- | exifCreationDate
--
-- Properties will either represent the exif creation date, or nil.
--
-- ObjC selector: @- exifCreationDate@
exifCreationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
exifCreationDate icCameraFile =
  sendMessage icCameraFile exifCreationDateSelector

-- | exifModificationDate
--
-- Properties will either represent the exif modification date, or nil.
--
-- ObjC selector: @- exifModificationDate@
exifModificationDate :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSDate)
exifModificationDate icCameraFile =
  sendMessage icCameraFile exifModificationDateSelector

-- | fingerprint
--
-- A fingerprint generated from the camera file data date, or nil.
--
-- ObjC selector: @- fingerprint@
fingerprint :: IsICCameraFile icCameraFile => icCameraFile -> IO (Id NSString)
fingerprint icCameraFile =
  sendMessage icCameraFile fingerprintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fingerprintForFileAtURL:@
fingerprintForFileAtURLSelector :: Selector '[Id NSURL] (Id NSString)
fingerprintForFileAtURLSelector = mkSelector "fingerprintForFileAtURL:"

-- | @Selector@ for @requestThumbnailDataWithOptions:completion:@
requestThumbnailDataWithOptions_completionSelector :: Selector '[Id NSDictionary, Ptr ()] ()
requestThumbnailDataWithOptions_completionSelector = mkSelector "requestThumbnailDataWithOptions:completion:"

-- | @Selector@ for @requestMetadataDictionaryWithOptions:completion:@
requestMetadataDictionaryWithOptions_completionSelector :: Selector '[Id NSDictionary, Ptr ()] ()
requestMetadataDictionaryWithOptions_completionSelector = mkSelector "requestMetadataDictionaryWithOptions:completion:"

-- | @Selector@ for @requestDownloadWithOptions:completion:@
requestDownloadWithOptions_completionSelector :: Selector '[Id NSDictionary, Ptr ()] (Id NSProgress)
requestDownloadWithOptions_completionSelector = mkSelector "requestDownloadWithOptions:completion:"

-- | @Selector@ for @requestReadDataAtOffset:length:completion:@
requestReadDataAtOffset_length_completionSelector :: Selector '[CLong, CLong, Ptr ()] ()
requestReadDataAtOffset_length_completionSelector = mkSelector "requestReadDataAtOffset:length:completion:"

-- | @Selector@ for @requestSecurityScopedURLWithCompletion:@
requestSecurityScopedURLWithCompletionSelector :: Selector '[Ptr ()] ()
requestSecurityScopedURLWithCompletionSelector = mkSelector "requestSecurityScopedURLWithCompletion:"

-- | @Selector@ for @requestFingerprintWithCompletion:@
requestFingerprintWithCompletionSelector :: Selector '[Ptr ()] ()
requestFingerprintWithCompletionSelector = mkSelector "requestFingerprintWithCompletion:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CLong
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CLong
heightSelector = mkSelector "height"

-- | @Selector@ for @originalFilename@
originalFilenameSelector :: Selector '[] (Id NSString)
originalFilenameSelector = mkSelector "originalFilename"

-- | @Selector@ for @createdFilename@
createdFilenameSelector :: Selector '[] (Id NSString)
createdFilenameSelector = mkSelector "createdFilename"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector '[] CLong
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] ICEXIFOrientationType
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector '[ICEXIFOrientationType] ()
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @highFramerate@
highFramerateSelector :: Selector '[] Bool
highFramerateSelector = mkSelector "highFramerate"

-- | @Selector@ for @timeLapse@
timeLapseSelector :: Selector '[] Bool
timeLapseSelector = mkSelector "timeLapse"

-- | @Selector@ for @firstPicked@
firstPickedSelector :: Selector '[] Bool
firstPickedSelector = mkSelector "firstPicked"

-- | @Selector@ for @originatingAssetID@
originatingAssetIDSelector :: Selector '[] (Id NSString)
originatingAssetIDSelector = mkSelector "originatingAssetID"

-- | @Selector@ for @groupUUID@
groupUUIDSelector :: Selector '[] (Id NSString)
groupUUIDSelector = mkSelector "groupUUID"

-- | @Selector@ for @gpsString@
gpsStringSelector :: Selector '[] (Id NSString)
gpsStringSelector = mkSelector "gpsString"

-- | @Selector@ for @relatedUUID@
relatedUUIDSelector :: Selector '[] (Id NSString)
relatedUUIDSelector = mkSelector "relatedUUID"

-- | @Selector@ for @burstUUID@
burstUUIDSelector :: Selector '[] (Id NSString)
burstUUIDSelector = mkSelector "burstUUID"

-- | @Selector@ for @burstFavorite@
burstFavoriteSelector :: Selector '[] Bool
burstFavoriteSelector = mkSelector "burstFavorite"

-- | @Selector@ for @burstPicked@
burstPickedSelector :: Selector '[] Bool
burstPickedSelector = mkSelector "burstPicked"

-- | @Selector@ for @sidecarFiles@
sidecarFilesSelector :: Selector '[] (Id NSArray)
sidecarFilesSelector = mkSelector "sidecarFiles"

-- | @Selector@ for @pairedRawImage@
pairedRawImageSelector :: Selector '[] (Id ICCameraFile)
pairedRawImageSelector = mkSelector "pairedRawImage"

-- | @Selector@ for @fileCreationDate@
fileCreationDateSelector :: Selector '[] (Id NSDate)
fileCreationDateSelector = mkSelector "fileCreationDate"

-- | @Selector@ for @fileModificationDate@
fileModificationDateSelector :: Selector '[] (Id NSDate)
fileModificationDateSelector = mkSelector "fileModificationDate"

-- | @Selector@ for @exifCreationDate@
exifCreationDateSelector :: Selector '[] (Id NSDate)
exifCreationDateSelector = mkSelector "exifCreationDate"

-- | @Selector@ for @exifModificationDate@
exifModificationDateSelector :: Selector '[] (Id NSDate)
exifModificationDateSelector = mkSelector "exifModificationDate"

-- | @Selector@ for @fingerprint@
fingerprintSelector :: Selector '[] (Id NSString)
fingerprintSelector = mkSelector "fingerprint"

