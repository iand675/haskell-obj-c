{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICCameraItem
--
-- ICCameraItem is an abstract class that represents an item in an ICCameraDevice object. ICCameraDevice object creates instances of two concrete subclasses of ICCameraItem: ICCameraFolder and ICCameraFile.
--
-- Generated bindings for @ICCameraItem@.
module ObjC.ImageCaptureCore.ICCameraItem
  ( ICCameraItem
  , IsICCameraItem(..)
  , requestThumbnail
  , requestMetadata
  , flushThumbnailCache
  , flushMetadataCache
  , device
  , parentFolder
  , name
  , uti
  , fileSystemPath
  , locked
  , raw
  , inTemporaryStore
  , creationDate
  , modificationDate
  , thumbnail
  , metadata
  , userData
  , ptpObjectHandle
  , addedAfterContentCatalogCompleted
  , thumbnailIfAvailable
  , largeThumbnailIfAvailable
  , addedAfterContentCatalogCompletedSelector
  , creationDateSelector
  , deviceSelector
  , fileSystemPathSelector
  , flushMetadataCacheSelector
  , flushThumbnailCacheSelector
  , inTemporaryStoreSelector
  , largeThumbnailIfAvailableSelector
  , lockedSelector
  , metadataSelector
  , modificationDateSelector
  , nameSelector
  , parentFolderSelector
  , ptpObjectHandleSelector
  , rawSelector
  , requestMetadataSelector
  , requestThumbnailSelector
  , thumbnailIfAvailableSelector
  , thumbnailSelector
  , userDataSelector
  , utiSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | requestThumbnail
--
-- This method requests thumbnail for the item. If one is not readily available, accessing this property will send a message to the device requesting a thumbnail for the file. The delegate of the device will be notified via method "cameraDevice:didReceiveThumbnail:forItem:error:", if this method is implemented by the delegate.
--
-- Note: Execution of the delegate callback will occur on the main thread.
--
-- ObjC selector: @- requestThumbnail@
requestThumbnail :: IsICCameraItem icCameraItem => icCameraItem -> IO ()
requestThumbnail icCameraItem =
  sendMessage icCameraItem requestThumbnailSelector

-- | requestMetadata
--
-- ￼Metadata for the file if one is readily available. If one is not readily available, accessing this property will send a message to the device requesting metadata for the file. The delegate of the device will be notified via method "cameraDevice:didReceiveMetadata:forItem:error:", if this method is implemented by the delegate.
--
-- Note: Execution of the delegate callback will occur on the main thread.
--
-- ObjC selector: @- requestMetadata@
requestMetadata :: IsICCameraItem icCameraItem => icCameraItem -> IO ()
requestMetadata icCameraItem =
  sendMessage icCameraItem requestMetadataSelector

-- | flushThumbnailCache
--
-- ￼Deletes cached thumbnail for the item.
--
-- ObjC selector: @- flushThumbnailCache@
flushThumbnailCache :: IsICCameraItem icCameraItem => icCameraItem -> IO ()
flushThumbnailCache icCameraItem =
  sendMessage icCameraItem flushThumbnailCacheSelector

-- | flushMetadataCache
--
-- ￼Deletes cached metadata for the item.
--
-- ObjC selector: @- flushMetadataCache@
flushMetadataCache :: IsICCameraItem icCameraItem => icCameraItem -> IO ()
flushMetadataCache icCameraItem =
  sendMessage icCameraItem flushMetadataCacheSelector

-- | device
--
-- ￼Parent device of this item.
--
-- ObjC selector: @- device@
device :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id ICCameraDevice)
device icCameraItem =
  sendMessage icCameraItem deviceSelector

-- | parentFolder
--
-- ￼Parent folder of this folder. The root folder's parentFolder is nil.
--
-- ObjC selector: @- parentFolder@
parentFolder :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id ICCameraFolder)
parentFolder icCameraItem =
  sendMessage icCameraItem parentFolderSelector

-- | name
--
-- ￼Name of this item.
--
-- ObjC selector: @- name@
name :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id NSString)
name icCameraItem =
  sendMessage icCameraItem nameSelector

-- | UTI
--
-- ￼Item UTI. This is an Uniform Type Identifier string. It is one of: kUTTypeFolder, kUTTypeImage, kUTTypeMovie, kUTTypeAudio, or kUTTypeData.
--
-- ObjC selector: @- UTI@
uti :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id NSString)
uti icCameraItem =
  sendMessage icCameraItem utiSelector

-- | fileSystemPath
--
-- ￼The file system path of the item for items on a device with transportType of ICTransportTypeMassStorage.
--
-- ObjC selector: @- fileSystemPath@
fileSystemPath :: IsICCameraItem icCameraItem => icCameraItem -> IO RawId
fileSystemPath icCameraItem =
  sendMessage icCameraItem fileSystemPathSelector

-- | locked
--
-- ￼Indicates the protection state of this item. It is locked if the storage card in the camera is locked.
--
-- ObjC selector: @- locked@
locked :: IsICCameraItem icCameraItem => icCameraItem -> IO Bool
locked icCameraItem =
  sendMessage icCameraItem lockedSelector

-- | raw
--
-- ￼Indicates if the file is a raw image file.
--
-- ObjC selector: @- raw@
raw :: IsICCameraItem icCameraItem => icCameraItem -> IO Bool
raw icCameraItem =
  sendMessage icCameraItem rawSelector

-- | inTemporaryStore
--
-- ￼Indicates if this folder is in a temporary store. A temporary store may be used by the device when images are   captures on the device when it is tethered to the computer.
--
-- ObjC selector: @- inTemporaryStore@
inTemporaryStore :: IsICCameraItem icCameraItem => icCameraItem -> IO Bool
inTemporaryStore icCameraItem =
  sendMessage icCameraItem inTemporaryStoreSelector

-- | creationDate
--
-- ￼Creation date of this file. This information is usually the same as the EXIF creation date.
--
-- ObjC selector: @- creationDate@
creationDate :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id NSDate)
creationDate icCameraItem =
  sendMessage icCameraItem creationDateSelector

-- | modificationDate
--
-- ￼Modification date of this file. This information is usually the same as the EXIF modification date.
--
-- ObjC selector: @- modificationDate@
modificationDate :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id NSDate)
modificationDate icCameraItem =
  sendMessage icCameraItem modificationDateSelector

-- | thumbnail
--
-- ￼Thumbnail for the item. The value of this property is NULL unless a 'requestThumbnail' message is sent to this object.
--
-- ObjC selector: @- thumbnail@
thumbnail :: IsICCameraItem icCameraItem => icCameraItem -> IO (Ptr ())
thumbnail icCameraItem =
  sendMessage icCameraItem thumbnailSelector

-- | metadata
--
-- ￼Metadata for the item. The value of this property is NULL unless a 'requestMetadata' message is sent to this object.
--
-- ObjC selector: @- metadata@
metadata :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id NSDictionary)
metadata icCameraItem =
  sendMessage icCameraItem metadataSelector

-- | userData
--
-- ￼A mutable dictionary to store arbitrary key-value pairs associated with a camera item object. This can be used by   view objects that bind to this object to store "house-keeping" information.
--
-- ObjC selector: @- userData@
userData :: IsICCameraItem icCameraItem => icCameraItem -> IO (Id NSMutableDictionary)
userData icCameraItem =
  sendMessage icCameraItem userDataSelector

-- | ptpObjectHandle
--
-- PTP object handle value if the item is on a camera that uses PTP protocol. The value of this property is set to 0   if the camera does not use PTP protocol.
--
-- ObjC selector: @- ptpObjectHandle@
ptpObjectHandle :: IsICCameraItem icCameraItem => icCameraItem -> IO CUInt
ptpObjectHandle icCameraItem =
  sendMessage icCameraItem ptpObjectHandleSelector

-- | addedAfterContentCatalogCompleted
--
-- This property is set if the file is captured on the device after the device's content is fully enumerated. This does not apply to files added as a result of adding a new store to the device.
--
-- ObjC selector: @- addedAfterContentCatalogCompleted@
addedAfterContentCatalogCompleted :: IsICCameraItem icCameraItem => icCameraItem -> IO Bool
addedAfterContentCatalogCompleted icCameraItem =
  sendMessage icCameraItem addedAfterContentCatalogCompletedSelector

-- | thumbnailIfAvailable
--
-- ObjC selector: @- thumbnailIfAvailable@
thumbnailIfAvailable :: IsICCameraItem icCameraItem => icCameraItem -> IO (Ptr ())
thumbnailIfAvailable icCameraItem =
  sendMessage icCameraItem thumbnailIfAvailableSelector

-- | largeThumbnailIfAvailable
--
-- ObjC selector: @- largeThumbnailIfAvailable@
largeThumbnailIfAvailable :: IsICCameraItem icCameraItem => icCameraItem -> IO (Ptr ())
largeThumbnailIfAvailable icCameraItem =
  sendMessage icCameraItem largeThumbnailIfAvailableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestThumbnail@
requestThumbnailSelector :: Selector '[] ()
requestThumbnailSelector = mkSelector "requestThumbnail"

-- | @Selector@ for @requestMetadata@
requestMetadataSelector :: Selector '[] ()
requestMetadataSelector = mkSelector "requestMetadata"

-- | @Selector@ for @flushThumbnailCache@
flushThumbnailCacheSelector :: Selector '[] ()
flushThumbnailCacheSelector = mkSelector "flushThumbnailCache"

-- | @Selector@ for @flushMetadataCache@
flushMetadataCacheSelector :: Selector '[] ()
flushMetadataCacheSelector = mkSelector "flushMetadataCache"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id ICCameraDevice)
deviceSelector = mkSelector "device"

-- | @Selector@ for @parentFolder@
parentFolderSelector :: Selector '[] (Id ICCameraFolder)
parentFolderSelector = mkSelector "parentFolder"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @UTI@
utiSelector :: Selector '[] (Id NSString)
utiSelector = mkSelector "UTI"

-- | @Selector@ for @fileSystemPath@
fileSystemPathSelector :: Selector '[] RawId
fileSystemPathSelector = mkSelector "fileSystemPath"

-- | @Selector@ for @locked@
lockedSelector :: Selector '[] Bool
lockedSelector = mkSelector "locked"

-- | @Selector@ for @raw@
rawSelector :: Selector '[] Bool
rawSelector = mkSelector "raw"

-- | @Selector@ for @inTemporaryStore@
inTemporaryStoreSelector :: Selector '[] Bool
inTemporaryStoreSelector = mkSelector "inTemporaryStore"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @modificationDate@
modificationDateSelector :: Selector '[] (Id NSDate)
modificationDateSelector = mkSelector "modificationDate"

-- | @Selector@ for @thumbnail@
thumbnailSelector :: Selector '[] (Ptr ())
thumbnailSelector = mkSelector "thumbnail"

-- | @Selector@ for @metadata@
metadataSelector :: Selector '[] (Id NSDictionary)
metadataSelector = mkSelector "metadata"

-- | @Selector@ for @userData@
userDataSelector :: Selector '[] (Id NSMutableDictionary)
userDataSelector = mkSelector "userData"

-- | @Selector@ for @ptpObjectHandle@
ptpObjectHandleSelector :: Selector '[] CUInt
ptpObjectHandleSelector = mkSelector "ptpObjectHandle"

-- | @Selector@ for @addedAfterContentCatalogCompleted@
addedAfterContentCatalogCompletedSelector :: Selector '[] Bool
addedAfterContentCatalogCompletedSelector = mkSelector "addedAfterContentCatalogCompleted"

-- | @Selector@ for @thumbnailIfAvailable@
thumbnailIfAvailableSelector :: Selector '[] (Ptr ())
thumbnailIfAvailableSelector = mkSelector "thumbnailIfAvailable"

-- | @Selector@ for @largeThumbnailIfAvailable@
largeThumbnailIfAvailableSelector :: Selector '[] (Ptr ())
largeThumbnailIfAvailableSelector = mkSelector "largeThumbnailIfAvailable"

