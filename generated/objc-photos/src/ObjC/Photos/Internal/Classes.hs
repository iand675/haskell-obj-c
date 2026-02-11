{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Photos.Internal.Classes (
    module ObjC.Photos.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.UniformTypeIdentifiers.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- ---------- PHAdjustmentData ----------

-- | Phantom type for @PHAdjustmentData@.
data PHAdjustmentData

instance IsObjCObject (Id PHAdjustmentData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAdjustmentData"

class IsNSObject a => IsPHAdjustmentData a where
  toPHAdjustmentData :: a -> Id PHAdjustmentData

instance IsPHAdjustmentData (Id PHAdjustmentData) where
  toPHAdjustmentData = unsafeCastId

instance IsNSObject (Id PHAdjustmentData) where
  toNSObject = unsafeCastId

-- ---------- PHAssetResource ----------

-- | Phantom type for @PHAssetResource@.
data PHAssetResource

instance IsObjCObject (Id PHAssetResource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetResource"

class IsNSObject a => IsPHAssetResource a where
  toPHAssetResource :: a -> Id PHAssetResource

instance IsPHAssetResource (Id PHAssetResource) where
  toPHAssetResource = unsafeCastId

instance IsNSObject (Id PHAssetResource) where
  toNSObject = unsafeCastId

-- ---------- PHAssetResourceCreationOptions ----------

-- | Phantom type for @PHAssetResourceCreationOptions@.
data PHAssetResourceCreationOptions

instance IsObjCObject (Id PHAssetResourceCreationOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetResourceCreationOptions"

class IsNSObject a => IsPHAssetResourceCreationOptions a where
  toPHAssetResourceCreationOptions :: a -> Id PHAssetResourceCreationOptions

instance IsPHAssetResourceCreationOptions (Id PHAssetResourceCreationOptions) where
  toPHAssetResourceCreationOptions = unsafeCastId

instance IsNSObject (Id PHAssetResourceCreationOptions) where
  toNSObject = unsafeCastId

-- ---------- PHAssetResourceManager ----------

-- | Phantom type for @PHAssetResourceManager@.
data PHAssetResourceManager

instance IsObjCObject (Id PHAssetResourceManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetResourceManager"

class IsNSObject a => IsPHAssetResourceManager a where
  toPHAssetResourceManager :: a -> Id PHAssetResourceManager

instance IsPHAssetResourceManager (Id PHAssetResourceManager) where
  toPHAssetResourceManager = unsafeCastId

instance IsNSObject (Id PHAssetResourceManager) where
  toNSObject = unsafeCastId

-- ---------- PHAssetResourceRequestOptions ----------

-- | Phantom type for @PHAssetResourceRequestOptions@.
data PHAssetResourceRequestOptions

instance IsObjCObject (Id PHAssetResourceRequestOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetResourceRequestOptions"

class IsNSObject a => IsPHAssetResourceRequestOptions a where
  toPHAssetResourceRequestOptions :: a -> Id PHAssetResourceRequestOptions

instance IsPHAssetResourceRequestOptions (Id PHAssetResourceRequestOptions) where
  toPHAssetResourceRequestOptions = unsafeCastId

instance IsNSObject (Id PHAssetResourceRequestOptions) where
  toNSObject = unsafeCastId

-- ---------- PHChange ----------

-- | Phantom type for @PHChange@.
data PHChange

instance IsObjCObject (Id PHChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHChange"

class IsNSObject a => IsPHChange a where
  toPHChange :: a -> Id PHChange

instance IsPHChange (Id PHChange) where
  toPHChange = unsafeCastId

instance IsNSObject (Id PHChange) where
  toNSObject = unsafeCastId

-- ---------- PHChangeRequest ----------

-- | Phantom type for @PHChangeRequest@.
data PHChangeRequest

instance IsObjCObject (Id PHChangeRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHChangeRequest"

class IsNSObject a => IsPHChangeRequest a where
  toPHChangeRequest :: a -> Id PHChangeRequest

instance IsPHChangeRequest (Id PHChangeRequest) where
  toPHChangeRequest = unsafeCastId

instance IsNSObject (Id PHChangeRequest) where
  toNSObject = unsafeCastId

-- ---------- PHCloudIdentifier ----------

-- | Phantom type for @PHCloudIdentifier@.
data PHCloudIdentifier

instance IsObjCObject (Id PHCloudIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHCloudIdentifier"

class IsNSObject a => IsPHCloudIdentifier a where
  toPHCloudIdentifier :: a -> Id PHCloudIdentifier

instance IsPHCloudIdentifier (Id PHCloudIdentifier) where
  toPHCloudIdentifier = unsafeCastId

instance IsNSObject (Id PHCloudIdentifier) where
  toNSObject = unsafeCastId

-- ---------- PHCloudIdentifierMapping ----------

-- | Contains the cloud identifier result from looking up a local identifier via @cloudIdentifierMappingsForLocalIdentifiers,@ or an @error@ indicating why the lookup failed
-- 
-- Phantom type for @PHCloudIdentifierMapping@.
data PHCloudIdentifierMapping

instance IsObjCObject (Id PHCloudIdentifierMapping) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHCloudIdentifierMapping"

class IsNSObject a => IsPHCloudIdentifierMapping a where
  toPHCloudIdentifierMapping :: a -> Id PHCloudIdentifierMapping

instance IsPHCloudIdentifierMapping (Id PHCloudIdentifierMapping) where
  toPHCloudIdentifierMapping = unsafeCastId

instance IsNSObject (Id PHCloudIdentifierMapping) where
  toNSObject = unsafeCastId

-- ---------- PHContentEditingInput ----------

-- | Phantom type for @PHContentEditingInput@.
data PHContentEditingInput

instance IsObjCObject (Id PHContentEditingInput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHContentEditingInput"

class IsNSObject a => IsPHContentEditingInput a where
  toPHContentEditingInput :: a -> Id PHContentEditingInput

instance IsPHContentEditingInput (Id PHContentEditingInput) where
  toPHContentEditingInput = unsafeCastId

instance IsNSObject (Id PHContentEditingInput) where
  toNSObject = unsafeCastId

-- ---------- PHContentEditingInputRequestOptions ----------

-- | Phantom type for @PHContentEditingInputRequestOptions@.
data PHContentEditingInputRequestOptions

instance IsObjCObject (Id PHContentEditingInputRequestOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHContentEditingInputRequestOptions"

class IsNSObject a => IsPHContentEditingInputRequestOptions a where
  toPHContentEditingInputRequestOptions :: a -> Id PHContentEditingInputRequestOptions

instance IsPHContentEditingInputRequestOptions (Id PHContentEditingInputRequestOptions) where
  toPHContentEditingInputRequestOptions = unsafeCastId

instance IsNSObject (Id PHContentEditingInputRequestOptions) where
  toNSObject = unsafeCastId

-- ---------- PHContentEditingOutput ----------

-- | Phantom type for @PHContentEditingOutput@.
data PHContentEditingOutput

instance IsObjCObject (Id PHContentEditingOutput) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHContentEditingOutput"

class IsNSObject a => IsPHContentEditingOutput a where
  toPHContentEditingOutput :: a -> Id PHContentEditingOutput

instance IsPHContentEditingOutput (Id PHContentEditingOutput) where
  toPHContentEditingOutput = unsafeCastId

instance IsNSObject (Id PHContentEditingOutput) where
  toNSObject = unsafeCastId

-- ---------- PHFetchOptions ----------

-- | Phantom type for @PHFetchOptions@.
data PHFetchOptions

instance IsObjCObject (Id PHFetchOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHFetchOptions"

class IsNSObject a => IsPHFetchOptions a where
  toPHFetchOptions :: a -> Id PHFetchOptions

instance IsPHFetchOptions (Id PHFetchOptions) where
  toPHFetchOptions = unsafeCastId

instance IsNSObject (Id PHFetchOptions) where
  toNSObject = unsafeCastId

-- ---------- PHFetchResult ----------

-- | Phantom type for @PHFetchResult@.
data PHFetchResult

instance IsObjCObject (Id PHFetchResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHFetchResult"

class IsNSObject a => IsPHFetchResult a where
  toPHFetchResult :: a -> Id PHFetchResult

instance IsPHFetchResult (Id PHFetchResult) where
  toPHFetchResult = unsafeCastId

instance IsNSObject (Id PHFetchResult) where
  toNSObject = unsafeCastId

-- ---------- PHFetchResultChangeDetails ----------

-- | Phantom type for @PHFetchResultChangeDetails@.
data PHFetchResultChangeDetails

instance IsObjCObject (Id PHFetchResultChangeDetails) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHFetchResultChangeDetails"

class IsNSObject a => IsPHFetchResultChangeDetails a where
  toPHFetchResultChangeDetails :: a -> Id PHFetchResultChangeDetails

instance IsPHFetchResultChangeDetails (Id PHFetchResultChangeDetails) where
  toPHFetchResultChangeDetails = unsafeCastId

instance IsNSObject (Id PHFetchResultChangeDetails) where
  toNSObject = unsafeCastId

-- ---------- PHImageManager ----------

-- | Phantom type for @PHImageManager@.
data PHImageManager

instance IsObjCObject (Id PHImageManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHImageManager"

class IsNSObject a => IsPHImageManager a where
  toPHImageManager :: a -> Id PHImageManager

instance IsPHImageManager (Id PHImageManager) where
  toPHImageManager = unsafeCastId

instance IsNSObject (Id PHImageManager) where
  toNSObject = unsafeCastId

-- ---------- PHImageRequestOptions ----------

-- | Phantom type for @PHImageRequestOptions@.
data PHImageRequestOptions

instance IsObjCObject (Id PHImageRequestOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHImageRequestOptions"

class IsNSObject a => IsPHImageRequestOptions a where
  toPHImageRequestOptions :: a -> Id PHImageRequestOptions

instance IsPHImageRequestOptions (Id PHImageRequestOptions) where
  toPHImageRequestOptions = unsafeCastId

instance IsNSObject (Id PHImageRequestOptions) where
  toNSObject = unsafeCastId

-- ---------- PHLivePhoto ----------

-- | Phantom type for @PHLivePhoto@.
data PHLivePhoto

instance IsObjCObject (Id PHLivePhoto) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHLivePhoto"

class IsNSObject a => IsPHLivePhoto a where
  toPHLivePhoto :: a -> Id PHLivePhoto

instance IsPHLivePhoto (Id PHLivePhoto) where
  toPHLivePhoto = unsafeCastId

instance IsNSObject (Id PHLivePhoto) where
  toNSObject = unsafeCastId

-- ---------- PHLivePhotoEditingContext ----------

-- | Phantom type for @PHLivePhotoEditingContext@.
data PHLivePhotoEditingContext

instance IsObjCObject (Id PHLivePhotoEditingContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHLivePhotoEditingContext"

class IsNSObject a => IsPHLivePhotoEditingContext a where
  toPHLivePhotoEditingContext :: a -> Id PHLivePhotoEditingContext

instance IsPHLivePhotoEditingContext (Id PHLivePhotoEditingContext) where
  toPHLivePhotoEditingContext = unsafeCastId

instance IsNSObject (Id PHLivePhotoEditingContext) where
  toNSObject = unsafeCastId

-- ---------- PHLivePhotoRequestOptions ----------

-- | Phantom type for @PHLivePhotoRequestOptions@.
data PHLivePhotoRequestOptions

instance IsObjCObject (Id PHLivePhotoRequestOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHLivePhotoRequestOptions"

class IsNSObject a => IsPHLivePhotoRequestOptions a where
  toPHLivePhotoRequestOptions :: a -> Id PHLivePhotoRequestOptions

instance IsPHLivePhotoRequestOptions (Id PHLivePhotoRequestOptions) where
  toPHLivePhotoRequestOptions = unsafeCastId

instance IsNSObject (Id PHLivePhotoRequestOptions) where
  toNSObject = unsafeCastId

-- ---------- PHLocalIdentifierMapping ----------

-- | Contains the local identifier result from looking up a cloud identifier via @localIdentifierMappingsForCloudIdentifiers,@ or an @error@ indicating why the lookup failed
-- 
-- Phantom type for @PHLocalIdentifierMapping@.
data PHLocalIdentifierMapping

instance IsObjCObject (Id PHLocalIdentifierMapping) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHLocalIdentifierMapping"

class IsNSObject a => IsPHLocalIdentifierMapping a where
  toPHLocalIdentifierMapping :: a -> Id PHLocalIdentifierMapping

instance IsPHLocalIdentifierMapping (Id PHLocalIdentifierMapping) where
  toPHLocalIdentifierMapping = unsafeCastId

instance IsNSObject (Id PHLocalIdentifierMapping) where
  toNSObject = unsafeCastId

-- ---------- PHObject ----------

-- | Phantom type for @PHObject@.
data PHObject

instance IsObjCObject (Id PHObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHObject"

class IsNSObject a => IsPHObject a where
  toPHObject :: a -> Id PHObject

instance IsPHObject (Id PHObject) where
  toPHObject = unsafeCastId

instance IsNSObject (Id PHObject) where
  toNSObject = unsafeCastId

-- ---------- PHObjectChangeDetails ----------

-- | Phantom type for @PHObjectChangeDetails@.
data PHObjectChangeDetails

instance IsObjCObject (Id PHObjectChangeDetails) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHObjectChangeDetails"

class IsNSObject a => IsPHObjectChangeDetails a where
  toPHObjectChangeDetails :: a -> Id PHObjectChangeDetails

instance IsPHObjectChangeDetails (Id PHObjectChangeDetails) where
  toPHObjectChangeDetails = unsafeCastId

instance IsNSObject (Id PHObjectChangeDetails) where
  toNSObject = unsafeCastId

-- ---------- PHPersistentChange ----------

-- | Phantom type for @PHPersistentChange@.
data PHPersistentChange

instance IsObjCObject (Id PHPersistentChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPersistentChange"

class IsNSObject a => IsPHPersistentChange a where
  toPHPersistentChange :: a -> Id PHPersistentChange

instance IsPHPersistentChange (Id PHPersistentChange) where
  toPHPersistentChange = unsafeCastId

instance IsNSObject (Id PHPersistentChange) where
  toNSObject = unsafeCastId

-- ---------- PHPersistentChangeFetchResult ----------

-- | Phantom type for @PHPersistentChangeFetchResult@.
data PHPersistentChangeFetchResult

instance IsObjCObject (Id PHPersistentChangeFetchResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPersistentChangeFetchResult"

class IsNSObject a => IsPHPersistentChangeFetchResult a where
  toPHPersistentChangeFetchResult :: a -> Id PHPersistentChangeFetchResult

instance IsPHPersistentChangeFetchResult (Id PHPersistentChangeFetchResult) where
  toPHPersistentChangeFetchResult = unsafeCastId

instance IsNSObject (Id PHPersistentChangeFetchResult) where
  toNSObject = unsafeCastId

-- ---------- PHPersistentChangeToken ----------

-- | Phantom type for @PHPersistentChangeToken@.
data PHPersistentChangeToken

instance IsObjCObject (Id PHPersistentChangeToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPersistentChangeToken"

class IsNSObject a => IsPHPersistentChangeToken a where
  toPHPersistentChangeToken :: a -> Id PHPersistentChangeToken

instance IsPHPersistentChangeToken (Id PHPersistentChangeToken) where
  toPHPersistentChangeToken = unsafeCastId

instance IsNSObject (Id PHPersistentChangeToken) where
  toNSObject = unsafeCastId

-- ---------- PHPersistentObjectChangeDetails ----------

-- | Phantom type for @PHPersistentObjectChangeDetails@.
data PHPersistentObjectChangeDetails

instance IsObjCObject (Id PHPersistentObjectChangeDetails) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPersistentObjectChangeDetails"

class IsNSObject a => IsPHPersistentObjectChangeDetails a where
  toPHPersistentObjectChangeDetails :: a -> Id PHPersistentObjectChangeDetails

instance IsPHPersistentObjectChangeDetails (Id PHPersistentObjectChangeDetails) where
  toPHPersistentObjectChangeDetails = unsafeCastId

instance IsNSObject (Id PHPersistentObjectChangeDetails) where
  toNSObject = unsafeCastId

-- ---------- PHPhotoLibrary ----------

-- | Phantom type for @PHPhotoLibrary@.
data PHPhotoLibrary

instance IsObjCObject (Id PHPhotoLibrary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHPhotoLibrary"

class IsNSObject a => IsPHPhotoLibrary a where
  toPHPhotoLibrary :: a -> Id PHPhotoLibrary

instance IsPHPhotoLibrary (Id PHPhotoLibrary) where
  toPHPhotoLibrary = unsafeCastId

instance IsNSObject (Id PHPhotoLibrary) where
  toNSObject = unsafeCastId

-- ---------- PHVideoRequestOptions ----------

-- | Phantom type for @PHVideoRequestOptions@.
data PHVideoRequestOptions

instance IsObjCObject (Id PHVideoRequestOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHVideoRequestOptions"

class IsNSObject a => IsPHVideoRequestOptions a where
  toPHVideoRequestOptions :: a -> Id PHVideoRequestOptions

instance IsPHVideoRequestOptions (Id PHVideoRequestOptions) where
  toPHVideoRequestOptions = unsafeCastId

instance IsNSObject (Id PHVideoRequestOptions) where
  toNSObject = unsafeCastId

-- ---------- PHAssetChangeRequest ----------

-- | Phantom type for @PHAssetChangeRequest@.
data PHAssetChangeRequest

instance IsObjCObject (Id PHAssetChangeRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetChangeRequest"

class IsPHChangeRequest a => IsPHAssetChangeRequest a where
  toPHAssetChangeRequest :: a -> Id PHAssetChangeRequest

instance IsPHAssetChangeRequest (Id PHAssetChangeRequest) where
  toPHAssetChangeRequest = unsafeCastId

instance IsNSObject (Id PHAssetChangeRequest) where
  toNSObject = unsafeCastId

instance IsPHChangeRequest (Id PHAssetChangeRequest) where
  toPHChangeRequest = unsafeCastId

-- ---------- PHAssetCollectionChangeRequest ----------

-- | Phantom type for @PHAssetCollectionChangeRequest@.
data PHAssetCollectionChangeRequest

instance IsObjCObject (Id PHAssetCollectionChangeRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetCollectionChangeRequest"

class IsPHChangeRequest a => IsPHAssetCollectionChangeRequest a where
  toPHAssetCollectionChangeRequest :: a -> Id PHAssetCollectionChangeRequest

instance IsPHAssetCollectionChangeRequest (Id PHAssetCollectionChangeRequest) where
  toPHAssetCollectionChangeRequest = unsafeCastId

instance IsNSObject (Id PHAssetCollectionChangeRequest) where
  toNSObject = unsafeCastId

instance IsPHChangeRequest (Id PHAssetCollectionChangeRequest) where
  toPHChangeRequest = unsafeCastId

-- ---------- PHAssetResourceUploadJobChangeRequest ----------

-- | Use within an application's @com.apple.photos.background-upload@ extension to create and change ``PHAssetResourceUploadJob`` records.
--
-- When the extension's principal class receives a call to @process@ background uploads, it can create new ``PHAssetResourceUploadJob``s through calls to perform changes on a PHPhotoLibrary using ``PHAssetResourceUploadJobChangeRequest`` and any in-flight upload jobs can be handled by updating their state to mark them as acknowledged, or to be retried. The maximum number of jobs that can be in flight is limited to the ``PHAssetResourceUploadJob.jobLimit``.
--
-- ``PHAssetResourceUploadJobChangeRequest`` can only be created or used within a photo library change block. For details on change blocks, see ``PHPhotoLibrary``.
-- 
-- Phantom type for @PHAssetResourceUploadJobChangeRequest@.
data PHAssetResourceUploadJobChangeRequest

instance IsObjCObject (Id PHAssetResourceUploadJobChangeRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetResourceUploadJobChangeRequest"

class IsPHChangeRequest a => IsPHAssetResourceUploadJobChangeRequest a where
  toPHAssetResourceUploadJobChangeRequest :: a -> Id PHAssetResourceUploadJobChangeRequest

instance IsPHAssetResourceUploadJobChangeRequest (Id PHAssetResourceUploadJobChangeRequest) where
  toPHAssetResourceUploadJobChangeRequest = unsafeCastId

instance IsNSObject (Id PHAssetResourceUploadJobChangeRequest) where
  toNSObject = unsafeCastId

instance IsPHChangeRequest (Id PHAssetResourceUploadJobChangeRequest) where
  toPHChangeRequest = unsafeCastId

-- ---------- PHCollectionListChangeRequest ----------

-- | Phantom type for @PHCollectionListChangeRequest@.
data PHCollectionListChangeRequest

instance IsObjCObject (Id PHCollectionListChangeRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHCollectionListChangeRequest"

class IsPHChangeRequest a => IsPHCollectionListChangeRequest a where
  toPHCollectionListChangeRequest :: a -> Id PHCollectionListChangeRequest

instance IsPHCollectionListChangeRequest (Id PHCollectionListChangeRequest) where
  toPHCollectionListChangeRequest = unsafeCastId

instance IsNSObject (Id PHCollectionListChangeRequest) where
  toNSObject = unsafeCastId

instance IsPHChangeRequest (Id PHCollectionListChangeRequest) where
  toPHChangeRequest = unsafeCastId

-- ---------- PHProjectChangeRequest ----------

-- | Phantom type for @PHProjectChangeRequest@.
data PHProjectChangeRequest

instance IsObjCObject (Id PHProjectChangeRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProjectChangeRequest"

class IsPHChangeRequest a => IsPHProjectChangeRequest a where
  toPHProjectChangeRequest :: a -> Id PHProjectChangeRequest

instance IsPHProjectChangeRequest (Id PHProjectChangeRequest) where
  toPHProjectChangeRequest = unsafeCastId

instance IsNSObject (Id PHProjectChangeRequest) where
  toNSObject = unsafeCastId

instance IsPHChangeRequest (Id PHProjectChangeRequest) where
  toPHChangeRequest = unsafeCastId

-- ---------- PHCachingImageManager ----------

-- | Phantom type for @PHCachingImageManager@.
data PHCachingImageManager

instance IsObjCObject (Id PHCachingImageManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHCachingImageManager"

class IsPHImageManager a => IsPHCachingImageManager a where
  toPHCachingImageManager :: a -> Id PHCachingImageManager

instance IsPHCachingImageManager (Id PHCachingImageManager) where
  toPHCachingImageManager = unsafeCastId

instance IsNSObject (Id PHCachingImageManager) where
  toNSObject = unsafeCastId

instance IsPHImageManager (Id PHCachingImageManager) where
  toPHImageManager = unsafeCastId

-- ---------- PHAsset ----------

-- | Phantom type for @PHAsset@.
data PHAsset

instance IsObjCObject (Id PHAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAsset"

class IsPHObject a => IsPHAsset a where
  toPHAsset :: a -> Id PHAsset

instance IsPHAsset (Id PHAsset) where
  toPHAsset = unsafeCastId

instance IsNSObject (Id PHAsset) where
  toNSObject = unsafeCastId

instance IsPHObject (Id PHAsset) where
  toPHObject = unsafeCastId

-- ---------- PHAssetResourceUploadJob ----------

-- | An object that represents a request to upload an asset resource.
--
-- Use within an application's @com.apple.photos.background-upload@ extension to request an upload of a ``PHAssetResource`` to a destination <doc://com.apple.documentation/documentation/foundation/nsurlrequest>.
--
-- When the extension's principal class receives a call to ``PHBackgroundResourceUploadExtension/process()`` background uploads, it can create new ``PHAssetResourceUploadJob`` objects using ``PHAssetResourceUploadJobChangeRequest``.
--
-- The maximum number of jobs that can be in flight is limited to the ``jobLimit``. To make space for new jobs, you must call ``PHAssetResourceUploadJobChangeRequest/fetchJobsWithAction:options:`` and retry/acknowledge them with ``PHAssetResourceUploadJobChangeRequest/acknowledge:`` or ``PHAssetResourceUploadJobChangeRequest/retryWithDestination:`` respectively.
-- 
-- Phantom type for @PHAssetResourceUploadJob@.
data PHAssetResourceUploadJob

instance IsObjCObject (Id PHAssetResourceUploadJob) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetResourceUploadJob"

class IsPHObject a => IsPHAssetResourceUploadJob a where
  toPHAssetResourceUploadJob :: a -> Id PHAssetResourceUploadJob

instance IsPHAssetResourceUploadJob (Id PHAssetResourceUploadJob) where
  toPHAssetResourceUploadJob = unsafeCastId

instance IsNSObject (Id PHAssetResourceUploadJob) where
  toNSObject = unsafeCastId

instance IsPHObject (Id PHAssetResourceUploadJob) where
  toPHObject = unsafeCastId

-- ---------- PHCollection ----------

-- | Phantom type for @PHCollection@.
data PHCollection

instance IsObjCObject (Id PHCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHCollection"

class IsPHObject a => IsPHCollection a where
  toPHCollection :: a -> Id PHCollection

instance IsPHCollection (Id PHCollection) where
  toPHCollection = unsafeCastId

instance IsNSObject (Id PHCollection) where
  toNSObject = unsafeCastId

instance IsPHObject (Id PHCollection) where
  toPHObject = unsafeCastId

-- ---------- PHObjectPlaceholder ----------

-- | Phantom type for @PHObjectPlaceholder@.
data PHObjectPlaceholder

instance IsObjCObject (Id PHObjectPlaceholder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHObjectPlaceholder"

class IsPHObject a => IsPHObjectPlaceholder a where
  toPHObjectPlaceholder :: a -> Id PHObjectPlaceholder

instance IsPHObjectPlaceholder (Id PHObjectPlaceholder) where
  toPHObjectPlaceholder = unsafeCastId

instance IsNSObject (Id PHObjectPlaceholder) where
  toNSObject = unsafeCastId

instance IsPHObject (Id PHObjectPlaceholder) where
  toPHObject = unsafeCastId

-- ---------- PHAssetCreationRequest ----------

-- | Phantom type for @PHAssetCreationRequest@.
data PHAssetCreationRequest

instance IsObjCObject (Id PHAssetCreationRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetCreationRequest"

class IsPHAssetChangeRequest a => IsPHAssetCreationRequest a where
  toPHAssetCreationRequest :: a -> Id PHAssetCreationRequest

instance IsPHAssetCreationRequest (Id PHAssetCreationRequest) where
  toPHAssetCreationRequest = unsafeCastId

instance IsNSObject (Id PHAssetCreationRequest) where
  toNSObject = unsafeCastId

instance IsPHAssetChangeRequest (Id PHAssetCreationRequest) where
  toPHAssetChangeRequest = unsafeCastId

instance IsPHChangeRequest (Id PHAssetCreationRequest) where
  toPHChangeRequest = unsafeCastId

-- ---------- PHAssetCollection ----------

-- | Phantom type for @PHAssetCollection@.
data PHAssetCollection

instance IsObjCObject (Id PHAssetCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHAssetCollection"

class IsPHCollection a => IsPHAssetCollection a where
  toPHAssetCollection :: a -> Id PHAssetCollection

instance IsPHAssetCollection (Id PHAssetCollection) where
  toPHAssetCollection = unsafeCastId

instance IsNSObject (Id PHAssetCollection) where
  toNSObject = unsafeCastId

instance IsPHCollection (Id PHAssetCollection) where
  toPHCollection = unsafeCastId

instance IsPHObject (Id PHAssetCollection) where
  toPHObject = unsafeCastId

-- ---------- PHCollectionList ----------

-- | Phantom type for @PHCollectionList@.
data PHCollectionList

instance IsObjCObject (Id PHCollectionList) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHCollectionList"

class IsPHCollection a => IsPHCollectionList a where
  toPHCollectionList :: a -> Id PHCollectionList

instance IsPHCollectionList (Id PHCollectionList) where
  toPHCollectionList = unsafeCastId

instance IsNSObject (Id PHCollectionList) where
  toNSObject = unsafeCastId

instance IsPHCollection (Id PHCollectionList) where
  toPHCollection = unsafeCastId

instance IsPHObject (Id PHCollectionList) where
  toPHObject = unsafeCastId

-- ---------- PHProject ----------

-- | Phantom type for @PHProject@.
data PHProject

instance IsObjCObject (Id PHProject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PHProject"

class IsPHAssetCollection a => IsPHProject a where
  toPHProject :: a -> Id PHProject

instance IsPHProject (Id PHProject) where
  toPHProject = unsafeCastId

instance IsNSObject (Id PHProject) where
  toNSObject = unsafeCastId

instance IsPHAssetCollection (Id PHProject) where
  toPHAssetCollection = unsafeCastId

instance IsPHCollection (Id PHProject) where
  toPHCollection = unsafeCastId

instance IsPHObject (Id PHProject) where
  toPHObject = unsafeCastId
