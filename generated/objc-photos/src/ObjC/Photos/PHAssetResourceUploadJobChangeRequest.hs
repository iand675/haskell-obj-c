{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Use within an application's @com.apple.photos.background-upload@ extension to create and change ``PHAssetResourceUploadJob`` records.
--
-- When the extension's principal class receives a call to @process@ background uploads, it can create new ``PHAssetResourceUploadJob``s through calls to perform changes on a PHPhotoLibrary using ``PHAssetResourceUploadJobChangeRequest`` and any in-flight upload jobs can be handled by updating their state to mark them as acknowledged, or to be retried. The maximum number of jobs that can be in flight is limited to the ``PHAssetResourceUploadJob.jobLimit``.
--
-- ``PHAssetResourceUploadJobChangeRequest`` can only be created or used within a photo library change block. For details on change blocks, see ``PHPhotoLibrary``.
--
-- Generated bindings for @PHAssetResourceUploadJobChangeRequest@.
module ObjC.Photos.PHAssetResourceUploadJobChangeRequest
  ( PHAssetResourceUploadJobChangeRequest
  , IsPHAssetResourceUploadJobChangeRequest(..)
  , createJobWithDestination_resource
  , changeRequestForUploadJob
  , acknowledge
  , retryWithDestination
  , createJobWithDestination_resourceSelector
  , changeRequestForUploadJobSelector
  , acknowledgeSelector
  , retryWithDestinationSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an asset resource upload job.
--
-- If the number of jobs exceeds ``PHAssetResourceUploadJob/jobLimit`` the photo library ``performChanges`` request will fail with a ``PHPhotosErrorLimitExceeded`` error. To generate jobs after this limit is triggered, you must acknowledge succeeded/failed jobs, and wait for the registered/pending ones to finish uploading, which will make those jobs also succeeded/failed.
--
-- - Parameter:     - destination: the destination <doc://com.apple.documentation/foundation/nsurlrequest> to which this asset resource will be sent.     - resource: the asset resource to be uploaded.
--
-- ObjC selector: @+ createJobWithDestination:resource:@
createJobWithDestination_resource :: (IsNSURLRequest destination, IsPHAssetResource resource) => destination -> resource -> IO ()
createJobWithDestination_resource destination resource =
  do
    cls' <- getRequiredClass "PHAssetResourceUploadJobChangeRequest"
    withObjCPtr destination $ \raw_destination ->
      withObjCPtr resource $ \raw_resource ->
        sendClassMsg cls' (mkSelector "createJobWithDestination:resource:") retVoid [argPtr (castPtr raw_destination :: Ptr ()), argPtr (castPtr raw_resource :: Ptr ())]

-- | Creates a request for modifying the specified upload job.
--
-- - Parameter job: a job to be modified.
--
-- ObjC selector: @+ changeRequestForUploadJob:@
changeRequestForUploadJob :: IsPHAssetResourceUploadJob job => job -> IO (Id PHAssetResourceUploadJobChangeRequest)
changeRequestForUploadJob job =
  do
    cls' <- getRequiredClass "PHAssetResourceUploadJobChangeRequest"
    withObjCPtr job $ \raw_job ->
      sendClassMsg cls' (mkSelector "changeRequestForUploadJob:") (retPtr retVoid) [argPtr (castPtr raw_job :: Ptr ())] >>= retainedObject . castPtr

-- | Acknowledges a successful or failed job. Jobs must be acknowledged to free up space for ``PHAssetResourceUploadJob/jobLimit``.
--
-- ObjC selector: @- acknowledge@
acknowledge :: IsPHAssetResourceUploadJobChangeRequest phAssetResourceUploadJobChangeRequest => phAssetResourceUploadJobChangeRequest -> IO ()
acknowledge phAssetResourceUploadJobChangeRequest  =
  sendMsg phAssetResourceUploadJobChangeRequest (mkSelector "acknowledge") retVoid []

-- | Retries a job that is failed, unacknowledged, and has not been retried before. Successful retries also free up space for ``PHAssetResourceUploadJob/jobLimit``.
--
-- ObjC selector: @- retryWithDestination:@
retryWithDestination :: (IsPHAssetResourceUploadJobChangeRequest phAssetResourceUploadJobChangeRequest, IsNSURLRequest destination) => phAssetResourceUploadJobChangeRequest -> destination -> IO ()
retryWithDestination phAssetResourceUploadJobChangeRequest  destination =
withObjCPtr destination $ \raw_destination ->
    sendMsg phAssetResourceUploadJobChangeRequest (mkSelector "retryWithDestination:") retVoid [argPtr (castPtr raw_destination :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createJobWithDestination:resource:@
createJobWithDestination_resourceSelector :: Selector
createJobWithDestination_resourceSelector = mkSelector "createJobWithDestination:resource:"

-- | @Selector@ for @changeRequestForUploadJob:@
changeRequestForUploadJobSelector :: Selector
changeRequestForUploadJobSelector = mkSelector "changeRequestForUploadJob:"

-- | @Selector@ for @acknowledge@
acknowledgeSelector :: Selector
acknowledgeSelector = mkSelector "acknowledge"

-- | @Selector@ for @retryWithDestination:@
retryWithDestinationSelector :: Selector
retryWithDestinationSelector = mkSelector "retryWithDestination:"

