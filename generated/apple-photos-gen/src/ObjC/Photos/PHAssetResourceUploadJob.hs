{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a request to upload an asset resource.
--
-- Use within an application's @com.apple.photos.background-upload@ extension to request an upload of a ``PHAssetResource`` to a destination <doc://com.apple.documentation/documentation/foundation/nsurlrequest>.
--
-- When the extension's principal class receives a call to ``PHBackgroundResourceUploadExtension/process()`` background uploads, it can create new ``PHAssetResourceUploadJob`` objects using ``PHAssetResourceUploadJobChangeRequest``.
--
-- The maximum number of jobs that can be in flight is limited to the ``jobLimit``. To make space for new jobs, you must call ``PHAssetResourceUploadJobChangeRequest/fetchJobsWithAction:options:`` and retry/acknowledge them with ``PHAssetResourceUploadJobChangeRequest/acknowledge:`` or ``PHAssetResourceUploadJobChangeRequest/retryWithDestination:`` respectively.
--
-- Generated bindings for @PHAssetResourceUploadJob@.
module ObjC.Photos.PHAssetResourceUploadJob
  ( PHAssetResourceUploadJob
  , IsPHAssetResourceUploadJob(..)
  , fetchJobsWithAction_options
  , jobLimit
  , resource
  , destination
  , state
  , destinationSelector
  , fetchJobsWithAction_optionsSelector
  , jobLimitSelector
  , resourceSelector
  , stateSelector

  -- * Enum types
  , PHAssetResourceUploadJobAction(PHAssetResourceUploadJobAction)
  , pattern PHAssetResourceUploadJobActionAcknowledge
  , pattern PHAssetResourceUploadJobActionRetry
  , PHAssetResourceUploadJobState(PHAssetResourceUploadJobState)
  , pattern PHAssetResourceUploadJobStateRegistered
  , pattern PHAssetResourceUploadJobStatePending
  , pattern PHAssetResourceUploadJobStateFailed
  , pattern PHAssetResourceUploadJobStateSucceeded

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns all asset resource upload jobs applicable for a given action.
--
-- - Parameters:     - action: The actions a client can take on a job.     - options: The fetch options to be passed in.
--
-- - Returns: The jobs available on which you can apply an action found in ``PHAssetResourceUploadJobAction``.
--
-- ObjC selector: @+ fetchJobsWithAction:options:@
fetchJobsWithAction_options :: IsPHFetchOptions options => PHAssetResourceUploadJobAction -> options -> IO (Id PHFetchResult)
fetchJobsWithAction_options action options =
  do
    cls' <- getRequiredClass "PHAssetResourceUploadJob"
    sendClassMessage cls' fetchJobsWithAction_optionsSelector action (toPHFetchOptions options)

-- | The maximum number of unacknowledged upload jobs allowed.
--
-- This includes jobs that are in-flight and those that have succeeded or failed.
--
-- ObjC selector: @+ jobLimit@
jobLimit :: IO CLong
jobLimit  =
  do
    cls' <- getRequiredClass "PHAssetResourceUploadJob"
    sendClassMessage cls' jobLimitSelector

-- | The asset resource this job promises to upload.
--
-- ObjC selector: @- resource@
resource :: IsPHAssetResourceUploadJob phAssetResourceUploadJob => phAssetResourceUploadJob -> IO (Id PHAssetResource)
resource phAssetResourceUploadJob =
  sendMessage phAssetResourceUploadJob resourceSelector

-- | The destination to send the job's resource.
--
-- ObjC selector: @- destination@
destination :: IsPHAssetResourceUploadJob phAssetResourceUploadJob => phAssetResourceUploadJob -> IO (Id NSURLRequest)
destination phAssetResourceUploadJob =
  sendMessage phAssetResourceUploadJob destinationSelector

-- | The state of this upload job.
--
-- ObjC selector: @- state@
state :: IsPHAssetResourceUploadJob phAssetResourceUploadJob => phAssetResourceUploadJob -> IO PHAssetResourceUploadJobState
state phAssetResourceUploadJob =
  sendMessage phAssetResourceUploadJob stateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchJobsWithAction:options:@
fetchJobsWithAction_optionsSelector :: Selector '[PHAssetResourceUploadJobAction, Id PHFetchOptions] (Id PHFetchResult)
fetchJobsWithAction_optionsSelector = mkSelector "fetchJobsWithAction:options:"

-- | @Selector@ for @jobLimit@
jobLimitSelector :: Selector '[] CLong
jobLimitSelector = mkSelector "jobLimit"

-- | @Selector@ for @resource@
resourceSelector :: Selector '[] (Id PHAssetResource)
resourceSelector = mkSelector "resource"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id NSURLRequest)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @state@
stateSelector :: Selector '[] PHAssetResourceUploadJobState
stateSelector = mkSelector "state"

