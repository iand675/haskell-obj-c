{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Detects specific landmark points on human bodies.
--
-- This request will produce a collection of VNHumanBodyPoseObservation objects which describe the pose of each detected human body.
--
-- Generated bindings for @VNDetectHumanBodyPoseRequest@.
module ObjC.Vision.VNDetectHumanBodyPoseRequest
  ( VNDetectHumanBodyPoseRequest
  , IsVNDetectHumanBodyPoseRequest(..)
  , supportedJointNamesForRevision_error
  , supportedJointNamesAndReturnError
  , supportedJointsGroupNamesForRevision_error
  , supportedJointsGroupNamesAndReturnError
  , results
  , resultsSelector
  , supportedJointNamesAndReturnErrorSelector
  , supportedJointNamesForRevision_errorSelector
  , supportedJointsGroupNamesAndReturnErrorSelector
  , supportedJointsGroupNamesForRevision_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Obtain the collection of human body joint names that are supported by a given request revision.
--
-- @revision@ — The revision of VNDetectHumanBodyPoseRequest being queried.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPoseObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @+ supportedJointNamesForRevision:error:@
supportedJointNamesForRevision_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSArray)
supportedJointNamesForRevision_error revision error_ =
  do
    cls' <- getRequiredClass "VNDetectHumanBodyPoseRequest"
    sendClassMessage cls' supportedJointNamesForRevision_errorSelector revision (toNSError error_)

-- | Obtain the collection of human body joint names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPoseObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnError :: (IsVNDetectHumanBodyPoseRequest vnDetectHumanBodyPoseRequest, IsNSError error_) => vnDetectHumanBodyPoseRequest -> error_ -> IO (Id NSArray)
supportedJointNamesAndReturnError vnDetectHumanBodyPoseRequest error_ =
  sendMessage vnDetectHumanBodyPoseRequest supportedJointNamesAndReturnErrorSelector (toNSError error_)

-- | Obtain the collection of human body joints group names that are supported by a given request revision.
--
-- @revision@ — The revision of VNDetectHumanBodyPoseRequest being queried.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPoseObservationJointsGroupName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @+ supportedJointsGroupNamesForRevision:error:@
supportedJointsGroupNamesForRevision_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesForRevision_error revision error_ =
  do
    cls' <- getRequiredClass "VNDetectHumanBodyPoseRequest"
    sendClassMessage cls' supportedJointsGroupNamesForRevision_errorSelector revision (toNSError error_)

-- | Obtain the collection of human body joints group names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPoseObservationJointsGroupName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectHumanBodyPoseRequest vnDetectHumanBodyPoseRequest, IsNSError error_) => vnDetectHumanBodyPoseRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectHumanBodyPoseRequest error_ =
  sendMessage vnDetectHumanBodyPoseRequest supportedJointsGroupNamesAndReturnErrorSelector (toNSError error_)

-- | VNHumanBodyPoseObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanBodyPoseRequest vnDetectHumanBodyPoseRequest => vnDetectHumanBodyPoseRequest -> IO (Id NSArray)
results vnDetectHumanBodyPoseRequest =
  sendMessage vnDetectHumanBodyPoseRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedJointNamesForRevision:error:@
supportedJointNamesForRevision_errorSelector :: Selector '[CULong, Id NSError] (Id NSArray)
supportedJointNamesForRevision_errorSelector = mkSelector "supportedJointNamesForRevision:error:"

-- | @Selector@ for @supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedJointNamesAndReturnErrorSelector = mkSelector "supportedJointNamesAndReturnError:"

-- | @Selector@ for @supportedJointsGroupNamesForRevision:error:@
supportedJointsGroupNamesForRevision_errorSelector :: Selector '[CULong, Id NSError] (Id NSArray)
supportedJointsGroupNamesForRevision_errorSelector = mkSelector "supportedJointsGroupNamesForRevision:error:"

-- | @Selector@ for @supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedJointsGroupNamesAndReturnErrorSelector = mkSelector "supportedJointsGroupNamesAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

