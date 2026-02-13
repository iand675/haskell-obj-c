{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Detects specific landmark points on human hands.
--
-- This request will produce a collection of VNRecognizedPointsObservation objects which describe the pose of each detected human hand.
--
-- Generated bindings for @VNDetectHumanHandPoseRequest@.
module ObjC.Vision.VNDetectHumanHandPoseRequest
  ( VNDetectHumanHandPoseRequest
  , IsVNDetectHumanHandPoseRequest(..)
  , supportedJointNamesForRevision_error
  , supportedJointNamesAndReturnError
  , supportedJointsGroupNamesForRevision_error
  , supportedJointsGroupNamesAndReturnError
  , maximumHandCount
  , setMaximumHandCount
  , results
  , maximumHandCountSelector
  , resultsSelector
  , setMaximumHandCountSelector
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

-- | Obtain the collection of human hand joint names that are supported by a given request revision.
--
-- @revision@ — The revision of VNDetectHumanHandPoseRequest being queried.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanHandPoseObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @+ supportedJointNamesForRevision:error:@
supportedJointNamesForRevision_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSArray)
supportedJointNamesForRevision_error revision error_ =
  do
    cls' <- getRequiredClass "VNDetectHumanHandPoseRequest"
    sendClassMessage cls' supportedJointNamesForRevision_errorSelector revision (toNSError error_)

-- | Obtain the collection of human hand joint names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanHandPoseObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnError :: (IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest, IsNSError error_) => vnDetectHumanHandPoseRequest -> error_ -> IO (Id NSArray)
supportedJointNamesAndReturnError vnDetectHumanHandPoseRequest error_ =
  sendMessage vnDetectHumanHandPoseRequest supportedJointNamesAndReturnErrorSelector (toNSError error_)

-- | Obtain the collection of human hand joints group names that are supported by a given request revision.
--
-- @revision@ — The revision of VNDetectHumanHandPoseRequest being queried.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanHandPoseObservationJointsGroupName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @+ supportedJointsGroupNamesForRevision:error:@
supportedJointsGroupNamesForRevision_error :: IsNSError error_ => CULong -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesForRevision_error revision error_ =
  do
    cls' <- getRequiredClass "VNDetectHumanHandPoseRequest"
    sendClassMessage cls' supportedJointsGroupNamesForRevision_errorSelector revision (toNSError error_)

-- | Obtain the collection of human hand joints group names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanHandPoseObservationJointsGroupName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest, IsNSError error_) => vnDetectHumanHandPoseRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectHumanHandPoseRequest error_ =
  sendMessage vnDetectHumanHandPoseRequest supportedJointsGroupNamesAndReturnErrorSelector (toNSError error_)

-- | Defines an upper bounds to the maximum number of hands that will be processed for key points in an image.
--
-- The complexity in key points determination is scalable by the number of hands to be processed.  All hands detected in an image will be ordered by relative size, with only the N largest ones having key points determined.  The default value for this property is 2. The maximum value for VNDetectHumanHandPoseRequestRevision1 is 6.
--
-- ObjC selector: @- maximumHandCount@
maximumHandCount :: IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest => vnDetectHumanHandPoseRequest -> IO CULong
maximumHandCount vnDetectHumanHandPoseRequest =
  sendMessage vnDetectHumanHandPoseRequest maximumHandCountSelector

-- | Defines an upper bounds to the maximum number of hands that will be processed for key points in an image.
--
-- The complexity in key points determination is scalable by the number of hands to be processed.  All hands detected in an image will be ordered by relative size, with only the N largest ones having key points determined.  The default value for this property is 2. The maximum value for VNDetectHumanHandPoseRequestRevision1 is 6.
--
-- ObjC selector: @- setMaximumHandCount:@
setMaximumHandCount :: IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest => vnDetectHumanHandPoseRequest -> CULong -> IO ()
setMaximumHandCount vnDetectHumanHandPoseRequest value =
  sendMessage vnDetectHumanHandPoseRequest setMaximumHandCountSelector value

-- | VNHumanHandPoseObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest => vnDetectHumanHandPoseRequest -> IO (Id NSArray)
results vnDetectHumanHandPoseRequest =
  sendMessage vnDetectHumanHandPoseRequest resultsSelector

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

-- | @Selector@ for @maximumHandCount@
maximumHandCountSelector :: Selector '[] CULong
maximumHandCountSelector = mkSelector "maximumHandCount"

-- | @Selector@ for @setMaximumHandCount:@
setMaximumHandCountSelector :: Selector '[CULong] ()
setMaximumHandCountSelector = mkSelector "setMaximumHandCount:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

