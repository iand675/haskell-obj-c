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
  , supportedJointNamesForRevision_errorSelector
  , supportedJointNamesAndReturnErrorSelector
  , supportedJointsGroupNamesForRevision_errorSelector
  , supportedJointsGroupNamesAndReturnErrorSelector
  , maximumHandCountSelector
  , setMaximumHandCountSelector
  , resultsSelector


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
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "supportedJointNamesForRevision:error:") (retPtr retVoid) [argCULong (fromIntegral revision), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtain the collection of human hand joint names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanHandPoseObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnError :: (IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest, IsNSError error_) => vnDetectHumanHandPoseRequest -> error_ -> IO (Id NSArray)
supportedJointNamesAndReturnError vnDetectHumanHandPoseRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectHumanHandPoseRequest (mkSelector "supportedJointNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "supportedJointsGroupNamesForRevision:error:") (retPtr retVoid) [argCULong (fromIntegral revision), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtain the collection of human hand joints group names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanHandPoseObservationJointsGroupName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest, IsNSError error_) => vnDetectHumanHandPoseRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectHumanHandPoseRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectHumanHandPoseRequest (mkSelector "supportedJointsGroupNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Defines an upper bounds to the maximum number of hands that will be processed for key points in an image.
--
-- The complexity in key points determination is scalable by the number of hands to be processed.  All hands detected in an image will be ordered by relative size, with only the N largest ones having key points determined.  The default value for this property is 2. The maximum value for VNDetectHumanHandPoseRequestRevision1 is 6.
--
-- ObjC selector: @- maximumHandCount@
maximumHandCount :: IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest => vnDetectHumanHandPoseRequest -> IO CULong
maximumHandCount vnDetectHumanHandPoseRequest  =
  sendMsg vnDetectHumanHandPoseRequest (mkSelector "maximumHandCount") retCULong []

-- | Defines an upper bounds to the maximum number of hands that will be processed for key points in an image.
--
-- The complexity in key points determination is scalable by the number of hands to be processed.  All hands detected in an image will be ordered by relative size, with only the N largest ones having key points determined.  The default value for this property is 2. The maximum value for VNDetectHumanHandPoseRequestRevision1 is 6.
--
-- ObjC selector: @- setMaximumHandCount:@
setMaximumHandCount :: IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest => vnDetectHumanHandPoseRequest -> CULong -> IO ()
setMaximumHandCount vnDetectHumanHandPoseRequest  value =
  sendMsg vnDetectHumanHandPoseRequest (mkSelector "setMaximumHandCount:") retVoid [argCULong (fromIntegral value)]

-- | VNHumanHandPoseObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanHandPoseRequest vnDetectHumanHandPoseRequest => vnDetectHumanHandPoseRequest -> IO (Id NSArray)
results vnDetectHumanHandPoseRequest  =
  sendMsg vnDetectHumanHandPoseRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedJointNamesForRevision:error:@
supportedJointNamesForRevision_errorSelector :: Selector
supportedJointNamesForRevision_errorSelector = mkSelector "supportedJointNamesForRevision:error:"

-- | @Selector@ for @supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnErrorSelector :: Selector
supportedJointNamesAndReturnErrorSelector = mkSelector "supportedJointNamesAndReturnError:"

-- | @Selector@ for @supportedJointsGroupNamesForRevision:error:@
supportedJointsGroupNamesForRevision_errorSelector :: Selector
supportedJointsGroupNamesForRevision_errorSelector = mkSelector "supportedJointsGroupNamesForRevision:error:"

-- | @Selector@ for @supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnErrorSelector :: Selector
supportedJointsGroupNamesAndReturnErrorSelector = mkSelector "supportedJointsGroupNamesAndReturnError:"

-- | @Selector@ for @maximumHandCount@
maximumHandCountSelector :: Selector
maximumHandCountSelector = mkSelector "maximumHandCount"

-- | @Selector@ for @setMaximumHandCount:@
setMaximumHandCountSelector :: Selector
setMaximumHandCountSelector = mkSelector "setMaximumHandCount:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

