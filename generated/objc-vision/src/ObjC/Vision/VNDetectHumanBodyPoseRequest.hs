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
  , supportedJointNamesForRevision_errorSelector
  , supportedJointNamesAndReturnErrorSelector
  , supportedJointsGroupNamesForRevision_errorSelector
  , supportedJointsGroupNamesAndReturnErrorSelector
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
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "supportedJointNamesForRevision:error:") (retPtr retVoid) [argCULong (fromIntegral revision), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtain the collection of human body joint names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPoseObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnError :: (IsVNDetectHumanBodyPoseRequest vnDetectHumanBodyPoseRequest, IsNSError error_) => vnDetectHumanBodyPoseRequest -> error_ -> IO (Id NSArray)
supportedJointNamesAndReturnError vnDetectHumanBodyPoseRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectHumanBodyPoseRequest (mkSelector "supportedJointNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "supportedJointsGroupNamesForRevision:error:") (retPtr retVoid) [argCULong (fromIntegral revision), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtain the collection of human body joints group names that are supported by a given request object configured with a specific revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPoseObservationJointsGroupName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectHumanBodyPoseRequest vnDetectHumanBodyPoseRequest, IsNSError error_) => vnDetectHumanBodyPoseRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectHumanBodyPoseRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectHumanBodyPoseRequest (mkSelector "supportedJointsGroupNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | VNHumanBodyPoseObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanBodyPoseRequest vnDetectHumanBodyPoseRequest => vnDetectHumanBodyPoseRequest -> IO (Id NSArray)
results vnDetectHumanBodyPoseRequest  =
  sendMsg vnDetectHumanBodyPoseRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

