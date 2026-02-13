{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request that detects specific landmark points on human bodies in 3D space relative to the camera.        When possible,@AVDepthData@ depth information is used to produce more accurate results, but the request does not require it to run.
--
-- This request generates a collection of VNHumanBodyPose3DObservation objects which describe the position of each detected body
--
-- Generated bindings for @VNDetectHumanBodyPose3DRequest@.
module ObjC.Vision.VNDetectHumanBodyPose3DRequest
  ( VNDetectHumanBodyPose3DRequest
  , IsVNDetectHumanBodyPose3DRequest(..)
  , init_
  , initWithCompletionHandler
  , supportedJointNamesAndReturnError
  , supportedJointsGroupNamesAndReturnError
  , results
  , initSelector
  , initWithCompletionHandlerSelector
  , resultsSelector
  , supportedJointNamesAndReturnErrorSelector
  , supportedJointsGroupNamesAndReturnErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new VNDetectHumanBodyPose3DRequest with no completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest => vnDetectHumanBodyPose3DRequest -> IO (Id VNDetectHumanBodyPose3DRequest)
init_ vnDetectHumanBodyPose3DRequest =
  sendOwnedMessage vnDetectHumanBodyPose3DRequest initSelector

-- | Creates a new VNDetectHumanBodyPose3DRequest with completion handler.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest => vnDetectHumanBodyPose3DRequest -> Ptr () -> IO (Id VNDetectHumanBodyPose3DRequest)
initWithCompletionHandler vnDetectHumanBodyPose3DRequest completionHandler =
  sendOwnedMessage vnDetectHumanBodyPose3DRequest initWithCompletionHandlerSelector completionHandler

-- | Obtain the collection of human body joint names that are supported by a given request revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPose3DObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnError :: (IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest, IsNSError error_) => vnDetectHumanBodyPose3DRequest -> error_ -> IO (Id NSArray)
supportedJointNamesAndReturnError vnDetectHumanBodyPose3DRequest error_ =
  sendMessage vnDetectHumanBodyPose3DRequest supportedJointNamesAndReturnErrorSelector (toNSError error_)

-- | Obtain the collection of human body joints group names that are supported by a request object configured with a request revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBody3DPoseObservationJointsGroupName symbols that are supported by the request, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest, IsNSError error_) => vnDetectHumanBodyPose3DRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectHumanBodyPose3DRequest error_ =
  sendMessage vnDetectHumanBodyPose3DRequest supportedJointsGroupNamesAndReturnErrorSelector (toNSError error_)

-- | VNHumanBodyPose3DObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest => vnDetectHumanBodyPose3DRequest -> IO (Id NSArray)
results vnDetectHumanBodyPose3DRequest =
  sendMessage vnDetectHumanBodyPose3DRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNDetectHumanBodyPose3DRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNDetectHumanBodyPose3DRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedJointNamesAndReturnErrorSelector = mkSelector "supportedJointNamesAndReturnError:"

-- | @Selector@ for @supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedJointsGroupNamesAndReturnErrorSelector = mkSelector "supportedJointsGroupNamesAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

