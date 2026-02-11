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
  , supportedJointNamesAndReturnErrorSelector
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

-- | Creates a new VNDetectHumanBodyPose3DRequest with no completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest => vnDetectHumanBodyPose3DRequest -> IO (Id VNDetectHumanBodyPose3DRequest)
init_ vnDetectHumanBodyPose3DRequest  =
  sendMsg vnDetectHumanBodyPose3DRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a new VNDetectHumanBodyPose3DRequest with completion handler.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest => vnDetectHumanBodyPose3DRequest -> Ptr () -> IO (Id VNDetectHumanBodyPose3DRequest)
initWithCompletionHandler vnDetectHumanBodyPose3DRequest  completionHandler =
  sendMsg vnDetectHumanBodyPose3DRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Obtain the collection of human body joint names that are supported by a given request revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBodyPose3DObservationJointName symbols that are supported by the request revision, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnError :: (IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest, IsNSError error_) => vnDetectHumanBodyPose3DRequest -> error_ -> IO (Id NSArray)
supportedJointNamesAndReturnError vnDetectHumanBodyPose3DRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectHumanBodyPose3DRequest (mkSelector "supportedJointNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtain the collection of human body joints group names that are supported by a request object configured with a request revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNHumanBody3DPoseObservationJointsGroupName symbols that are supported by the request, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest, IsNSError error_) => vnDetectHumanBodyPose3DRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectHumanBodyPose3DRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectHumanBodyPose3DRequest (mkSelector "supportedJointsGroupNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | VNHumanBodyPose3DObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectHumanBodyPose3DRequest vnDetectHumanBodyPose3DRequest => vnDetectHumanBodyPose3DRequest -> IO (Id NSArray)
results vnDetectHumanBodyPose3DRequest  =
  sendMsg vnDetectHumanBodyPose3DRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnErrorSelector :: Selector
supportedJointNamesAndReturnErrorSelector = mkSelector "supportedJointNamesAndReturnError:"

-- | @Selector@ for @supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnErrorSelector :: Selector
supportedJointsGroupNamesAndReturnErrorSelector = mkSelector "supportedJointsGroupNamesAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

